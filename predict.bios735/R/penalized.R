#' Penalized Logistic Regression
#'
#' This function performs penalized logistic regression using iterative updates
#'
#' @import stats
#'
#' @param X Matrix of predictor variables.
#' @param y Vector of response variable.
#' @param beta Initial values of the coefficients.
#' @param lambda Vector of penalty parameters.
#' @param iter Maximum number of iterations (default: 1000).
#' @param tol Tolerance for convergence (default: 0.0001).
#'
#' @return A matrix containing the estimated coefficients for each lambda value.
#'
#' @details The function iteratively updates the coefficients using a penalized
#' logistic regression algorithm until convergence or maximum iterations are reached.
#'
#' @examples
#' data("diabetes_train")
#' predictors <- scale(model.matrix(diabetes ~ Age + Gender + BMI + SBP + DBP + height +
#'                         Triglyceride + ALT + AST + BUN + CCR + Cholesterol +
#'                         factor(smoke) + factor(drink) + factor(history), data = diabetes_train)[,-1]
#' response <- diabetes_train$diabetes
#'
#' # We get the best lambda
#' lambda_seq <- seq(0.013, 0.0008, by = -0.0001)
#' cv_lambda <- cv.penalized.logit(predictors, response, n_folds = 5, lambda = lambda_seq)
#' cv_lambda$se.1.lambda
#'
#' # Penalized logistic regression solution
#' beta0 = glm(y~X, family = "binomial")$coefficients
#' penalized_logit_result <- penalized.logit(predictors, response, beta = beta0, lambda = cv_lambda$se.1.lambda)
#' penalized_logit_result
#'
#' @export
penalized.logit = function(X, y, beta, lambda, iter = 1e3, tol = 1e-4){
  ## Defining function within the closure
  mult.elmtwise = function(mat,vec){
    if(dim(mat)[2]!=length(vec))  stop("Dimensions do not match")
    mat.new = mat * 0
    for(i in 1:ncol(mat)){
      mat.new[,i] = mat[,i]*vec[i]
    }
    return(mat.new)
  }
  beta_new = function(X, y, beta_old, lambda){
    beta = as.matrix(beta_old)
    X = cbind(1,X)
    # Calculate the probability
    pi = exp(X %*% beta) / (1 + exp(X %*% beta))
    # Calculate weights
    w = pi * (1 - pi)
    # Compute the element-wise multiplication of the transpose of X and the weights
    t_x_delta = mult.elmtwise(mat = t(X), vec = w)
    # Calculate z
    z = X %*% beta + (1 / w) * (y - pi)
    # Define penalization matrix with lambda
    penalization = (dim(X)[1]*lambda)* diag(ncol(X))
    penalization[1,1] = 0 ## intercept is not penalized
    # Update beta using the formula for Ridge regression
    beta_t = solve((t_x_delta %*% X) + penalization) %*% (t_x_delta %*% z)
    return(beta_t)
  }

  # Input validation
  if (missing(X) || missing(y) || missing(beta) || any(lengths(list(X, y, beta)) == 0)) {
    stop("Arguments X, y, and beta cannot be empty")
  }

  ##initialize values
  out = matrix(0, ncol = length(lambda), nrow = length(beta))
  rownames(out) = names(beta)
  for(i in 1:length(lambda)){
    beta_t0 = as.matrix(beta)
    k = 0; diff = 1e3
    while(k < iter & diff > tol){
      beta_t = beta_new(X=X, y=y,beta_t0, lambda = lambda[i])
      diff = norm(beta_t-beta_t0)
      beta_t0 = beta_t
      k = k + 1
    }
    out[,i] = beta_t
  }
  return(out)
}
