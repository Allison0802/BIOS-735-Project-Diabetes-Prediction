#' Penalized Logistic Regression
#'
#' This function performs penalized logistic regression using iterative updates
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
#' beta_true <- c(1.54, 3, 2.3)
#' X <- matrix(rnorm(100), ncol = 2)
#' linear_predictor <- cbind(1, X) %*% beta_true
#' p <- exp(linear_predictor) / (1 + exp(linear_predictor))
#' y <- rbinom(50, 1, prob = p)
#' beta_initial <- rep(1, 3)
#' lambda_values <- c(0.1, 0.01, 0.001)
#' 
#' # Run the penalized logistic regression function
#' penalized.logit(X = X, y = y, beta = beta_initial, lambda = lambda_values)
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
    penalization = (lambda) * diag(ncol(X)) 
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