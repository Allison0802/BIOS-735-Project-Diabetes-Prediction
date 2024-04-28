#' Cross-validated penalized logistic regression
#'
#' This function performs cross-validated penalized logistic regression using glmnet for.
#' minimizing the error rate
#' Taken and adapted from: www.r-bloggers.com/2021/10/lambda-min-lambda-1se-and-cross-validation-in-lasso-binomial-response/
#'
#' @param X Matrix of predictors.
#' @param y Vector of response variables.
#' @param n_folds Number of folds for cross-validation.
#' @param lambda Vector of lambda values for penalization. If NULL, lambda values are explored automatically.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{lambda.min}}{The lambda value corresponding to the minimum misclassification rate (mcr).}
#'   \item{\code{se.mcr}}{Standard error of the misclassification rate (mcr).}
#'   \item{\code{se.lambda.min}}{Standard error of the minimum mcr lambda value.}
#'   \item{\code{se.1.lambda}}{Lambda value with mcr within 1 standard error of the minimum mcr.}
#'   \item{\code{cv.out}}{Data frame containing lambda, log_lambda, and mcr values for each lambda.}
#' }
#'
#' @examples
#' X <- matrix(rnorm(100), ncol = 5)
#' y <- sample(0:1, 20, replace = TRUE)
#' cv_result <- cv.penalized.logit(X, y)
#'
#' @import glmnet
#' @importFrom caret createFolds confusionMatrix
#'
#' @export
cv.penalized.logit = function(X, y, n_folds = 5, lambda=NULL){
  
  out = list()
  ### create the Expit function for prediction
  expit = function(x){exp(x)/(1+exp(x))}
  
  ### create the folds
  folds = caret::createFolds(y, k = n_folds)
    
  ### Explore the possible values of lambda
  if(is.null(lambda)){
    fit = glmnet(x = scale(X), y = y, family = binomial(link="logit"), alpha = 0)
    lambda_ = fit$lambda
    lambda_ = lambda_[lambda_< 0.0099]
  }else{
    lambda_ = lambda
  }
  n_lambda = length(lambda_)
  
  ### create the vector for the loop
  m.mce = matrix(0,nrow = n_folds, ncol=n_lambda)
  m.tot = matrix(0,nrow = n_folds, ncol=n_lambda)
  m.mcr = matrix(0,nrow = n_folds, ncol=n_lambda)  
  
    #dev.ratio = numeric(length = n_folds)
  for(i in 1:n_folds){
      ### Splitting the data in training and test 
    x_train = X[unlist(folds[-i]),]
    y_train = y[unlist(folds[-i])] 
    x_test = X[unlist(folds[i]),]
    y_test = y[unlist(folds[i])]
    
    ## initial value of beta
    beta_o = lm(y_train ~ x_train)$coefficients
    
    ## fitting the penalized logistic regression 
    bt = penalized.logit(X = x_train, y = y_train, lambda = lambda_, beta = beta_o)
    
    ## prediction on validation fold, 
    for(k in 1:n_lambda){
      pred = ifelse(expit(cbind(1,x_test)%*%bt[,k])>1/2,1,0)
    
      ## confusion matrix
      cfm = caret::confusionMatrix(as.factor(pred),as.factor(y_test))
      
      # misclassification count
      m.mce[i,k] = cfm$table[1,2]+cfm$table[2,1]
      
      # total count
      m.tot[i,k] = sum(cfm$table)
      
      # misclassification rate
      m.mcr[i,k] =  m.mce[i,k]/m.tot[i,k]
    }
  }
  # average misclassification error rate (mcr)
  v.mcr = colMeans(m.mcr)
  
  # save manual cross validation output
  cv.out = data.frame(lambda = lambda_, 
                      log_lambda = log(lambda_), 
                      mcr = v.mcr)
  
  #——————————-
  no_lambda_min = which.min(cv.out$mcr)
  out$lambda.min = cv.out$lambda[no_lambda_min]
  
  # standard error of mcr
  out$se.mcr = apply((m.mce)/m.tot,2,sd)/sqrt(n_folds)
  
  # se of min lambda
  out$se.lambda.min = out$se.mcr[no_lambda_min]
  
  # 1.se lambda
  out$se.1.lambda = max(cv.out$lambda[cv.out$mcr < min(cv.out$mcr) + out$se.lambda.min])
  
  out$cv.out = cv.out
  
  return(out)
}





