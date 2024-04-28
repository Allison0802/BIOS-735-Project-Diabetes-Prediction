#' Logarithm of Normal Prior
#' 
#' This function computes the log of a normal density with mean 0 and
#' standard deviation lambda. It uses a noninformative prior on the first
#' parameter, assumed to be the intercept.
#' 
#' @param beta a vector of numbers
#' @param lambda the prior sd
#'
#' @return the sum of the logarithm of the density at each beta value
log_pbeta <- function(beta, lambda) {
  log(dnorm(beta[1], mean = 0, sd = 10)) + sum(log(dnorm(beta[2:length(beta)], mean = 0, sd = max(lambda, 0))))
}

#' Logarithm of Half-Cauchy Prior
#' 
#' This function computes the logarithm of a standard half-Cauchy density.
#' 
#' @param lambda a number
#'
#' @return the logarithm of a standard half-Cauchy density at lambda
log_plambda <- function(lambda) {
  density <- 2/(pi*(1 + lambda^2)) * ifelse(lambda > 0, 1, 0)
  log(density)
}

#' Log-Likelihood for Logistic Regression
#' 
#' This function computes the log-likelihood at a given beta vector
#' for a given response vector y and design matrix x.
#' 
#' @param y the vector of responses
#' @param x the design matrix, including the intercept
#' @param beta specified regression coefficients
#'
#' @return the logarithm of a standard half-Cauchy density at lambda
log_likelihood <- function(y, x, beta) {
  y %*% x %*% beta - sum(log(1 + exp(x %*% beta)))
}

#' Unnormalized Log-Posterior
#'
#' This function computes the unnormalized log-posterior for logistic regression
#' assuming a normal prior on the betas and a standard half-Cauchy prior on lambda,
#' the standard deviation of the betas
#' 
#' @param y the vector of responses
#' @param x the design matrix, including the intercept
#' @param beta specified regression coefficients
#' @param lambda the standard deviation for the normal beta prior
#'
#' @return the logarithm of the log-posterior for logistic regression
log_posterior_unnormalized <- function(y, x, beta, lambda) {
  log_likelihood(y, x, beta) + log_pbeta(beta, lambda) + log_plambda(lambda)
}

#' Covariance Estimate for Adaptive Metropolis-Hastings
#'
#' This function computes the estimated covariance matrix at a given iteration of
#' the Metropolis-Hastings algorithm. A small diagonal matrix is added to ensure
#' positive-definiteness
#' 
#' @param chain_mat the complete MCMC
#' @param state the index of the row of the chain corresponding to the current iteration
#' @param start the initial index of the chain matrix to compute the covariance 
#'
#' @return the logarithm of a standard half-Cauchy density at lambda
cov_state <- function(chain_mat, state, start) {
  chain_mat <- chain_mat[start:state,]
  cov_estimate <- cov(chain_mat)
  dim <- ncol(chain_mat)
  epsilon <- 0.0001
  scale_cov <- 2.38^2/dim
  scale_ident <- 0.1^2/dim
  (1-epsilon)*scale_cov*cov_estimate + epsilon*scale_ident*diag(dim)
}

#' Proposal Function for Metropolis Hastings Random Walk
#'
#' This function generates a beta based on the estimated covariance matrix of the current chain. 
#' It also generates a lambda proposal by eith including it in the covariance 
#' matrix or generating it independently. Finally, the function returns the 
#' concatenated c(beta, lambda) vector.
#' 
#' @param chain_mat the complete MCMC
#' @param state the index of the row of the chain corresponding to the current iteration
#' @param start the initial index of the chain matrix to compute the covariance 
#' @param lambda_adapt logical, include lambda in the adaptive proposal (TRUE) or generate independently (FALSE)
#'
#' @return a random draw from a multivariate normal density with covariance estimated from chain
proposal <- function(chain_mat, state, start, lambda_adapt = TRUE) {
  if (lambda_adapt == TRUE) {
    beta_lambda_current <- chain_mat[state, ]
    cov_prop <- cov_state(chain_mat, state, start)
    prop <- MASS::mvrnorm(1, beta_lambda_current, Sigma = cov_prop)
  } else{
    beta_current <- chain_mat[state, 1:(ncol(chain_mat) - 1)]
    cov_prop <- cov_state(chain_mat[, 1:(ncol(chain_mat) - 1)], state, start)
    
    beta_prop <- MASS::mvrnorm(1, beta_current, Sigma = cov_prop)
    lambda_prop <- MASS::mvrnorm(1, chain_mat[state, ncol(chain_mat)], Sigma = 1e-2)
    prop <- c(beta_prop, lambda_prop)
  }
  prop
}

#' Metropolis-Hastings Ratio
#'
#' This function computes the Metropolis-Hastings ratio
#' 
#' @param y the vector of responses
#' @param x the design matrix, including the intercept
#' @param log_posterior_curr the unnormalized current log posterior
#' @param log_posterior_prop the unnormalized proposal log posterior
#'
#' @return the minimum of 1 and the ratio of unnormalized posteriors
R <- function(y, x, log_posterior_curr, log_posterior_prop) {
  log_ratio <- log_posterior_prop - log_posterior_curr
  r <- min(exp(log_ratio), 1)
  r
}

#' Metropolis-Hastings Sampler
#'
#' This function generates an MCMC chain using adaptive Metropolis-Hastings
#' algorithm for a logistic regression problem, assuming i.i.d N(0, lambda^2) priors
#' on the regression coefficients conditional on lambda, and a standard half-Cauchy
#' prior on lambda.
#' 
#' @param formula the formula to be used in the logistic regression model
#' @param data the complete data including response and covariates
#' @param chain_length the number of posterior draws to sample
#' @param lambda_adapt logical, include lambda in the adaptive proposal (TRUE) or generate independently (FALSE)
#' @param seed the seed to begin the sampler, for reproducability
#' @param beta_init the starting values for the regression coefficients
#' @param lambda_init the starting value for the shrinkage parameter lambda
#' @param trace logical, if TRUE show details while running sampler
#'
#' @return A list containing the following:
#' \item{chain}{The MCMC chain with the unstandardized coefficients}
#' \item{chain_std}{The MCMC with the standardized coefficients}
#' \item{acceptance_rate}{The acceptance rate for the sampler}
#' \item{design}{The design matrix}
#' \item{design_std}{The standardized design matrix used for the sampler}
#' 
#' @export
bayes.ridge.mcmc <- function(formula, data, chain_length = 60000, lambda_adapt = TRUE, 
                             seed = NULL, beta_init = NULL, lambda_init = NULL,
                             trace = FALSE) {
  ## Get time for chain
  start.time <- Sys.time()
  
  ## Design matrix
  x <- model.matrix(formula, data)
  
  ## Save dimension of parameter vector
  dim <- ncol(x)
  
  ## Standardized design matrix: normalize columns to have mean 0 and variance 1; leave intercept as is
  ## This gives intercept interpretation as probability of response given mean covariates
  x_std <- x
  sd_x <- rep(1, dim)

  for (j in 2:dim) {
    sd_x[j] <- sd(x[,j])
    x_std[,j] <- (x_std[,j] - mean(x_std[,j])) / sd_x[j]
  }
  
  ## Obtain response vector
  formula.lhs <- as.character(formula[[2]])
  y <- data[[formula.lhs[1]]]
  
  ## Initialize beta at MLEs
  if (is.null(beta_init)) {
    beta <- suppressWarnings(summary(glm(formula, data, family = "binomial"))$coefficients[,1])
    beta[1] <- log(mean(y)/(1-mean(y))) # Initialize y at overall log-odds
  } else{
    beta <- beta_init
  }
  
  ## Choose initial value for lambda
  if (is.null(lambda_init)) {
    lambda <- 1
  } else {
    lambda <- lambda_init
  }
  
  ## Initialize the chain matrix
  chain <- matrix(0, chain_length, length(beta) + 1)
  chain[1,] <- c(beta, lambda)
  colnames(chain) = c(colnames(x), "lambda")
  
  ## Count the number of acceptances
  n.acc <- 0
  
  ## Count number of negative lambda proposals
  n.lambda.neg <- 0
  
  ## Get unnormalized posterior for initial parameters
  log_posterior_curr <- log_posterior_unnormalized(y, x_std, beta, lambda)
  
  ## Start chain
  if (!is.null(seed)) {
    set.seed(seed)
  }
  for(i in 1:(chain_length-1)) {
    
    ## Set the value at current iteration of the chain to variable xt
    curr <- chain[i,]
    
    ## Use independent multivariate normal for first 100 iterations, then switch to adaptive
    if (i < 100) {
      prop <- MASS::mvrnorm(1, curr, Sigma = diag(c(rep(1e-3, dim), 1e-2)))
    } else {
      prop <- proposal(chain, i, max(90, i/2), lambda_adapt)
    }
    
    ## Update n.lambda.neg
    if (prop[length(prop)] < 0) {
      n.lambda.neg <- n.lambda.neg + 1
    }
    
    ## Calculate unnormalized log posterior of proposal
    log_posterior_prop <- log_posterior_unnormalized(y, x_std, prop[1:dim], prop[dim + 1])
    
    ## Calculate MH ratio 
    r <- R(y, x_std, log_posterior_curr, log_posterior_prop)
    
    if (is.na(log_posterior_prop)) {
      print(unname(prop[1:dim]))
      print(unname(prop[dim + 1]))
      print(log_posterior_curr)
      print(r)
    }
    
    ## Generate draw from bernoulli(p).
    keep <- rbinom(1, 1, r)
    
    ## If keep = 1, then set next iteration equal to then proposal, update n.acc, 
    ## and set current posterior to proposal posterior
    if (keep == 1){
      chain[i+1,] <- prop
      n.acc <- n.acc + 1
      log_posterior_curr <- log_posterior_prop
    }else{
      ## Otherwise, carry over value from the current iteration
      chain[i+1,] <- curr
    }
    if (trace == TRUE) {
      if (i%%500==0) {
        cat(paste0("Sample: ", i, "/", chain_length, "    Acceptance rate: ", round(n.acc/i, 4), "\n"))
      }
    }
  }
  
  ## Get unscaled parameter estimates
  chain_unscaled <- chain
  for (j in 2:dim) {
    chain_unscaled[,j] <- chain_unscaled[,j] / sd_x[j]
  }
  
  ## Calculate acceptance rate
  acceptance_rate <- n.acc/chain_length
  acceptance_rate
  
  ## Print final message
  end.time <- Sys.time()
  cat(paste0("Sample: ", chain_length, "/", chain_length, "    Acceptance rate: ", round(n.acc/i, 4), "\n",
             "Chain completed in ", round(difftime(time1 = end.time, time2 = start.time, units = "secs"), 2)
             , " seconds"))
  
  ## Return objects
  list("chain" = as_draws_df(as.data.frame(chain_unscaled)), 
       "chain_std" = as_draws_df(as.data.frame(chain)), 
       "acceptance_rate" = acceptance_rate,
       "design" = x,
       "design_std" = x_std,
       "n_lambda_neg" = n.lambda.neg)
}

#' Mode of a distribution
#'
#' This function computes the mode of a set of numbers by approximating the density
#' 
#' @param x the set of values with which to compute the mode
#'
#' @return the estimated mode
Mode <- function(x, adjust = 0.5) {
  density(x)$x[which.max(density(x, adjust = adjust)$y)]
}