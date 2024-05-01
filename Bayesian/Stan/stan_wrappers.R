library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

# Import model from stan file
bridge <- cmdstan_model("Bayesian/Stan/ridge_logistic.stan")

#' Get stan data for Bayesian Ridge
#' 
#' Obtains stan data for the Bayesian Ridge shrinkage method based on user input. This
#' is an internal function.
#' 
#' @param formula two-sided formula explaining how the outcome relates to treatment, covariates, and subgroups.
#' @param data data set
#' 
#' @return a list giving data to be passed onto `rstan` or `cmdstanr` models.
get.standata.bridge <- function(formula, data) {
  ##' Create design matrix
  X <- model.matrix(formula, data)
  
  ##' Obtain response vector
  formula.lhs <- as.character( formula[[2]] )
  y <- data[[formula.lhs[1]]]
  
  data_list <- list(
    'n'               = nrow(X)
    , 'p'             = ncol(X)
    , 'y'             = y
    , 'X'             = X
  )
  
  return(data_list)
}

#' Sample from posterior distribution of Bayesian Ridge model
#' 
#' This is a wrapper function to obtain posterior samples for Bayesian Ridge
#' shrinkage.
#' 
#' @param formula two-sided formula explaining how the outcome relates to treatment, covariates, and subgroups.
#' @param data data set
#' @param ... other arguments to pass onto `cmdstanr::sample`
#' 
#' @return an object of class `draws_df` giving posterior draws. See the `posterior` package for more details.
#' @examples 
#' 
bridge.mcmc <- function(formula, data, ...) {
  # Get standata to pass to model
  standata <- get.standata.bridge(formula, data)
  
  # Obtain posterior draws
  fit_bridge <- bridge$sample(
    data = standata, 
    # seed = 123, 
    # chains = 1, 
    # parallel_chains = 1, 
    # iter_warmup = 2000, 
    # iter_sampling = 10000,
    ...
  )
  
  # Reformat into dataframe
  fit_bridge <- fit_bridge$draws(format = 'draws_df')
  
  # Rename regression coefficients
  index <- grep('^beta\\[', colnames(fit_bridge))
  colnames(fit_bridge)[index] <- colnames(standata$X)
  
  return(fit_bridge)
}

# Function to define the mode
Mode <- function(x) {
  density(x)$x[which.max(density(x)$y)]
}
