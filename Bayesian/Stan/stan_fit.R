# Source bayesian ridge wrapper functions
source("Bayesian/Stan/stan_wrappers.R")

# Read in data
train <- diabetes_train
train$smoke <- as.factor(train$smoke)
train$drink <- as.factor(train$drink)
train$history <- as.factor(train$history)
train$Gender <- as.factor(train$Gender)

# Downsample data
set.seed(458)
diabetes <- sample_n(train, 30000)

# Fit model
fmla <- formula(diabetes ~ . - site - yr_f - FPG - FPG_final - weight - HDL - LDL)
fit_bridge <- bridge.mcmc(fmla, data = diabetes, 
                          chains = 1, parallel_chains = 1, 
                          iter_warmup = 2000, iter_sampling = 10000)

# Obtain summary
summary_stan <- summarize_draws(fit_bridge, "mean", "median", "Mode", "sd", "mad",
                                  ~quantile(.x, probs = c(0.025, 0.975), na.rm = TRUE),      
                                  "rhat", "ess_bulk", "ess_tail")
summary_stan
# View(summary_bridge)

# Diagnostics
# mcmc_trace(fit_bridge)
# mcmc_acf(fit_bridge)
# mcmc_hist(fit_bridge)
