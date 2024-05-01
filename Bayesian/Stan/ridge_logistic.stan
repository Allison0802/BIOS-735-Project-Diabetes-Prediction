// This is a logistic regression model using a normal prior on the regression coefficients
// and a half-cauchy on the ridge parameter
data {
  int<lower=0>                            n;                    // number of observations
  int<lower=0>                            p;                    // total number of covariates (incl. intercept)
  array[n] int<lower=0, upper=1>          y;                    // response vector (binary responses)
  matrix[n,p]                             X;                    // design matrix
}
transformed data {
  matrix[n,p] X_std;
  vector[p] sd_X;
  
  sd_X[1] = 1;                                    // Set intercept sd to 1
  X_std[,1] = X[,1];                              // Keep intercept as is
  
  for (j in 2:p) {
    sd_X[j] = sd(X[, j]);
    X_std[, j] = X[, j] / sd_X[j];
  }
}
parameters {
  vector[p] beta_std;                                 // Coefficients
  real<lower=0,upper=pi()/2> ridge_scale_uniform;     // Shrinkage parameter
}
transformed parameters {
  real ridge_scale = tan(ridge_scale_uniform);
}
model {
  // prior
  beta_std[1] ~ normal(0, 10);                      // noninformative prior on intercept
  target += normal_lpdf(beta_std[2:p] | 0, ridge_scale); // normal prior on all covariates
  ridge_scale ~ cauchy(0,1);
  
  // likelihood
  y ~ bernoulli_logit_glm(X_std, 0, beta_std);
}
generated quantities {
  vector[p] beta = inv(sd_X) .* beta_std;
}
