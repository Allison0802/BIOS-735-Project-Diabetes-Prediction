library(testthat)

test_that("chain length is correct", {
  set.seed(123)  # for reproducibility
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * (p-1)), nrow = n, ncol = (p-1))
  X <- cbind(rep(1, n), x)
  beta <- c(0, 0.5, -0.5, 0, 1)
  probs <- exp(X %*% beta) / (1 + exp(X %*% beta))
  y <- rbinom(n = n, size = 1, prob = probs)
  df <- data.frame(y, x)
  chain.mcmc <- bayes.ridge.mcmc(y ~ ., data = df, chain_length = 5000, trace = F)

  expect_equal(nrow(chain.mcmc$chain), 5000)
})

test_that("returned design matrix is correct", {
  set.seed(123)  # for reproducibility
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * (p-1)), nrow = n, ncol = (p-1))
  X <- cbind(rep(1, n), x)
  beta <- c(0, 0.5, -0.5, 0, 1)
  probs <- exp(X %*% beta) / (1 + exp(X %*% beta))
  y <- rbinom(n = n, size = 1, prob = probs)
  df <- data.frame(y, x)
  chain.mcmc <- bayes.ridge.mcmc(y ~ ., data = df, chain_length = 5000, trace = F)

  expect_equal(matrix(chain.mcmc$design, ncol = p), matrix(X, ncol = p))
})

test_that("returned design_std matrix is correct", {
  set.seed(123)  # for reproducibility
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * (p-1)), nrow = n, ncol = (p-1))
  X <- cbind(rep(1, n), x)
  beta <- c(0, 0.5, -0.5, 0, 1)
  probs <- exp(X %*% beta) / (1 + exp(X %*% beta))
  y <- rbinom(n = n, size = 1, prob = probs)
  df <- data.frame(y, x)
  chain.mcmc <- bayes.ridge.mcmc(y ~ ., data = df, chain_length = 5000, trace = F)

  X_std <- cbind(X[,1], scale(X[,2:p]))

  expect_equal(matrix(chain.mcmc$design_std, ncol = p), matrix(X_std, ncol = p))
})

test_that("invalid formula identified", {
  set.seed(123)  # for reproducibility
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * (p-1)), nrow = n, ncol = (p-1))
  X <- cbind(rep(1, n), x)
  beta <- c(0, 0.5, -0.5, 0, 1)
  probs <- exp(X %*% beta) / (1 + exp(X %*% beta))
  y <- rbinom(n = n, size = 1, prob = probs)
  df <- data.frame(y, x)

  # there is no X5
  expect_error(bayes.ridge.mcmc(y ~ X1 + X2 + X3 + X4 + X5, data = df, chain_length = 5000))
})

test_that("invalid data argument identified", {
  set.seed(123)  # for reproducibility
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * (p-1)), nrow = n, ncol = (p-1))
  X <- cbind(rep(1, n), x)
  beta <- c(0, 0.5, -0.5, 0, 1)
  probs <- exp(X %*% beta) / (1 + exp(X %*% beta))
  y <- rbinom(n = n, size = 1, prob = probs)
  df <- data.frame(y, x)

  # there is no X5
  expect_error(bayes.ridge.mcmc(y ~ ., data = as.matrix(df), chain_length = 5000))
})


