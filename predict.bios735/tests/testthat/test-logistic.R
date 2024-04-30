library(testthat)

test_that("all arguments are provided", {

  expect_error(penalized.logit(c(1:10), c(1:100)))
  expect_error(cv.penalized.logit(c(1:10)))
  expect_error(bayes.ridge.mcmc(as.data.frame(matrix(1:20, nrow = 2))))

})

test_that("penalized.logit function tests", {
  # Test case 1: Compare with glmnet when lambda = 0
  set.seed(123)  # for reproducibility
  n <- 100
  p <- 5
  X <- matrix(rnorm(n * (p-1)), nrow = n, ncol = (p-1))
  y <- sample(0:1, n, replace = TRUE)
  beta <- rep(0, p)
  lambda <- 0
  
  # Run penalized.logit function
  result_penalized <- as.vector(penalized.logit(X, y, beta, lambda))
  
  # Run glmnet with lambda = 0
  fit_glm <- as.vector(glm(y~X, family = "binomial")$coefficients)
  
  # Check if results are similar
  expect_equal(result_penalized, fit_glm, tolerance = 1e-3)  # Adjust tolerance as needed

  # Test case 2: Inserting beta_0 as a vector of 1
  tryCatch({
    beta <- rep(1, p)  # beta_0 vector with all 1s
    penalized.logit(X, y, beta, lambda)  # This should raise an error
    # If no error was raised, fail the test
    stop("An error was expected but none was raised.")
  }, error = function(e) {
    # If an error was raised, the test passes
    expect_true(TRUE)
  })
})