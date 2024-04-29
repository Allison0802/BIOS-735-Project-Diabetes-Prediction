library(testthat)


test_that("all arguments are provided", {

  expect_error(penalized.logit(c(1:10), c(1:100)))
  expect_error(cv.penalized.logit(c(1:10)))
  expect_error(bayes.ridge.mcmc(as.data.frame(matrix(1:20, nrow = 2))))

})
