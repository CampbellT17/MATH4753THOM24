library(testthat)
library(MATH4753THOM24)

test_that("Test mu component of myncurve output", {
  result <- myncurve(0, 1, 1)
  expect_equal(result$mu, 0)
})

test_that("Test sigma component of myncurve output", {
  result <- myncurve(0, 1, 1)
  expect_equal(result$sigma, 1)
})

test_that("Test probability component of myncurve output", {
  result <- myncurve(0, 1, 1)
  expected_prob <- pnorm(1, mean = 0, sd = 1)
  expect_equal(result$Prob, expected_prob)
})
