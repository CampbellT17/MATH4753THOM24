library(testthat)
library(MATH4753THOM24)

test_that("Test optimal tickets sold to fill but not overbook a flight via ntickets function", {
  result <- ntickets(400, 0.02, 0.95)
  expect_equal(result$nd, result$nc)
})

