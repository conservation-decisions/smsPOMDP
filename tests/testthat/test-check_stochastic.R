context("smsPOMDP")

test_that("check stochastic matrix", {
  m <- matrix(c(1,0,0,1), ncol = 2)
  testthat::expect_true(smsPOMDP::check_stochastic(m))
  
  set.seed(1)
  a <- runif(2)
  m1 <- matrix(c(a[1], 1-a[1], a[2], 1-a[2]), ncol = 2, byrow = T)
  testthat::expect_true(smsPOMDP::check_stochastic(m1))
  
  m2 <- matrix(c(1,1,1,1), ncol = 2)
  testthat::expect_false(smsPOMDP::check_stochastic(m2))
  
  m2 <- matrix(c(-1,2,2,-1), ncol = 2)
  testthat::expect_false(smsPOMDP::check_stochastic(m2))
})