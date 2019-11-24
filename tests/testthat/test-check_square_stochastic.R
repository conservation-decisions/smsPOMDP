context("smsPOMDP")

test_that("check square stochastic matrix", {
  #square and stochastic
  m <- matrix(c(1,0,0,1), ncol = 2)
  testthat::expect_true(smsPOMDP::check_square_stochastic(m))
  
  set.seed(1)
  a <- runif(2)
  m1 <- matrix(c(a[1], 1-a[1], a[2], 1-a[2]), ncol = 2, byrow = T)
  testthat::expect_true(smsPOMDP::check_square_stochastic(m1))
  
  #square but not stochastic
  m2 <- matrix(c(1,1,1,1), ncol = 2)
  testthat::expect_false(smsPOMDP::check_square_stochastic(m2))
  
  #stochastic but not square
  m3 <- matrix(c(1,0,0,
                 0,0,1), byrow = T, nrow = 2)
  testthat::expect_false(smsPOMDP::check_square_stochastic(m3))
  
  #not square and not stochastic
  m4 <- matrix(c(1,2,3,
                 5,4,1), byrow = T, nrow = 2)
  testthat::expect_false(smsPOMDP::check_square_stochastic(m4))
  
})