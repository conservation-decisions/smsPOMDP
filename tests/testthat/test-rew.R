context("smsPOMDP")

test_that("reward matrix", {
  #values for Sumatran tigers
  pen <- 0.1
  p0 <- 1-pen
  pem <- 0.05816
  pm <- 1 - pem
  V <- 175.133
  Cm <- 18.784
  Cs <- 10.840
  d0 <- 0.01
  dm <- 0.01
  ds <- 0.78193
  
  r <- smsPOMDP::rew(p0, pm, d0, dm, ds, V, Cm, Cs)#reward matrix
  
  #checking the dimensions of the matrices
  expect_equal(dim(r)[1],2)#2 states
  expect_equal(dim(r)[2],3)#3 actions
  
})