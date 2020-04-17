context("smsPOMDP")

test_that("observation matrix", {
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
  
  #buiding the matrices of the problem
  o <- smsPOMDP::obs(p0, pm, d0, dm, ds, V, Cm, Cs)#observation matrix
  
  #checking the dimensions of the matrices
  expect_equal(dim(o)[1],2)#2 states
  expect_equal(dim(o)[2],2)#2 observations
  expect_equal(dim(o)[3],3)#3 actions
  
  #is the matrix stochastic
  expect_equal(o[1,1,1]+o[1,2,1],1)
  expect_equal(o[2,1,1]+o[2,2,1],1)
  expect_equal(o[1,1,2]+o[1,2,2],1)
  expect_equal(o[2,1,2]+o[2,2,2],1)
  expect_equal(o[1,1,3]+o[1,2,3],1)
  expect_equal(o[2,1,3]+o[2,2,3],1)
  
})