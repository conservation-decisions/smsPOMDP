context("smsPOMDP")

test_that("transition matrix between states", {
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
  
  t <- smsPOMDP::tr(p0, pm, d0, dm, ds, V, Cm, Cs) #transition matrix
  
  #checking the dimensions of the matrices
  expect_equal(dim(t)[1],2)#2 states
  expect_equal(dim(t)[2],2)#2 states
  expect_equal(dim(t)[3],3)#3 actions
  
  #is the matrix stochastic
  expect_equal(t[1,1,1]+t[1,2,1],1)
  expect_equal(t[2,1,1]+t[2,2,1],1)
  expect_equal(t[1,1,2]+t[1,2,2],1)
  expect_equal(t[2,1,2]+t[2,2,2],1)
  expect_equal(t[1,1,3]+t[1,2,3],1)
  expect_equal(t[2,1,3]+t[2,2,3],1)
  
})