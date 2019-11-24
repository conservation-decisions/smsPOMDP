context("smsPOMDP")

test_that("check pomdp", {
  #EXPECT PASS THE TEST
  #values for Sumatran tigers
  pen <- 0.1
  p0 <- 1-pen
  pem <- 0.05816
  pm <- 1 - pem
  V <- 175.133
  Cm <- 18.784
  Cs <- 10.840
  d0 <- 0.01
  d <- 0.78193
  
  t <- smsPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  o <- smsPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
  r <- smsPOMDP::rew(p0, pm, d0, d, V, Cm, Cs)#reward matrix
  
  testthat::expect_true(smsPOMDP::check_pomdp(t, o, r))

  #EXPECT TO FAIL THE TEST
  #not stochastic transition matrix
  t1 <- array(0, dim = c(2,2,3))
  testthat::expect_false(smsPOMDP::check_pomdp(t1, o, r))
  
  #not stochastic observation
  o1 <- array(0, dim = c(2,2,3))
  testthat::expect_false(smsPOMDP::check_pomdp(t, o1, r))
  
  #not compatible number of actions: transition and reward
  r1 <- array(0, dim = c(2,2))
  testthat::expect_false(smsPOMDP::check_pomdp(t, o, r1))
  
  #not compatible number of actions: transition and observation
  o2 <- array(c(o[,,1], o[,,2]), dim = c(2,2,2))
  testthat::expect_false(smsPOMDP::check_pomdp(t, o2, r))
  
})