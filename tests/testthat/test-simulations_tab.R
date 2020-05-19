context('smsPOMDP')

test_that("simulations_tab works", {
  #testing on Sumatran tiger values
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
  
  #Initial belief state
  state_prior <- c(1,0)
  Tmax <-20
  
  data <- smsPOMDP::simulations_tab(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, Tmax)
  
  #testing data output
  testthat::expect_equal(nrow(data), Tmax+1)
  testthat::expect_equal(ncol(data), 6)
  testthat::expect_named(data, c("mean_belief","up_belief","low_belief","mean_reward","up_reward","low_reward"))
  
})
