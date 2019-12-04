context('smsPOMDP')

test_that("plot_stream: plots have known output", {
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
  
  #Initial belief state
  state_prior <- c(0,1) #only deterministic case
  Tmax <-20
  disp_plot_sim = function () smsPOMDP::sim(p0, pm, d0, d, V, Cm, Cs, state_prior, Tmax)
  
  vdiffr::expect_doppelganger("disp-sim-base", disp_plot_sim)
  #plot checked using the package vdiffr
})