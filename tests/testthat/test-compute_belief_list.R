context("smsPOMDP")

test_that("multiplication works", {
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
  
  act <- c("Manage", "Survey", "Stop")
  obs <- c("Seen", "Seen", "Not seen")
  state_prior <- c(1,0)
  
  state_posterior<-smsPOMDP::compute_belief_list(p0, pm, d0, dm, ds, V, Cm,
                                                 Cs, state_prior, act, obs)
  
  testthat::expect_equal(nrow(state_posterior), length(act)+1)
  testthat::expect_equal(ncol(state_posterior), 2)
  testthat::expect_equal(unname(state_posterior[,1]+state_posterior[,2]),
                         rep(1,length(act)+1))
})
