context("smsPOMDP")

test_that("reward_belief works", {
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

  state_posterior<-smsPOMDP::compute_belief_list(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, act, obs)
  
  rew_tab<-smsPOMDP::reward_belief(p0, pm, d0, dm, ds, V, Cm, Cs,state_posterior, act)
  
  testthat::expect_equal(length(rew_tab), length(act))
  testthat::expect_equal(unname(rew_tab[1]), V-Cm)
  testthat::expect_equal(unname(rew_tab[2]), V-Cs)
  testthat::expect_equal(unname(rew_tab[3]), V)
  
  
})