context("smsPOMDP")

test_that("interp_policy returns action and value function", {
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
  t <- smsPOMDP::tr(p0, pm, d0, dm, ds, V, Cm, Cs) #transition matrix
  o <- smsPOMDP::obs(p0, pm, d0, dm, ds, V, Cm, Cs)#observation matrix
  r <- smsPOMDP::rew(p0, pm, d0, dm, ds, V, Cm, Cs)#reward matrix
  
  state_prior <- c(0.5,0.5) #initial belief state
  log_dir <- tempdir()
  id <- 'test-interp_policy'
  infile <- paste0(log_dir, "/", id, ".pomdpx")
  outfile <- paste0(log_dir, "/", id, ".policyx")
  stdout <- paste0(log_dir, "/", id, ".log")
  
  sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  policy <- sarsop::read_policyx(file = outfile)
  output <- smsPOMDP::interp_policy(state_prior,policy$vectors,policy$action)
  output <- c(output)
  expect_equal(length(output), 2)#value and actions
  
  #2nd case
  state_prior <- c(0,0) #initial belief state
  output <- smsPOMDP::interp_policy(state_prior,policy$vectors,policy$action)
  output <- c(output)
  expect_equal(length(output), 2)#value and actions
  expect_equal(output[[1]], 0)#value
  expect_equal(output[[2]], 1)#action
  
})