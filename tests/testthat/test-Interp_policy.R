context("smsPOMDP")

test_that("Interp_policy returns action and value function", {
  pen <- 0.1 #local probability of extinction P(extinct/extant, survey or nothing)
  p0 <- 1-pen #local probability of persitance P(extant/extant, manage)
  pem <- 0.05816 #local probability of extinction if managed P(extinct/extant, manage)
  pm <- 1 - pem #local probability of persistance if managed P(extant/extant, manage)
  d0 <- 0.01 #local probability of detection P(present/extant, manage or nothing)
  d <- 0.78193 #local probability of detection if surveyed P(present/extant, survey)
  V <- 175.133 #Estimated economic value of the species ($/yr)
  Cm <- 18.784 #Estimated cost of managing ($/yr)
  Cs <- 10.840 #Estimated cost of surveying ($/yr)
  disc <- 0.95
  #buiding the matrices of the problem
  t <- smsPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  o <- smsPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
  r <- smsPOMDP::rew(p0, pm, d0, d, V, Cm, Cs) #reward matrix
  
  state_prior <- c(0.5,0.5) #initial belief state
  log_dir <- tempdir()
  id <- 'test-Interp_policy'
  infile <- paste0(log_dir, "/", id, ".pomdpx")
  outfile <- paste0(log_dir, "/", id, ".policyx")
  stdout <- paste0(log_dir, "/", id, ".log")
  
  sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  policy <- sarsop::read_policyx(file = outfile)
  output <- smsPOMDP::Interp_policy(state_prior,policy$vectors,policy$action)
  output = c(output)
  expect_equal(length(output), 2)#value and actions
  
  #2nd case
  state_prior <- c(0,0) #initial belief state
  log_dir <- tempdir()
  id <- 'test-Interp_policy'
  infile <- paste0(log_dir, "/", id, ".pomdpx")
  outfile <- paste0(log_dir, "/", id, ".policyx")
  stdout <- paste0(log_dir, "/", id, ".log")
  
  sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  policy <- sarsop::read_policyx(file = outfile)
  output <- smsPOMDP::Interp_policy(state_prior,policy$vectors,policy$action)
  output = c(output)
  expect_equal(length(output), 2)#value and actions
  expect_equal(output[1], 0)#value
  expect_equal(output[2], 1)#action
  
})