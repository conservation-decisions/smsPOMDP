context("smsPOMDP")

test_that("Compute current belief state given past action, observation and initial belief state", {
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
  
  z0 <- 2 #Not seen
  a0 <- 1 #manage
  state_prior <-c(1,0)
  current <- update_belief(state_prior, t, o, z0, a0)
  #is current a distribution over 2 states
  expect_length(current,2)#belief state: 2 states, length of current is 2
  expect_gte(current[1],0)
  expect_gte(current[2],0)
  expect_lte(current[1],1)
  expect_lte(current[2],1)
  expect_equal(current[1]+current[2],1)#current should be a probability distribution
  
  #test2: last observation is seen: current belief state is [1,0], the species is extant
  #previous actions and observations
  t <- smsPOMDP::tr(p0, pm, d0, dm, ds, V, Cm, Cs) #transition matrix
  o <- smsPOMDP::obs(p0, pm, d0, dm, ds, V, Cm, Cs)#observation matrix
  
  #observations and actions
  z0 <- 1 #seen
  a0 <- 1 #manage
  
  current <- update_belief(state_prior, t, o, z0, a0)
  #is current a distribution over 2 states
  expect_length(current,2)#belief state: 2 states, length of current is 2
  expect_gte(current[1],0)
  expect_gte(current[2],0)
  expect_lte(current[1],1)
  expect_lte(current[2],1)
  expect_equal(current[1]+current[2],1)#current should be a probability distribution
  expect_equal(current[1],1)# last observation is seen: current belief state is [1,0], the species is extant
  
})