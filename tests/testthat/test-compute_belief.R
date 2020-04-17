context("smsPOMDP")

test_that("Compute current belief state given past list (string) of actions, observations and initial belief state", {
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
  
  #Initial belief state
  state_prior <- c(0.9,0.1) #extant : 0.9, extinct : 0.1
  
  #test1: last observation is not seen: current belief state is not[1,0]
  #previous actions and observations
  ac <- c('Manage','Survey','Stop')
  ob <- c('Seen','Not_seen','Not_seen')
  current <- compute_belief(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, ac, ob)#current belief state
  
  #is current a distribution over 2 states
  expect_length(current,2)#belief state: 2 states, length of current is 2
  expect_gte(current[1],0)
  expect_gte(current[2],0)
  expect_lte(current[1],1)
  expect_lte(current[2],2)
  expect_equal(current[1]+current[2],1)#current should be a probability distribution
  expect_lt(current[1],1)# last observation is seen: current belief state is [1,0], the species is extant
  
  #test2: last observation is seen: current belief state is [1,0], the species is extant
  #previous actions and observations
  ac <- c('Manage','Survey','Stop')
  ob <- c('Not_seen','Not_seen', 'Seen')
  current <- compute_belief(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, ac, ob)#current belief state
  
  #is current a distribution over 2 states
  expect_length(current,2)#belief state: 2 states, length of current is 2
  expect_gte(current[1],0)
  expect_gte(current[2],0)
  expect_lte(current[1],1)
  expect_lte(current[2],2)
  expect_equal(current[1]+current[2],1)#current should be a probability distribution
  expect_equal(current[1],1)# last observation is seen: current belief state is [1,0], the species is extant
  
})