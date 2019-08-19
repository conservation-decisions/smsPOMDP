context("smsPOMDP")

test_that("Compute current belief state given past actions, observations and initial belief state", {
  pen = 0.1 #local probability of extinction P(extinct/extant, survey or nothing)
  p0 = 1-pen #local probability of persitance P(extant/extant, manage)
  pem = 0.05816 #local probability of extinction if managed P(extinct/extant, manage)
  pm = 1 - pem #local probability of persistance if managed P(extant/extant, manage)
  d0 = 0.01 #local probability of detection P(present/extant, manage or nothing)
  d = 0.78193 #local probability of detection if surveyed P(present/extant, survey)
  V = 175.133 #Estimated economic value of the species ($/yr)
  Cm = 18.784 #Estimated cost of managing ($/yr)
  Cs = 10.840 #Estimated cost of surveying ($/yr)
  
  #Initial belief state
  state_prior = c(0.9,0.1) #extant : 0.9, extinct : 0.1
  
  #test1: last observation is seen: current belief state is [1,0], the species is extant
  #previous actions and observations
  ac = c('Manage','Survey','Stop')
  ob = c('Seen','Not_seen','Not_seen')
  current = compute_belief(p0, pm, d0, d, V, Cm, Cs, state_prior, ac, ob)#current belief state
  
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
  ac = c('Manage','Survey','Stop')
  ob = c('Not_seen','Not_seen', 'Seen')
  current = compute_belief(p0, pm, d0, d, V, Cm, Cs, state_prior, ac, ob)#current belief state

  #is current a distribution over 2 states
  expect_length(current,2)#belief state: 2 states, length of current is 2
  expect_gte(current[1],0)
  expect_gte(current[2],0)
  expect_lte(current[1],1)
  expect_lte(current[2],2)
  expect_equal(current[1]+current[2],1)#current should be a probability distribution
  expect_equal(current[1],1)# last observation is seen: current belief state is [1,0], the species is extant
  
})