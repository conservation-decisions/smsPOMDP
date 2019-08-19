context("smsPOMDP")

test_that("transition matrix between states", {
  pen = 0.1 #local probability of extinction P(extinct/extant, survey or nothing)
  p0 = 1-pen #local probability of persitance P(extant/extant, manage)
  pem = 0.05816 #local probability of extinction if managed P(extinct/extant, manage)
  pm = 1 - pem #local probability of persistance if managed P(extant/extant, manage)
  d0 = 0.01 #local probability of detection P(present/extant, manage or nothing)
  d = 0.78193 #local probability of detection if surveyed P(present/extant, survey)
  V = 175.133 #Estimated economic value of the species ($/yr)
  Cm = 18.784 #Estimated cost of managing ($/yr)
  Cs = 10.840 #Estimated cost of surveying ($/yr)
  
  #buiding the matrices of the problem
  t = smsPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  
  #checking the dimensions of the matrices
  expect_equal(dim(t)[1],2)#2 states
  expect_equal(dim(t)[2],2)#2 states
  expect_equal(dim(t)[3],3)#3 actions
  
  #is the matrix stochastic
  expect_equal(t[1,1,1]+t[1,2,1],1)
  expect_equal(t[2,1,1]+t[2,2,1],1)
  expect_equal(t[1,1,2]+t[1,2,2],1)
  expect_equal(t[2,1,2]+t[2,2,2],1)
  expect_equal(t[1,1,3]+t[1,2,3],1)
  expect_equal(t[2,1,3]+t[2,2,3],1)
  
})