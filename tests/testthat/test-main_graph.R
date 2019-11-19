context("smsPOMDP")

test_that("test main_graph", {
  pen = 0.1 #local probability of extinction P(extinct/extant, survey or nothing)
  p0 = 1-pen #local probability of persitance P(extant/extant, manage)
  pem = 0.05816 #local probability of extinction if managed P(extinct/extant, manage)
  pm = 1 - pem #local probability of persistance if managed P(extant/extant, manage)
  d0 = 0.01 #local probability of detection P(present/extant, manage or nothing)
  d = 0.78193 #local probability of detection if surveyed P(present/extant, survey)
  V = 175.133 #Estimated economic value of the species ($/yr)
  Cm = 18.784 #Estimated cost of managing ($/yr)
  Cs = 10.840 #Estimated cost of surveying ($/yr)
  disc = 0.95
  #buiding the matrices of the problem
  #Initial belief state
  state_prior = c(0.9,0.1) #extant : 0.9, extinct : 0.1
  
  disp_main_graph = function() smsPOMDP::main_graph(p0, pm, d0, d, V, Cm, Cs, state_prior)
  vdiffr::expect_doppelganger("disp_main_graph_base",
                              disp_main_graph)
})