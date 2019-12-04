context("smsPOMDP")

test_that("test graph", {
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

  state_prior1 <- c(1,0) #extant : 0.9, extinct : 0.1
  
  disp_graph_1 <- function() smsPOMDP::graph(p0, pm, d0, d, V, Cm, Cs, state_prior1)
  vdiffr::expect_doppelganger("disp_graph1_base",
                              disp_graph_1)
  #Initial belief state
  state_prior <- c(0.9,0.1) #extant : 0.9, extinct : 0.1
  
  disp_graph <- function() smsPOMDP::graph(p0, pm, d0, d, V, Cm, Cs, state_prior)
  vdiffr::expect_doppelganger("disp_graph_base",
                              disp_graph)
})