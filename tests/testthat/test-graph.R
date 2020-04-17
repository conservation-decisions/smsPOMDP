context("smsPOMDP")

test_that("test graph", {
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

  state_prior1 <- c(1,0) #extant : 0.9, extinct : 0.1
  
  disp_graph_1 <- function() smsPOMDP::graph(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior1)
  vdiffr::expect_doppelganger("disp_graph1_base",
                              disp_graph_1)
  #Initial belief state
  state_prior2 <- c(0.9,0.1) #extant : 0.9, extinct : 0.1
  
  disp_graph <- function() smsPOMDP::graph(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior2)
  vdiffr::expect_doppelganger("disp_graph_base",
                              disp_graph)
})