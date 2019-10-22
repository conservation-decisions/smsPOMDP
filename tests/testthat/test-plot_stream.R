library("ggplot2")

test_that("plots have known output", {
  #values for Sumatran tigers
  pen = 0.1
  p0 = 1-pen
  pem = 0.05816
  pm = 1 - pem
  V = 175.133
  Cm = 18.784
  Cs = 10.840
  d0 = 0.01
  d = 0.78193
  
  s = c(1,0)
  act = c('Manage', 'Manage', 'Survey','Stop')
  obs = c('Not_seen','Not_seen','Not_seen','Seen')
  disp_plot_stream = function () smsPOMDP::plot_stream(p0, pm, d0, d, V, Cm, Cs, s, act, obs, disc = 0.95, size = 1)
  
  vdiffr::expect_doppelganger("disp-plot-stream-base", disp_plot_stream)
  #plot checked using the package vdiffr
})