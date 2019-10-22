context("smsPOMDP")

test_that("Minigraph: test all different possible graphs", {
  #the different tabs are a data.frame resuming the actions to be implemented,
  #the order and the number of years, 
  #in which they should be implemented if the species is not seen, 
  #usually used if the initial belief state is c(1,0) (species known extant).
  #Can be computed using tab_actions with c(1,0) as initial belief state
  
  #case 1: only one tab
  #3actions
  tab1 = data.frame(action = c(1,2,3), years = c(10,2,89))#manage for 10 years, survey for 2 years and stop
  disp_man10_sur2_stop = function() smsPOMDP::minigraph(tab1, test =T)
  vdiffr::expect_doppelganger("disp_man10_sur2_stop_base", disp_man10_sur2_stop)
  
  #2 actions
  tab2 = data.frame(action = c(2,3), years = c(2,89))#survey for 2 years and stop
  disp_sur2_stop = function() smsPOMDP::minigraph(tab2, test =T)
  vdiffr::expect_doppelganger("disp_sur2_stop_base", disp_sur2_stop)
  
  #1 action
  tab3 = data.frame(action = c(3), years = c(89))#stop
  disp_stop = function() smsPOMDP::minigraph(tab3, test =T)
  vdiffr::expect_doppelganger("disp_stop_base", disp_stop)
  
  #case 2: 2 tabs
  #1st tab 3 actions, 2nd tab 3 actions
  tab11 = data.frame(action = c(1,2,3), years = c(10,2,89))#manage for 10 years, survey for 2 years and stop
  tab12 = data.frame(action = c(1,2,3), years = c(8,2,89))#manage for 8 years, survey for 2 years and stop
  disp_man10_sur2_stop_man8_sur2_stop = function() smsPOMDP::minigraph(tab11, tab12, test =T)
  vdiffr::expect_doppelganger("disp_man10_sur2_stop_man8_sur2_stop_base",
                              disp_man10_sur2_stop_man8_sur2_stop)

  #1st tab 3 actions, 2nd tab 2 actions
  tab11 = data.frame(action = c(1,2,3), years = c(10,2,89))#manage for 10 years, survey for 2 years and stop
  tab12 = data.frame(action = c(2,3), years = c(1,89))#survey for 1 years and stop
  disp_man10_sur2_stop_sur1_stop = function() smsPOMDP::minigraph(tab11, tab12, test =T)
  vdiffr::expect_doppelganger("disp_man10_sur2_stop_sur1_stop_base",
                              disp_man10_sur2_stop_sur1_stop)
  
  #1st tab 3 actions, 2nd tab 1 actions
  tab11 = data.frame(action = c(1,2,3), years = c(10,2,89))#manage for 10 years, survey for 2 years and stop
  tab12 = data.frame(action = c(3), years = c(89))# stop
  disp_man10_sur2_stop_stop = function() smsPOMDP::minigraph(tab11, tab12, test =T)
  vdiffr::expect_doppelganger("disp_man10_sur2_stop_stop_base",
                              disp_man10_sur2_stop_stop)
  
  #1st tab 2 actions, 2nd tab 2 actions
  tab11 = data.frame(action = c(1,3), years = c(10,89))#manage for 10 years and stop
  tab12 = data.frame(action = c(1,3), years = c(1,89))#manage for 1 years and stop
  disp_man10_stop_man1_stop = function() smsPOMDP::minigraph(tab11, tab12, test =T)
  vdiffr::expect_doppelganger("disp_man10_stop_man1_stop_base",
                              disp_man10_stop_man1_stop)
  
  #1st tab 2 actions, 2nd tab 1 action
  tab11 = data.frame(action = c(1,3), years = c(10,89))#survey for 10 years and stop
  tab12 = data.frame(action = c(3), years = c(89))#stop
  disp_sur10_stop_stop = function() smsPOMDP::minigraph(tab11, tab12, test =T)
  vdiffr::expect_doppelganger("disp_sur10_stop_stop_base",
                              disp_sur10_stop_stop) 
  
})