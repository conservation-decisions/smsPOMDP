library(testthat)
library(smsPOMDP)

test_file("~/smsPOMDP/tests/testthat/test-compute_belief.R")
test_file("~/smsPOMDP/tests/testthat/test-Interp_policy.R")
test_file("~/smsPOMDP/tests/testthat/test-graph.R")
test_file("~/smsPOMDP/tests/testthat/test-minigraph.R")
test_file("~/smsPOMDP/tests/testthat/test-obs.R")
test_file("~/smsPOMDP/tests/testthat/test-plot_stream.R")
test_file("~/smsPOMDP/tests/testthat/test-rew.R")
test_file("~/smsPOMDP/tests/testthat/test-tab_actions.R")
test_file("~/smsPOMDP/tests/testthat/test-tr.R")
test_file("~/smsPOMDP/tests/testthat/test-update_belief.R")
test_file('~/smsPOMDP/tests/testthat/test-run_application.R')

test_package('smsPOMDP')

# Code used to check run_application 
# the folder smsPOMDP/tests/testthat/app/ contains a file calling run_application
# library(shinytest)
# library(shiny)
# recordTest("~/smsPOMDP/tests/testthat/app/")
