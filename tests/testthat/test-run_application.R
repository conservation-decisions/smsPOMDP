context("smsPOMDP")
# This file is for testing the applications in the apps/ directory.

library(shinytest)

test_that("run_application() works", {
  # Don't run these tests on the CRAN build servers
  #  skip_on_cran()

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  testthat::expect_equal(class(run_application()), "shiny.appobj")
  
  appdir <- system.file(package = "smsPOMDP", "app")
  testApp(appdir, compareImages = FALSE)
})