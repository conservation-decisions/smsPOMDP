library(testthat)
library(smsPOMDP)

test_check("smsPOMDP")


devtools::install_github("conservation-decisions/smsPOMDP", host = "https://api.github.com")
library(smsPOMDP)
