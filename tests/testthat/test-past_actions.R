context("smsPOMDP")

test_that("check,past_actions()", {
#repoducing input of shiny app as a list
input <- list(1, 1, 1, 1, "Not_seen", 1, 3, 175.133, 1, "Not_seen", 0.9, "Stop",
              0.78193, 0, 0, "Manage", 0.95, 10.84, "Survey", 0.01, 10, 0.94184,
              18.784, 1, "Seen", 1)

n <- c("past","submit_couple_3","submit_length_past","submit_couple_2"
           ,"past_obs_2","next_policy","length_past","V"
           ,"submit_couple_1","past_obs_1","p0","past_action_3"
           ,"d","graph","sim","past_action_1"
           ,"disc","Cs","past_action_2","d0"
           ,"Tmax","pm","Cm","b"
           ,"past_obs_3","past_init_b")

names(input) <- n

values <- smsPOMDP::past_actions(input)
testthat::expect_equal(values[1], 'Manage')
testthat::expect_equal(values[2], 'Survey')
testthat::expect_equal(values[3], 'Stop')

})