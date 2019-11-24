#' @export
check_pomdp <- function(transition, observation, reward){
  test_passed <- TRUE

  #Checking transition matrix
  s1 <- dim(transition)[1]#number of states (rows)
  s2 <- dim(transition)[2]#number of stater (cols)
  a1 <- dim(transition)[3]#number of actions

  if (s1 < 1 | a1 < 1 | s1 != s2) {
    test_passed <- FALSE
  }
  
  if (test_passed) {
    a <- 1
    while (a <= a1) {
      pass <- smsPOMDP::check_square_stochastic(transition[,,a])
      #pass=TRUE if transition[,,a] is square stochastic
      if (pass) {
        a <- a + 1
      } else {
        a <- a1 + 1
        test_passed <- FALSE
      }
    }
  }
  
  #Checking reward matrix
  if (test_passed) {
      s3 <- dim(reward)[1]#number of states of the reward matrix
      a2 <- dim(reward)[2]#number of actions of the reward matrix
    if (s3 < 1 | a2 < 1) {
      test_passed <- FALSE
    }
  }
  if (test_passed) {
    if (s1 != s3 | a1 != a2) {
      test_passed <- FALSE
    }
  }
  
  #checking observation matrix
  if (test_passed) {
    s4 <- dim(observation)[1]#number of states of the observation matrix
    o1 <- dim(observation)[2]#number of observations of the observation matrix
    a3 <- dim(observation)[3]#number of actions of the observation matrix
    if (s4 < 1 | a3 < 1) {
      test_passed <- FALSE
    }
  }
  
  if (test_passed) {
    a <- 1
    while (a <= a3) {
      pass <- smsPOMDP::check_stochastic(observation[,,a])
      #pass=TRUE if observation[,,a] is square stochastic
      if (pass) {
        a <- a + 1
      } else {
        a <- a1 + 1
        test_passed <- FALSE
      }
    }
  }
  if (test_passed) {
    if (s1 != s4 | a1 != a3) {
      test_passed <- FALSE
    }
  }
  return(test_passed)
}
