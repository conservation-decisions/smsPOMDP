#' @export
pomdp_check = function(transition, observation, reward, state_prior){
  is_error_detected <- FALSE
  error_msg <- ""

  #Checking transition matrix
  s1 <- dim(transition)[1]#number of states (rows)
  s2 <- dim(transition)[2]#number of stater (cols)
  a1 <- dim(transition)[3]#number of actions

  if (s1 < 1 | a1 < 1 | s1 != s2) {
    error_msg <- "The transition matrix must be on the form P(S,S,A) with S : number of states greater than 0 and A : number of action greater than 0"
    is_error_detected <- T
  }
  if (!is_error_detected) {
    a <- 1
    while (a <= a1) {
      error <- smsPOMDP::check_square_stochastic(transition[,,a])
      if (length(error_msg) == 0) {
        a <- a + 1
      } else {
        a <- a1 + 1
      }
    }
  }
  
  #Checking reward matrix
  if (!is_error_detected) {
      s3 <- dim(reward)[1]#number of states of the reward matrix
      a2 <- dim(reward)[2]#number of actions of the reward matrix
    if (s3 < 1 | a2 < 1) {
      error_msg <- "The reward matrix R must be an array (SxA) with S : number of states greater than 0 and A : number of actions greater than 0"
      is_error_detected <- T
    }
  }
  if (!is_error_detected) {
    if (s1 != s3 | a1 != a2) {
      error_msg <- "Incompatibility between transition and reward dimensions"
      is_error_detected <- T
    }
  }
  
  #checking observation matrix
  if (!is_error_detected) {
    s4 <- dim(observation)[1]#number of states of the reward matrix
    o1 <- dim(observation)[1]#number of states of the reward matrix
    a3 <- dim(reward)[2]#number of actions of the reward matrix
    if (s4 < 1 | a3 < 1) {
      error_msg <- "The reward matrix R must be an array (S,S,A) or (SxA) with S : number of states greater than 0 and A : number of actions greater than 0"
      is_error_detected <- T
    }
  }
  if (!is_error_detected) {
    if (s1 != s4 | a1 != a3) {
      error_msg <- "Incompatibility between transition and observation dimensions"
      is_error_detected <- T
    }
  }
  
  return(is_error_detected)
}
