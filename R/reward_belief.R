#' @export
reward_belief <- function(p0, pm, d0, dm, ds, V, Cm, Cs,
                          state_posterior, act, disc = 0.95){
  stopifnot(p0 >= 0, p0 <= 1)
  stopifnot(pm >= 0, pm <= 1)
  stopifnot(d0 >= 0, d0 <= 1)
  stopifnot(dm >= 0, dm <= 1)
  stopifnot(ds >= 0, ds <= 1)
  stopifnot(V >= 0, Cm >= 0, Cs >= 0)
  stopifnot(disc >= 0, disc <= 1)
  r <- smsPOMDP::rew(p0, pm, d0, dm, ds, V, Cm, Cs)
  conv_action <- function(asdf) {
    switch(asdf, Manage = 1, Survey = 2, Stop = 3)
  }
  action <- unlist(lapply(act, conv_action))
  
  rewards <- c()
  for (i in seq(1, min(nrow(state_posterior), length(action)))){
    rew <- state_posterior[i,1]*r[1, action[i]] + state_posterior[i,2]*r[2, action[i]]
    rewards <- c(rewards, rew)
  }
  return(rewards)
}
