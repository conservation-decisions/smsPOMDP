#' @export
compute_belief_list <- function (p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, act, obs, 
                                 disc = 0.95) {
  stopifnot(p0 >= 0, p0 <= 1)
  stopifnot(pm >= 0, pm <= 1)
  stopifnot(d0 >= 0, d0 <= 1)
  stopifnot(dm >= 0, dm <= 1)
  stopifnot(ds >= 0, ds <= 1)
  stopifnot(V >= 0, Cm >= 0, Cs >= 0)
  stopifnot(disc >= 0, disc <= 1)
  stopifnot(length(act) == length(obs))
  t <- smsPOMDP::tr(p0, pm, d0, dm, ds, V, Cm, Cs)
  o <- smsPOMDP::obs(p0, pm, d0, dm, ds, V, Cm, Cs)
  r <- smsPOMDP::rew(p0, pm, d0, dm, ds, V, Cm, Cs)
  conv_action <- function(asdf) {
    switch(asdf, Manage = 1, Survey = 2, Stop = 3)
  }
  action <- unlist(lapply(act, conv_action))
  conv_obs <- function(asdf) {
    if (asdf=="Seen"){
      return(1)
    } else if (asdf=="Not seen"){
      return(2)
    }
  }
  observation <- unlist(lapply(obs, conv_obs))
  state_posterior <- matrix(state_prior, ncol = 2)
  for (i in seq_len(length(act))) {
    a1 <- action[i]
    o1 <- observation[i]
    s_p <- smsPOMDP::update_belief(state_posterior[i, 
                                                   ], t, o, o1, a1)
    s_p <- s_p/sum(s_p)
    state_posterior <- rbind(state_posterior, s_p)
  }
  b <- state_posterior[nrow(state_posterior), ]
  # b <- b/sum(b)
  # return(b)
  return(state_posterior)
}
