#' @export
compute_belief=function(p0, pm, d0, d, V, Cm, Cs, s, act, obs, disc = 0.95, size = 1)
{

  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(d>=0,d<=1) #checks if d is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif
  stopifnot(disc>=0, disc <= 1) #checks if values and costs are positif
  stopifnot(length(act)==length(obs)) #checks if there is the same number of actions and observations

  #buiding the matrices of the problem
  t = TigerPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  o = TigerPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
  r = TigerPOMDP::rew(p0, pm, d0, d, V, Cm, Cs) #reward matrix

  T = length(act)
  conv_action = function(asdf){
    switch (asdf, Manage = 1, Survey = 2, Nothing = 3)
  }
  action = unlist(lapply(act, conv_action))

  conv_obs = function(asdf){
    switch (asdf, Seen = 1, Not_seen = 2)
  }
  observation = unlist(lapply(obs, conv_obs))

  #calcul of belief states, extant
  state_posterior = matrix(s, ncol = 2)
  update_belief <- function(state_prior, transition, observation, z0, a0){
    belief <-
      vapply(1:length(state_prior), function(i){
        state_prior %*% transition[, i, a0] * observation[i, z0, a0]
      }, numeric(1))
    belief / sum(belief)
  }
  for (i in c(1:(T-1))){
    a1 = action[i]
    o1 = observation[i]
    s_p <- update_belief(state_posterior[i,], t, o, o1, a1)
    state_posterior = rbind(state_posterior,s_p)
  }
  return(state_posterior[nrow(state_posterior),])
}
