#' @export
compute_belief=function(p0, pm, d0, d, V, Cm, Cs, state_prior, act, obs, disc = 0.95, size = 1)
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
  t = smsPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  o = smsPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
  r = smsPOMDP::rew(p0, pm, d0, d, V, Cm, Cs) #reward matrix

  T = length(act)
  conv_action = function(asdf){
    switch (asdf, Manage = 1, Survey = 2, Stop = 3)
  }
  action = unlist(lapply(act, conv_action))

  conv_obs = function(asdf){
    switch (asdf, Seen = 1, Not_seen = 2)
  }
  observation = unlist(lapply(obs, conv_obs))

  #calcul of belief states, extant
  state_posterior = matrix(state_prior, ncol = 2)
  Tmax = length(act)
  for (i in c(1:(Tmax))){
    a1 = action[i]
    o1 = observation[i]
    s_p <- smsPOMDP::update_belief(state_posterior[i,], t, o, o1, a1)
    state_posterior = rbind(state_posterior,s_p)
  }
  
  b = state_posterior[nrow(state_posterior),]
  b = b/sum(b) #trick used to avoid to return a belief that is not a probability distribution
  return(b)
}
