#' @export
graph = function(p0, pm, d0, d, V, Cm, Cs, disc=0.95, size = 1){

  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(d>=0,d<=1) #checks if d is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif
  stopifnot(disc>=0, disc <= 1) #checks if the discount factor is between 0 and 1

  #buiding the matrices of the problem
  t = TigerPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  o = TigerPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
  r = TigerPOMDP::rew(p0, pm, d0, d, V, Cm, Cs) #reward matrix

  state_prior = c(1,0) #initial belief state
  log_dir = tempdir()
  id <- digest::digest(match.call())
  infile <- paste0(log_dir, "/", id, ".pomdpx")
  outfile <- paste0(log_dir, "/", id, ".policyx")
  stdout <- paste0(log_dir, "/", id, ".log")

  sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  policy <- sarsop::read_policyx(file = outfile)
  output <- TigerPOMDP::Interp_policy(state_prior,policy$vectors,policy$action)

  state_posterior = matrix(state_prior, ncol = 2)
  optimal_action = output[[2]]
  Tmax = 100
  for (i in c(1:(Tmax))) {
    a1 = optimal_action[i]
    o1 = 2 #we treat the case when the species is not seen
    s_p <- TigerPOMDP::update_belief(state_posterior[i, ], t, o, o1,
                         a1)
    state_posterior = rbind(state_posterior, s_p)
    output <- Interp_policy(s_p,policy$vectors,policy$action)

    optimal_action = c(optimal_action, output[[2]])
  }


  a = optimal_action[1]
  act = a
  years = numeric()
  while(sum(years) < length(optimal_action)){
    if (length(unique(optimal_action))==1){
      i = length(optimal_action)
      years = c(years, i)
      break
    } else {
      i = min(which(optimal_action!= a))-1
      optimal_action = optimal_action[-c(1:i)]
      a = optimal_action[1]
      act = c(act,a)
      years = c(years, i)
    }
  }
  tab = data.frame(action = act, years = years)
  return(TigerPOMDP::minigraph(tab, size))
}
