#' @export
simulations_tab<-function(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, Tmax, 
                          disc = 0.95, nbSimul = 100) {
  stopifnot(p0 >= 0, p0 <= 1)
  stopifnot(pm >= 0, pm <= 1)
  stopifnot(d0 >= 0, d0 <= 1)
  stopifnot(dm >= 0, dm <= 1)
  stopifnot(ds >= 0, ds <= 1)
  stopifnot(V >= 0, Cm >= 0, Cs >= 0)
  stopifnot(sum(state_prior) == 1)
  stopifnot(state_prior >= 0, state_prior <= 1)
  stopifnot(Tmax >= 0)
  stopifnot(disc >= 0, disc <= 1)
  t <- smsPOMDP::tr(p0, pm, d0, dm, ds, V, Cm, Cs)
  o <- smsPOMDP::obs(p0, pm, d0, dm, ds, V, Cm, Cs)
  r <- smsPOMDP::rew(p0, pm, d0, dm, ds, V, Cm, Cs)
  stopifnot(smsPOMDP::check_pomdp(t, o, r))
  alpha <- sarsop::sarsop(t, o, r, disc, state_prior)
  log_dir <- tempdir()
  id <- digest::digest(match.call())
  infile <- paste0(log_dir, "/", id, ".pomdpx")
  outfile <- paste0(log_dir, "/", id, ".policyx")
  stdout <- paste0(log_dir, "/", id, ".log")
  sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  policy <- sarsop::read_policyx(file = outfile)
  
  for (simu in seq_len(nbSimul)){
    
    set.seed(as.integer((as.double(Sys.time()) * 1000 + Sys.getpid())%%2^31))
    rand <- stats::runif(1)
    if (rand <= state_prior[1]) {
      real_state <- 1
    }
    else {
      real_state <- 2
    }
    output <- smsPOMDP::interp_policy(state_prior, policy$vectors, 
                                      policy$action)
    actions <- output[[2]][1]
    set.seed(as.integer((as.double(Sys.time()) * 2000 + Sys.getpid())%%2^31))
    rand <- stats::runif(1)
    if (rand <= o[real_state, 1, actions]) {
      observations <- 1
    }
    else {
      observations <- 2
    }
    
    state_posterior <- matrix(state_prior, ncol = 2)
    rewards <- r[real_state, actions]
    
    for (i in seq_len(Tmax)) {
      a1 <- actions[i]
      o1 <- observations[i]
      s_p <- smsPOMDP::update_belief(state_posterior[i, ], 
                                     t, o, o1, a1)
      s_p <- s_p/sum(s_p)
      state_posterior <- rbind(state_posterior, s_p)
      set.seed(as.integer((as.double(Sys.time()) * i * 1000 + 
                             Sys.getpid())%%2^31))
      rand <- stats::runif(1)
      if (rand < t[real_state[i], 1, a1]) {
        r_s <- 1
        real_state <- c(real_state, 1)
      }
      else {
        r_s <- 2
        real_state <- c(real_state, 2)
      }
      output <- smsPOMDP::interp_policy(s_p, policy$vectors, 
                                        policy$action)
      actions <- c(actions, output[[2]][1])
      rewards <- c(rewards, disc**i*r[r_s, output[[2]][1]])
      set.seed(as.integer((as.double(Sys.time()) * i * 21000 + 
                             Sys.getpid())%%2^31))
      rand <- stats::runif(1)
      if (rand <= o[r_s, 1, actions[i + 1]]) {
        observations <- c(observations, 1)
      }
      else {
        observations <- c(observations, 2)
      }
    }
    
    if (simu == 1){
      datasimu <- state_posterior[,1]
      rewardsim <- matrix(rewards, ncol = 1)
    } else {
      datasimu <- cbind(datasimu, state_posterior[,1])
      rewardsim <- cbind(rewardsim, rewards)
    } 
  }
  mean_belief <- apply(datasimu, 1, mean)
  sd_belief <- apply(datasimu, 1, sd)
  up_belief <- pmin(mean_belief + 1.96 * sd_belief, rep(1, length(mean_belief)))
  low_belief <- pmax(mean_belief - 1.96 * sd_belief, rep(0, length(mean_belief)))
  
  mean_reward <- apply(rewardsim, 1, mean)
  sd_reward <-  apply(rewardsim, 1, sd)
  up_reward <- mean_reward + 1.96 * sd_reward
  low_reward <- mean_reward - 1.96 * sd_reward
  
  data = data.frame(mean_belief=mean_belief,
                    up_belief = up_belief,
                    low_belief = low_belief,
                    mean_reward = mean_reward,
                    up_reward = up_reward,
                    low_reward = low_reward
  )
  
  return(data) 
}
