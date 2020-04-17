#' @export
sim <- function(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, Tmax, disc = 0.95, size = 1)
{
  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(dm>=0,dm<=1) #checks if dm is a probability
  stopifnot(ds>=0,ds<=1) #checks if ds is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif
  stopifnot(disc>=0, disc <= 1) #checks if values and costs are positif
  stopifnot(T >0) #positive horizon
  #Compute matrices
  t <- smsPOMDP::tr(p0, pm, d0, d, V, Cm, Cs)
  o <- smsPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)
  r <- smsPOMDP::rew(p0, pm, d0, d, V, Cm, Cs)
  
  stopifnot(check_pomdp(t,o,r))
  
  #solve POMDP
  alpha <- sarsop::sarsop(t, o, r, disc, state_prior)
  log_dir <- tempdir()
  id <- digest::digest(match.call())
  infile <- paste0(log_dir, "/", id, ".pomdpx")
  outfile <- paste0(log_dir, "/", id, ".policyx")
  stdout <- paste0(log_dir, "/", id, ".log")
  sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  policy <- sarsop::read_policyx(file = outfile)
  
  #initialise
  #randomize real initial state
  set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
  rand <- stats::runif(1)
  if (rand <= state_prior[1]){
    real_state <- 1
  } else {
    real_state <- 2
  }
  #compute best action given belief state
  output <- smsPOMDP::interp_policy(state_prior,policy$vectors,policy$action)
  actions <- output[[2]][1]
  #random observation
  set.seed( as.integer((as.double(Sys.time())*2000+Sys.getpid()) %% 2^31) )
  rand <- stats::runif(1)
  if (rand <= o[real_state,1, actions]){
    observations <- 1
  } else {
    observations <- 2
  }
  state_posterior <- matrix(state_prior, ncol = 2)
  for (i in seq_len(Tmax-1)){
    a1 <- actions[i]
    o1 <- observations[i]
    #compute next belief state
    s_p <- smsPOMDP::update_belief(state_posterior[i, ], 
                                   t, o, o1, a1)
    state_posterior <- rbind(state_posterior, s_p)
    set.seed( as.integer((as.double(Sys.time())*i*1000+Sys.getpid()) %% 2^31) )
    rand <- stats::runif(1)
    if (rand < t[real_state[i],1,a1]){
      r_s <- 1
      real_state <- c(real_state,1)
    } else {
      r_s <- 2
      real_state <- c(real_state,2)
    }
    #compute best action given belief state
    output <- smsPOMDP::interp_policy(s_p,policy$vectors,policy$action)
    actions <- c(actions, output[[2]][1])
    #random observation
    set.seed( as.integer((as.double(Sys.time())*i*21000+Sys.getpid()) %% 2^31) )
    rand <- stats::runif(1)
    if (rand <= o[r_s,1, actions[i+1]]){
      observations <- c(observations,1)
    } else {
      observations <- c(observations,2)
    }
  }
  
  
  graphics::par(mfrow = c(4, 1), mai = c(0.7, 0.6, 0.1, 0.1), 
                cex.lab = size)
  plot1 <- graphics::plot(seq(0, Tmax-1), real_state, yaxt = "n", 
                         pch = 19, xlab = "Time (years)", ylab = "State", 
                         ylim = c(0.9, 2.1), xlim = c(-2, Tmax), cex = 2)
  graphics::legend("topleft", legend = "Extinct", bty = "n", 
                   cex = size)
  graphics::legend("bottomleft", legend = "Extant", bty = "n", 
                   cex = size)
  plot2 <- graphics::plot(seq(0, Tmax-1), actions, yaxt = "n", 
                         pch = 19, xlab = "Time (years)", 
                         ylab = "Action", ylim = c(0.9, 3.1), 
                         xlim = c(-2, Tmax), cex = 2)
  graphics::legend("topleft", legend = "Stop", bty = "n", 
                   cex = size)
  graphics::legend("left", legend = "Survey", bty = "n", cex = size)
  graphics::legend("bottomleft", legend = "Manage", bty = "n", 
                   cex = size)
  plot3 <- graphics::plot(seq(Tmax), observations, yaxt = "n", pch = 19, 
                         xlab = "Time (years)", ylab = "Observation",
                         ylim = c(0.9, 2.1), xlim = c(-2, Tmax), cex = 2)
  graphics::legend("topleft", legend = "Not seen", bty = "n", 
                   cex = size)
  graphics::legend("bottomleft", legend = "Seen", bty = "n", 
                   cex = size)
  plot4 <- graphics::plot(seq(0, Tmax-1), state_posterior[, 1], 
                         type = "l", xlab = "Time (years)", ylab = "Belief state", 
                         ylim = c(0, 1), xlim = c(-2, Tmax))
  graphics::lines(seq(0, Tmax-1), state_posterior[, 2], col = "red", 
                  cex = size)
  graphics::legend("bottomleft", legend = c("Extant", "Extinct"), 
                   col = c("black", "red"), lty = 1, bty = "n", cex = size)

}
