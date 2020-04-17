#' @export
plot_stream <- function(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, actions, observations, disc = 0.95, size = 1)
{

  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(dm>=0,dm<=1) #checks if dm is a probability
  stopifnot(ds>=0,ds<=1) #checks if ds is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif
  stopifnot(disc>=0, disc <= 1) #checks if values and costs are positif
  stopifnot(length(actions)==length(observations)) #checks if there is the same number of actions and observationservations

  #buiding the matrices of the problem
  t <- smsPOMDP::tr(p0, pm, d0, dm, ds, V, Cm, Cs) #transition matrix
  o <- smsPOMDP::obs(p0, pm, d0, dm, ds, V, Cm, Cs)#observation matrix
  r <- smsPOMDP::rew(p0, pm, d0, dm, ds, V, Cm, Cs)#reward matrix
  
  Tmax <- length(actions)
  conv_action <- function(asdf){
    switch (asdf, Manage = 1, Survey = 2, Stop = 3)
  }
  action <- unlist(lapply(actions, conv_action))

  conv_obs <- function(asdf){
    switch (asdf, Seen = 1, Not_seen = 2)
  }
  observation <- unlist(lapply(observations, conv_obs))

  #calcul of belief states, extant
  state_posterior <- matrix(state_prior, ncol = 2)

  for (i in seq_len(Tmax)){
    a1 <- action[i]
    o1 <- observation[i]
    s_p <- smsPOMDP::update_belief(state_posterior[i,], t, o, o1, a1)
    state_posterior <- rbind(state_posterior,s_p)
  }

  graphics::par(mfrow = c(3, 1), mai = c(0.7, 0.6, 0.1, 0.1), cex.lab = size)

  #Actions
  plot2 <- graphics::plot(seq(0,Tmax-1),action, yaxt = "n",
                         pch = 19, xlab = "Time (years)", ylab = "Action",
                         ylim = c(0.9, 3.1), xlim = c(-2, Tmax), cex = 2)
  graphics::legend("topleft", legend = "Stop", bty = "n", cex = size)
  graphics::legend("left", legend = "Survey", bty = "n", cex = size)
  graphics::legend("bottomleft", legend = "Manage", bty = "n", cex = size)
  
  #Observations
  plot3 <- graphics::plot(seq(1,Tmax), observation, yaxt = "n", pch = 19,
                         xlab = "Time (years)", ylab = "Observation",
                         ylim = c(0.9, 2.1), xlim = c(-2, Tmax), cex = 2)
  graphics::legend("topleft", legend = "Not seen", bty = "n", cex = size)
  graphics::legend("bottomleft", legend = "Seen", bty = "n", cex = size)


  plot4 <- graphics::plot(seq(0,Tmax), state_posterior[,1],
                         type = "l", xlab = "Time (years)", ylab = "Belief state",
                         ylim = c(0, 1), xlim = c(-2, Tmax) )
  graphics::lines(seq(0,Tmax), state_posterior[,2], col = "red",
                  cex = size)
  graphics::legend("bottomleft", legend = c("Extant", "Extinct"),
                   col = c("black","red"), lty = 1, bty = "n", cex = size)
}
