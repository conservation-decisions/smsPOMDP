#' @export
plot_stream=function(p0, pm, d0, d, V, Cm, Cs, s, act, obs, disc = 0.95, size = 1)
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
    switch (asdf, Manage = 1, Survey = 2, Nothing = 3)
  }
  action = unlist(lapply(act, conv_action))

  conv_obs = function(asdf){
    switch (asdf, Seen = 1, Not_seen = 2)
  }
  observation = unlist(lapply(obs, conv_obs))

  #calcul of belief states, extant
  state_posterior = matrix(s, ncol = 2)

  for (i in c(1:(T))){
    a1 = action[i]
    o1 = observation[i]
    s_p <- smsPOMDP::update_belief(state_posterior[i,], t, o, o1, a1)
    state_posterior = rbind(state_posterior,s_p)
  }

  graphics::par(mfrow = c(3, 1), mai = c(0.7, 0.6, 0.1, 0.1), cex.lab = size)

  #Actions
  plot2 = graphics::plot(c(0:(T-1)),action, yaxt = "n",
                         pch = 19, xlab = "Time (years)", ylab = "Action", ylim = c(0.9,
                                                                                    3.1), xlim = c(-2, T), cex = 2)
  graphics::legend("topleft", legend = "Nothing", bty = "n", cex = size)
  graphics::legend("left", legend = "Survey", bty = "n", cex = size)
  graphics::legend("bottomleft", legend = "Manage", bty = "n", cex = size)
  #Observation
  plot3 = graphics::plot(c(1:(T)), observation, yaxt = "n", pch = 19,
                         xlab = "Time (years)", ylab = "Observation", ylim = c(0.9,
                                                                               2.1), xlim = c(-2, T), cex = 2)
  graphics::legend("topleft", legend = "Not seen", bty = "n", cex = size)
  graphics::legend("bottomleft", legend = "Seen", bty = "n", cex = size)


  plot4 = graphics::plot(c(0:(T)), state_posterior[,1],
                         type = "l", xlab = "Time (years)", ylab = "Probabilities",
                         ylim = c(0, 1), xlim = c(-2, T) )
  graphics::lines(c(0:(T)), state_posterior[,2], col = "red",
                  cex = size)
  graphics::legend("bottomleft", legend = c("Extant", "Extinct"), col = c("black",
                                                                          "red"), lty = 1, bty = "n", cex = size)
}
