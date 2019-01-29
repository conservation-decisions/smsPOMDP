#' @export
sim=function(p0, pm, d0, d, V, Cm, Cs, state_prior, T, disc = 0.95, size = 1)
{

  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(d>=0,d<=1) #checks if d is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif
  stopifnot(disc>=0, disc <= 1) #checks if values and costs are positif
  stopifnot(T >0) #positive horizon

  #buiding the matrices of the problem
  t = TigerPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  o = TigerPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
  r = TigerPOMDP::rew(p0, pm, d0, d, V, Cm, Cs) #reward matrix

  alpha = sarsop::sarsop(t, o, r, disc, state_prior)

  log_dir = tempdir()
  id <- digest::digest(match.call())
  infile <- paste0(log_dir, "/", id, ".pomdpx")
  outfile <- paste0(log_dir, "/", id, ".policyx")
  stdout <- paste0(log_dir, "/", id, ".log")
  simout <- paste0(log_dir, "/", id, ".sim")

  sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  g = sarsop::pomdpsim(infile, outfile, simout, steps = T,
                       simulations = 1, stdout = stdout, spinner = TRUE)

  g = readChar(simout, file.info(simout)$size)
  g = strsplit(g, split = '\r\n')
  g = unlist(g)
  i = min(which(grepl('R', g)))+1
  j = which(grepl('terminated', g))-1
  st = function(c){
    char = c
    char = stringr::str_replace(char, "Y   :\\(", '')
    char = stringr::str_replace(char, 'O   :\\(', '')
    char = stringr::str_replace(char, 'ML Y:\\(', '')
    char = stringr::str_replace(char, 'A   :\\(', '')
    char = stringr::str_replace(char, 'R   :', '')
    char = stringr::str_replace(char, '\\)', '')
    return(char)
  }
  sim = unlist(lapply(g[i:j], st))
  sim=matrix(sim, ncol = 5, byrow = T)
  colnames(sim) = c('Y', 'O', 'ML Y', 'A', 'R')

  init = unlist(lapply(g[2:(i-1)], st))

  state = c(init[1], sim[,1])
  conv_state = function(asdf){
    switch (asdf, a1 = 1, a2 = 2)
  }
  state = unlist(lapply(state, conv_state))

  action = c(init[3], sim[,4])
  conv_action = function(asdf){
    switch (asdf, a1 = 1, a2 = 2, a3 = 3)
  }
  action = unlist(lapply(action, conv_action))

  obs = c(sim[,2])
  conv_obs = function(asdf){
    switch (asdf, o0 = 1, o1 = 2)
  }
  obs = unlist(lapply(obs, conv_obs))

  #calcul of belief states, extant
  state_posterior = matrix(state_prior, ncol = 2)

  for (i in c(1:(T-1))){
    a1 = action[i]
    o1 = obs[i]
    s_p <- TigerPOMDP::update_belief(state_posterior[i,], t, o, o1, a1)
    state_posterior = rbind(state_posterior,s_p)
  }

  graphics::par(mfrow = c(4, 1), mai = c(0.7, 0.6, 0.1, 0.1), cex.lab = size)

  #States
  plot1 = graphics::plot(c(0:(T-1)), state, yaxt = "n", pch = 19,
    xlab = "Time (years)", ylab = "State", ylim = c(0.9,
      2.1), xlim = c(-2, T), cex = 2)
  graphics::legend("topleft", legend = "Extinct", bty = "n", cex = size)
  graphics::legend("bottomleft", legend = "Extant", bty = "n", cex = size)
  #Actions
  plot2 = graphics::plot(c(0:(T-1)),action, yaxt = "n",
    pch = 19, xlab = "Time (years)", ylab = "Action", ylim = c(0.9,
      3.1), xlim = c(-2, T), cex = 2)
  graphics::legend("topleft", legend = "Nothing", bty = "n", cex = size)
  graphics::legend("left", legend = "Survey", bty = "n", cex = size)
  graphics::legend("bottomleft", legend = "Manage", bty = "n", cex = size)
  #Observation
  plot3 = graphics::plot(c(1:(T-1)), obs, yaxt = "n", pch = 19,
    xlab = "Time (years)", ylab = "Observation", ylim = c(0.9,
      2.1), xlim = c(-2, T), cex = 2)
  graphics::legend("topleft", legend = "Not seen", bty = "n", cex = size)
  graphics::legend("bottomleft", legend = "Seen", bty = "n", cex = size)


  plot4 = graphics::plot(c(0:(T - 1)), state_posterior[,1],
    type = "l", xlab = "Time (years)", ylab = "Probabilities",
    ylim = c(0, 1), xlim = c(-2, T) )
  graphics::lines(c(0:(T - 1)), state_posterior[,2], col = "red",
    cex = size)
  graphics::legend("bottomleft", legend = c("Extant", "Extinct"), col = c("black",
    "red"), lty = 1, bty = "n", cex = size)
}
