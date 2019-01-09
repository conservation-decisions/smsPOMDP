#' @export
sim=function(p0, pm, d0, d, V, Cm, Cs, state_prior, Tmax, a = c('Manage', 'Survey', 'Nothing'), discount = 0.95, size = 1)
{
  #checking presence of sarsop package
  list.of.packages <- c("sarsop")
  new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
  if(length(new.packages)>0) {
    devtools::install_github("boettiger-lab/sarsop")
  }

  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(d>=0,d<=1) #checks if d is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif
  stopifnot(discount>=0, discount <= 1) #checks if values and costs are positif
  stopifnot(Tmax >0) #positive horizon

  #buiding the matrices of the problem
  t = TigerTest::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  o = TigerTest::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
  r = TigerTest::rew(p0, pm, d0, d, V, Cm, Cs) #reward matrix

  alpha = sarsop::sarsop(t, o, r, discount, state_prior)
  x0 = 1
  a0 = switch(a, Manage = 1, Survey = 2, Nothing = 3)
  sim <- sarsop::sim_pomdp(t, o, r, discount, state_prior = state_prior,
    x0 = x0, a0 = a0, Tmax = Tmax, alpha = alpha)
  par(mfrow = c(4, 1), mai = c(0.7, 0.6, 0.1, 0.1), cex.lab = size)
  plot1 = plot(sim$df$time, sim$df$state, yaxt = "n", pch = 19,
    xlab = "Time (years)", ylab = "State", ylim = c(0.9,
      2.1), xlim = c(-2, Tmax), cex = 2)
  legend("topleft", legend = "Extinct", bty = "n", cex = size)
  legend("bottomleft", legend = "Extant", bty = "n", cex = size)
  plot2 = plot(c(0, sim$df$time), c(a0, sim$df$action), yaxt = "n",
    pch = 19, xlab = "Time (years)", ylab = "Action", ylim = c(0.9,
      3.1), xlim = c(-2, Tmax), cex = 2)
  legend("topleft", legend = "Nothing", bty = "n", cex = size)
  legend("left", legend = "Survey", bty = "n", cex = size)
  legend("bottomleft", legend = "Manage", bty = "n", cex = size)
  plot3 = plot(sim$df$time, sim$df$obs, yaxt = "n", pch = 19,
    xlab = "Time (years)", ylab = "Observation", ylim = c(0.9,
      2.1), xlim = c(-2, Tmax), cex = 2)
  legend("topleft", legend = "Not seen", bty = "n", cex = size)
  legend("bottomleft", legend = "Seen", bty = "n", cex = size)
  sim$state_posterior = as.data.frame(sim$state_posterior)
  names(sim$state_posterior) = c("extant", "extinct")
  plot4 = plot(c(0:(Tmax - 1)), sim$state_posterior$extant,
    type = "l", xlab = "Time (years)", ylab = "Probabilities",
    ylim = c(0, 1), xlim = c(-2, Tmax) )
  lines(c(0:(Tmax - 1)), sim$state_posterior$extinct, col = "red",
    cex = size)
  legend("bottomleft", legend = c("Extant", "Extinct"), col = c("black",
    "red"), lty = 1, bty = "n", cex = size)
}
