#' @export
sim = function(t, o, r, discount=0.95, state_prior, Tmax, a){
  list.of.packages <- c("sarsop")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) {
    devtools::install_github("boettiger-lab/sarsop")
  }
  library(sarsop)
  library(ggplot2)
  library(ggpubr)
  library(tidyr)

  alpha = sarsop(t, o, r, discount, state_prior)
  x0 = 1
  a0 = switch(a, 'Manage' = 1, 'Survey' = 2, 'Nothing' = 3)
  sim <- sim_pomdp(t, o, r, discount, state_prior = state_prior,
                   x0 = x0, a0 = a0, Tmax = Tmax, alpha = alpha)

  par(mfrow = c(4,1), mai = c(0.7, 0.6, 0.1, 0.1))
  plot1 = plot(sim$df$time, sim$df$state, yaxt='n', xlab = 'Time (years)',ylab = 'State', ylim = c(0.9,2.1), xlim = c(0,20))
  legend(-0.8,2.15,legend = 'Extinct', cex = 0.7,  bty = "n")
  legend(-0.8,1.15,legend = 'Extant', cex = 0.7,  bty = "n")
  
  plot2 =plot(sim$df$time, sim$df$action, yaxt='n', xlab = 'Time (years)',ylab = 'Action', ylim = c(0.9,3.1), xlim = c(0,20))
  legend(-0.8,3.25,legend = 'Nothing', cex = 0.7,  bty = "n")
  legend(-0.8,2.25,legend = 'Survey', cex = 0.7,  bty = "n")
  legend(-0.8,1.25,legend = 'Manage', cex = 0.7,  bty = "n")
  
  plot3 =plot(sim$df$time, sim$df$obs, yaxt='n', xlab = 'Time (years)',ylab = 'Observation', ylim = c(0.9,2.1), xlim = c(0,20) )
  legend(-0.8,2.15,legend = 'Not seen', cex = 0.7,  bty = "n")
  legend(-0.8,1.15,legend = 'Seen', cex = 0.7,  bty = "n")
  
  sim$state_posterior = as.data.frame(sim$state_posterior)
  names(sim$state_posterior) = c('extant','extinct')
  plot4 = plot(sim$state_posterior$extant, type = 'l', xlab = 'Time (years)',ylab = 'Probabilities', ylim = c(0,1), xlim = c(0,20))
  lines(sim$state_posterior$extinct, col = 'red')
  legend(-0.8,1,legend = c('Extant','Extinct'), col = c('black', 'red'), lty = 1, cex = 0.7,  bty = "n")
}
