#' @export
sim = function(t, o, r, state_prior, Tmax, a, discount=0.95){
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

  plot1 = sim$df %>%
    ggplot(aes(time, state, color = state)) + geom_line()  + geom_point() + xlim(1,Tmax+1)

  plot2 = sim$df %>%
    ggplot(aes(time, action, color = action)) + geom_line()  + geom_point() + xlim(1,Tmax+1)

  plot3 = sim$df %>%
    ggplot(aes(time, obs, color = obs)) + geom_line()  + geom_point() + xlim(1,Tmax+1)

  sim$state_posterior = as.data.frame(sim$state_posterior)
  names(sim$state_posterior) = c('extant','extinct')
  plot4 = sim$state_posterior %>%
    data.frame(time = 1:(Tmax)) %>%
    gather(variable, prob, -time) %>%
    ggplot(aes(time, prob, color = variable)) + geom_line()  + geom_point()+ xlim(1,Tmax)

  ggarrange(plot1,
            plot2,
            plot3,
            plot4,
            nrow = 4)
}
