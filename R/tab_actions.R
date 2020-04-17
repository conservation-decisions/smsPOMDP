#' @export
tab_actions <- function(transition, observation, reward, state_prior, disc = 0.95, Tmax = 100){
  stopifnot(smsPOMDP::check_pomdp(transition, observation, reward))

  log_dir <- tempdir()
  id <- digest::digest(match.call())
  infile <- paste0(log_dir, "/", id, ".pomdpx")
  outfile <- paste0(log_dir, "/", id, ".policyx")
  stdout <- paste0(log_dir, "/", id, ".log")

  sarsop::write_pomdpx(transition, observation, reward, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  policy <- sarsop::read_policyx(file = outfile)
  output <- smsPOMDP::interp_policy(state_prior,policy$vectors,policy$action)

  state_posterior <- matrix(state_prior, ncol = 2)
  optimal_action <- output[[2]]
  
  for (i in seq_len(Tmax)) {
    a1 <- optimal_action[i]
    o1 <- 2 #we treat the case when the species is not seen
    s_p <- smsPOMDP::update_belief(state_posterior[i, ], transition, observation, o1,
                                     a1)
    state_posterior <- rbind(state_posterior, s_p)
    output <- smsPOMDP::interp_policy(s_p,policy$vectors,policy$action)

    optimal_action <- c(optimal_action, output[[2]])
  }

  a <- optimal_action[1]
  act <- a
  years <- numeric()
  while(sum(years) < length(optimal_action)){
    if (length(unique(optimal_action))==1){
      i <- length(optimal_action)
      years <- c(years, i)
      break
    } else {
      i <- min(which(optimal_action!= a))-1
      optimal_action <- optimal_action[-c(1:i)]
      a <- optimal_action[1]
      act <- c(act,a)
      years <- c(years, i)
    }
  }
  tab <- data.frame(action = act, years = years)
  return(tab)
}
