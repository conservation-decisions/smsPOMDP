#' @export
update_belief <- function(state_prior, transition, observation, z0, a0){
  belief <-
    vapply(1:length(state_prior), function(i){
      state_prior %*% transition[, i, a0] * observation[i, z0, a0]
    }, numeric(1))
  belief / sum(belief)
}
