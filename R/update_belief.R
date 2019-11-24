#' @export
update_belief <- function(state_prior, transition, observation, z0, a0){
  L <- length(state_prior)
  belief <-
    vapply(seq_len(L), function(i){
      state_prior %*% transition[, i, a0] * observation[i, z0, a0]
    }, numeric(1))
  belief / sum(belief)
}
