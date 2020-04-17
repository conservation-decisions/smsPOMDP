#' @export
interp_policy <- function(state_prior, alpha, alpha_action){
  ## Compute dot product with initial
  a <- state_prior %*% alpha
  
  ## Return policy of the vector which has the biggest inner product
  #alpha_action[which.max(a)]
  if(sum(a==0)==length(a)){
    output <- list(0,1)
  } else{
    output <- list(max(a),alpha_action[which.max(a)])
  }
}
