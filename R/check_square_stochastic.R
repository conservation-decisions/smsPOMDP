#' @export
check_square_stochastic <- function(X){
  #X is a matrix
  #returns a bool
  #TRUE if x is a square stochastic matrix
  #FALSE otherwise
  error_msg <- TRUE
  s1 <- dim(X)[1]
  s2 <- dim(X)[2]
  if (s1 != s2) {
    error_msg <-FALSE
  }
  else if (max(abs(rowSums(X) - rep(1, s2))) > 10^(-12)) {
    error_msg <-FALSE
  }
  else if (length(which(X < 0)) > 0) {
    error_msg <-FALSE
  }
  return(error_msg)
}
