#' @export
check_square_stochastic = function(X){
  #X is a matrix
  #returns a bool
  #T if x is a square stochastic matrix
  #F otherwise
  error_msg = T
  s1 <- dim(X)[1]
  s2 <- dim(X)[2]
  if (s1 != s2) {
    error_msg <-F
  }
  else if (max(abs(rowSums(X) - rep(1, s2))) > 10^(-12)) {
    error_msg <-F
  }
  else if (length(which(X < 0)) > 0) {
    error_msg <-F
  }
  return(error_msg)
}