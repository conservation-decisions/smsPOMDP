#' @export
check_stochastic = function(X){
  error_msg = T
  s1 <- dim(X)[1]
  s2 <- dim(X)[2]
 if (max(abs(rowSums(X) - rep(1, s2))) > 10^(-12)) {
    error_msg <-F
  }
  else if (length(which(X < 0)) > 0) {
    error_msg <-F
  }
  return(error_msg)
}
