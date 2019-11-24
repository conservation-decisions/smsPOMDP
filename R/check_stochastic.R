#' @export
check_stochastic <- function(X){
  #X is a matrix
  #returns a bool
  #T if x is a stochastic matrix
  #F otherwise
  error_msg <- TRUE
  s1 <- dim(X)[1]
  s2 <- dim(X)[2]
 if (max(abs(rowSums(X) - rep(1, s2))) > 10^(-12)) {
    error_msg <-FALSE
  }
  else if (length(which(X < 0)) > 0) {
    error_msg <-FALSE
  }
  return(error_msg)
}
