#' @export
rew <- function(p0, pm, d0, d, V, Cm, Cs){
  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(d>=0,d<=1) #checks if d is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif

  R <- t(matrix(c(V-Cm,V-Cs, V, -Cm, -Cs, 0), nrow = 3))
  return(R)
}
