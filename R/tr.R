#' @export
tr <- function(p0, pm, d0, dm, ds, V, Cm, Cs){
  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(dm>=0,dm<=1) #checks if dm is a probability
  stopifnot(ds>=0,ds<=1) #checks if ds is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif

  manage <- matrix(c(pm,0,1-pm,1), nrow = 2)
  survey <- matrix(c(p0,0,1-p0,1), nrow=2)
  nothing <- matrix(c(p0,0,1-p0,1), nrow=2)
  t <- array(c(manage, survey, nothing), dim = c(2,2,3))
  return(t)
}
