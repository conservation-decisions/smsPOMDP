#' @export
obs = function(p0, pm, d0, d, V, Cm, Cs){
  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(d>=0,d<=1) #checks if d is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif

  manage = nothing = matrix(c(d0,0,1-d0,1), nrow = 2)
  survey = matrix(c(d,0,1-d,1), nrow = 2)
  o = array(c(manage, survey, nothing), dim = c(2,2,3))
  return(o)
}
