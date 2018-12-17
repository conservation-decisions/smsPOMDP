#' @export
obs = function(p0, pm, d0, d, V, Cm, Cs){
  manage = nothing = matrix(c(d0,0,1-d0,1), nrow = 2)
  survey = matrix(c(d,0,1-d,1), nrow = 2)
  o = array(c(manage, survey, nothing), dim = c(2,2,3))
  return(o)
}
