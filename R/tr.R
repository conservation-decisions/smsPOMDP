#' @export
tr = function(p0, pm, d0, d, V, Cm, Cs){
  pem = 1-pm
  pen = 1-p0
  manage = matrix(c(pm,0,pem,1), nrow = 2)
  survey = nothing = matrix(c(p0,0,pen,1), nrow=2)
  t = array(c(manage, survey, nothing), dim = c(2,2,3))
  return(t)
}
