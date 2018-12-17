#' @export
rew = function(p0, pm, d0, d, V, Cm, Cs){
  R = t(matrix(c(V-Cm,V-Cs, V, -Cm, -Cs, 0), nrow = 3))
  return(R)
}
