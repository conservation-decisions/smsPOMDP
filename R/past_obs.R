#' @export
past_obs=function(input)
{
  depth = input$length_past
  tablo = c()
  for (i in c(1:depth)){
    tablo = c(tablo,input[[paste0('submit_couple_', i)]])
  }
  if (0 %in% tablo){
    id = which.min(tablo) - 1
  } else {
    id = depth
  }

  obs = c()
  for (i in c(1:id)){
    obs = c(obs, input[[paste0('past_obs_', i)]])
  }
  return(obs)
}
