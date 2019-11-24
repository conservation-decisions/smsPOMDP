#' @export
past_actions<-function(input)
{
  depth <- input$length_past
  tablo <- c()
  for (i in seq_len(depth)){
    tablo <- c(tablo,input[[paste0('submit_couple_', i)]])
  }
  if (0 %in% tablo){
      id <- which.min(tablo) - 1
  } else {
    id <- depth
  }

  act <- c()
  for (i in seq_len(id)){
    act <- c(act, input[[paste0('past_action_', i)]])
  }
  return(act)
}
