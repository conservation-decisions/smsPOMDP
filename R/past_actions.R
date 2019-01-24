#' @export
past_actions=function(input)
{
  names = names(input)
  submit_couples = names[which(grepl('submit_couple', names))]
}
