#' @export
run_application <- function(){
  appdir <- system.file('app', package = 'smsPOMDP')
  
  shiny::shinyAppDir(appdir)
}
