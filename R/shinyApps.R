# Author : Vineet Ahluwalia

#' Launch an interactive user interface
#'
#' @description This function launches a shiny based application for viewing 
#' draft history or plotting a shot chart for a player. 
#' @param appName, The name of the application that you want to launch. 
#' Valid options are: "shotcharts", "draft"
#' @export
launchApp <- function(appName = 'shotcharts') {
  shiny::runApp(system.file(appName, package = "rNBA"))
}
