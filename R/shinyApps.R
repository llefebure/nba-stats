# Author : Vineet Ahluwalia

#' Launch a UI based application to view or interaction
#'
#' @description This function launch a shiny based application for interactive play. 
#' @param appName, Application Name
#' @export
launchApp <- function(appName = 'rNBAshinyTestApp') {
  shiny::runApp(system.file(appName, package = "rNBA"))
}
