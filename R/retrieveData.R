#' Retrieves a data frame containing info for a player.
#'
#' @param playerID, ID of player you want to retrieve
#' @return A list of data frames containing info on the player with the specified player_id.
#' @export
#' @examples
#' getPlayerData(playerID = 201939)
getPlayerData <- function(playerID){
  getGenericData("commonplayerinfo", list(playerID = playerID))
}

#' Retrieves a generic data source
#'
#' @param resource, endpoint to retrieve as a character vector
#' @param params, list of parameters
#' @return A data frame or a list of data frames containing the requested data.
#' @export
#' @examples
#' getGenericData("commonallplayers", list(IsOnlyCurrentSeason = 0, LeagueID="00", Season="2015-16"))
getGenericData <- function(resource, params = list()){
  url <- buildURL(resource, params)
  data <- jsonlite::fromJSON(url)
  headers <- data$resultSets$headers
  values <- data$resultSets$rowSet
  res <- lapply(1:length(headers), function(i) {
    df <- data.frame(values[[i]])
    colnames(df) <- headers[[i]]
    df
  })
  if (length(res) == 1){
    return(res[[1]])
  } else {
    return(res)
  }
}

#' Builds a URL for requesting a resource.
#'
#' @param resource, endpoint to retrieve as a character vector
#' @param params, list of parameters
#' @return A full URL for retrieving the specified data.
#' @examples
#' buildURL("commonplayerinfo", list(playerID = 201939))
buildURL <- function(resource, params = list()){
  base <- "http://stats.nba.com/stats/"
  param.list <- paste(paste(names(params), unlist(params), sep = "="), collapse = "&")
  url <- paste0(base, resource)
  if (length(params) > 0) url <- paste0(url, "?", param.list)  
  url
}