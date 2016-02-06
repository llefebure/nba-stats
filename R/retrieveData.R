#' Retrieves a data frame containing info for a player.
#'
#' @param playerID, ID of player you want to retrieve
#' @return A data frame containing info on the player with the specified player_id
#' @export
#' @examples
#' getPlayerData(list(playerID=201939))
getPlayerData <- function(params){
  url <- buildURL("commonplayerinfo", params)
  data <- RJSONIO::fromJSON(url, nullValue=NA)
  header <- data$resultSets[[1]]$headers
  values <- unlist(data$resultSets[[1]]$rowSet)
  data.frame(Attribute = header, Value = values)
}

#' Retrieves a generic data source
#'
#' @param resource, endpoint to retrieve as a character vector
#' @param params, list of parameters
#' @return A data frame containing info on the player with the specified player_id
#' @export
#' @examples
#' getGenericData("commonallplayers", list(IsOnlyCurrentSeason = 0, LeagueID="00", Season="2015-16"))
getGenericData <- function(resource, params){
  url <- buildURL(resource, params)
  data <- RJSONIO::fromJSON(url, nullValue=NA)
  header <- data$resultSets[[1]]$headers
  values <- lapply(data$resultSets[[1]]$rowSet, unlist)
  df <- data.frame(t(sapply(values, unlist)))
  colnames(df) <- header
  df
}

#' Builds a URL for requesting a resource.
#'
#' @param resource, endpoint to retrieve as a character vector
#' @param params, list of parameters
#' @return A full URL for retrieving the specified data.
#' @examples
#' buildURL("commonplayerinfo", list(playerID = 201939))
buildURL <- function(resource, params){
  base <- "http://stats.nba.com/stats/"
  param.list <- paste(paste(names(params), unlist(params), sep = "="), collapse = "&")
  paste0(base, resource, "?", param.list)
}
