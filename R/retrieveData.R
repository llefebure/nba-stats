#' Retrieves a data frame containing info for a player (this is just an example.. trying to learn how packages work).
#'
#' @param playerID, ID of player you want to retrieve
#' @return A data frame containing info on the player with the specified player_id
#' @export
#' @examples
#' getPlayerData(201939)
getPlayerData <- function(playerID){
  url <- paste0("http://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=", playerID, "&SeasonType=Regular+Season")
  data <- RJSONIO::fromJSON(url, nullValue=NA)
  header <- data$resultSets[[1]]$headers
  values <- unlist(data$resultSets[[1]]$rowSet)
  data.frame(Attribute = header, Value = values)
}