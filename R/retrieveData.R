# Author: Luke Lefebure

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

#' Retrieve a generic data source
#'
#' @description Use \code{\link{searchEndpoints}} and \code{\link{getEndpointParams}} to learn
#' what arguments to pass into this function.
#' @param endpoint, endpoint to retrieve as a character vector
#' @param params, list of parameters
#' @return A data frame or a list of data frames containing the requested data.
#' @export
#' @examples
#' getGenericData("commonallplayers", list(IsOnlyCurrentSeason = 0, LeagueID="00", Season="2015-16"))
getGenericData <- function(endpoint, params = list()){
  url <- buildURL(endpoint, params)
  data <- jsonlite::fromJSON(url)
  headers <- data$resultSets$headers
  values <- data$resultSets$rowSet
  res <- lapply(1:length(headers), function(i){
    df <- data.frame(values[[i]], stringsAsFactors = FALSE)
    if (length(df) > 0){
      colnames(df) <- headers[[i]]
      return(df)
    } else {
      return(NA)
    }
  })
  res <- res[!is.na(res)]
  if (length(res) == 1){
    return(res[[1]])
  } else {
    return(res)
  }
}

#' Builds a URL for requesting a resource.
#'
#' @param endpoint, endpoint to retrieve as a character vector
#' @param params, list of parameters
#' @return A full URL for retrieving the specified data.
#' @examples
#' buildURL("commonplayerinfo", list(playerID = 201939))
buildURL <- function(endpoint, params = list()){
  base <- "http://stats.nba.com/stats/"
  param.list <- paste(paste(names(params), unlist(params), sep = "="), collapse = "&")
  url <- paste0(base, endpoint)
  if (length(params) > 0) url <- paste0(url, "?", param.list)  
  url
}

#' Search raw endpoints that can be passed to \code{\link{getGenericData}}.
#'
#' @description This function lets you search for available endpoints. Use \code{\link{getEndpointParams}}
#' to get the required parameters for a specified endpoint. Once you have an endpoint and a parameter list,
#' a call can be made to \code{\link{getGenericData}}.
#' @param pattern, regex pattern to search for (see \link{regex})
#' @return A character vector containing all available and matching endpoints.
#' @export
#' @examples
#' searchEndpoints(".")
#' searchEndpoints("player")
searchEndpoints <- function(pattern){
  ind <- grep(pattern, endpoints$Endpoint, ignore.case = TRUE)
  endpoints$Endpoint[ind]
}

#' Get raw endpoint parameters to pass to \code{\link{getGenericData}}.
#'
#' @description This function returns the expected parameters for a specified endpoint. Use \code{\link{searchEndpoints}}
#' to search the available endpoints. Once you have an endpoint and a parameter list, a call can be made to 
#' \code{\link{getGenericData}}.
#' @param endpoint, a character vector containing an endpoint
#' @return A character vector describing the required parameters.
#' @export
#' @examples
#' getEndpointParams("commonplayerinfo")
getEndpointParams <- function(endpoint){
  if (length(endpoint) != 1) stop("Must pass in only one endpoint")
  if (!(endpoint %in% endpoints$Endpoint)) stop("Endpoint doesn't exist")
  endpoints$Parameter[endpoints$Endpoint == endpoint]
}