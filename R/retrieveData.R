# Author: Luke Lefebure

#' Retrieve a generic data source
#'
#' @description This function retrieves the specified data source. Use \code{\link{searchEndpoints}}
#' to see valid options for the endpoint parameter (these are the available datasets) and 
#' \code{\link{getEndpointParams}} to learn what parameters are needed to pass in as the
#' params argument for the chosen endpoint.
#' @param endpoint, endpoint to retrieve as a character vector
#' @param params, list of parameters
#' @return A data frame or a list of data frames (if the endpoint returns multiple 
#' datasets) containing the requested data.
#' @export
#' @examples
#' getGenericData("commonallplayers", list(IsOnlyCurrentSeason = 0, LeagueID = "00", Season = "2015-16"))
#' getGenericData("playergamelog", list(PlayerID = 201939, Season = "2015-16", SeasonType = "Regular+Season"))
getGenericData <- function(endpoint, params = list()){
  url <- buildGenericURL(endpoint, params)
  data <- jsonlite::fromJSON(url)
  return(jsonToDF(data))
}

#' Convert JSON response into data frame or list of data frames
#'
#' @description Converts the JSON response to a data frame or a list of data frames (if the
#' JSON has multiple row sets in it).
#' @param data, JSON resulting from a call to jsonlite::fromJSON
#' @return A data frame or list of data frames containing the JSON data.
jsonToDF <- function(data) {
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

#' Builds a URL for requesting a generic endpoint
#'
#' @description This function is called by \code{\link{getGenericData}} to build the 
#' appropriate URL for retrieving data. 
#' @param endpoint, endpoint to retrieve as a character vector
#' @param params, list of parameters
#' @return A full URL for retrieving the specified data.
#' @examples
#' buildGenericURL("commonplayerinfo", list(playerID = 201939))
buildGenericURL <- function(endpoint, params = list()){
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

#' Retrieve SportVU Player Tracking Data
#'
#' @description This function retrieves Player Tracking stats (see the "Player Tracking" tab on 
#' \link{http://stats.nba.com/}).
#' @param year, the season from which to retrieve data. 2014 refers to the 2014-15 season, 2015 refers to 
#' the 2015-16 season, etc. The earliest available year for this data is 2013 (the 2013-14 season).
#' @param type, a character vector containing one or more type of data to retrieve. The valid types are
#' "catchShootData", "defenseData", "drivesData", "passingData", "touchesData", "pullUpShootData", 
#' "reboundingData", "shootingData", or "speedData". The default is all of them.
#' @return A data frame or list of data frames (if multiple types specified) containing the requested data.
#' @export
#' @examples
#' getPlayerTrackingData(year = 2014)
#' getPlayerTrackingData(year = 2015, "defenseData")
getPlayerTrackingData <- function(year,
                                  type = c("catchShootData", "defenseData", "drivesData", 
                                           "passingData", "touchesData", "pullUpShootData", 
                                           "reboundingData", "shootingData", "speedData")){
  if (year < 2013) {
    stop("Data is only available from 2013 onwards.")
  }
  res <- lapply(type, function(t) {
    url <- paste0("http://stats.nba.com/js/data/sportvu/", year, "/", t, ".json")
    data <- jsonlite::fromJSON(url)
    df <- jsonToDF(data)
    df$SEASON <- paste0(y, "-", (y + 1) %% 2000)
    df
  })
  names(res) <- type
  if (length(res) == 1) {
    return(res[[1]])
  } else {
    return(res)
  }
}