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
#' player.info.endpoint <- "commonplayerinfo"
#' player.info.params <-list(PlayerID=201939,IsOnlyCurrentSeason=0,LeagueID="00",Season="2015-16")
#' player.info <- getGenericData(player.info.endpoint, player.info.params)
#' str(player.info)
#' ######
#' game.log.endpoint <- "playergamelog"
#' game.log.params <- list(PlayerID = 201939, Season = "2015-16", SeasonType = "Regular+Season")
#' game.log <- getGenericData(game.log.endpoint, game.log.params)
#' str(game.log)
getGenericData <- function(endpoint, params = list()){
  if (!(endpoint %in% endpoints$Endpoint)) {
    stop("Invalid Endpoint")
  }
  tryCatch({
    url <- buildGenericURL(endpoint, params)
    data <- jsonlite::fromJSON(url)
    return(jsonToDF(data))
  }, error = function(e) {
    cat(paste("Request failed. Make sure you passed in the parameters",
              "correctly (if applicable) and that you are connected to the internet!"))
    closeAllConnections()
    }
  )
}

# Converts the JSON response to a data frame or a list of data frames (if the
# JSON has multiple row sets in it).
#
# @param data, JSON resulting from a call to jsonlite::fromJSON
# @return A data frame or list of data frames containing the JSON data.
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

# This function is called by getGenericData to build the appropriate 
# URL for retrieving data. 
#
# @param endpoint, endpoint to retrieve as a character vector
# @param params, list of parameters
# @return A full URL for retrieving the specified data.
buildGenericURL <- function(endpoint, params = list()){
  base <- "http://stats.nba.com/stats/"
  param.list <- paste(paste(names(params), unlist(params), sep = "="), collapse = "&")
  url <- paste0(base, endpoint)
  if (length(params) > 0) url <- paste0(url, "?", param.list)  
  url
}

#' Search for endpoints that can be passed to \code{\link{getGenericData}}
#'
#' @description This function lets you search for available endpoints. Use \code{\link{getEndpointParams}}
#' to get the required parameters for a specified endpoint. Once you have an endpoint and a parameter list,
#' a call can be made to \code{\link{getGenericData}}.
#' @param pattern, regex pattern to search for (see \link{regex})
#' @return A character vector containing all available and matching endpoints.
#' @export
#' @examples
#' all.endpoints <- searchEndpoints()
#' head(all.endpoints)
#' ######
#' player.endpoints <- searchEndpoints("player")
#' head(player.endpoints)
searchEndpoints <- function(pattern = "."){
  ind <- grep(pattern, endpoints$Endpoint, ignore.case = TRUE)
  endpoints$Endpoint[ind]
}

#' Get endpoint parameters to pass to \code{\link{getGenericData}}
#'
#' @description This function returns the expected parameters for a specified endpoint. Use \code{\link{searchEndpoints}}
#' to search the available endpoints. Once you have an endpoint and a parameter list, a call can be made to 
#' \code{\link{getGenericData}}.
#' @param endpoint, a character vector containing an endpoint
#' @return A character vector describing the required parameters.
#' @export
#' @examples
#' params <- getEndpointParams("commonplayerinfo")
getEndpointParams <- function(endpoint){
  if (length(endpoint) != 1) stop("Must pass in only one endpoint")
  if (!(endpoint %in% endpoints$Endpoint)) stop("Endpoint doesn't exist")
  endpoints$Parameter[endpoints$Endpoint == endpoint]
}

#' Retrieve SportVU Player Tracking Data
#'
#' @description This function retrieves Player Tracking stats (see the "Player Tracking" tab on 
#' \url{http://stats.nba.com/}.
#' @param year, the season from which to retrieve data. 2014 refers to the 2014-15 season, 2015 refers to 
#' the 2015-16 season, etc. The earliest available year for this data is 2013 (the 2013-14 season).
#' @param type, a character vector containing one or more type of data to retrieve. The valid types are
#' "catchShootData", "defenseData", "drivesData", "passingData", "touchesData", "pullUpShootData", 
#' "reboundingData", "shootingData", or "speedData". The default is all of them.
#' @return A data frame or list of data frames (if multiple types specified) containing the requested data.
#' @export
#' @examples
#' pt.data <- getPlayerTrackingData(year = 2014, c("defenseData", "catchShootData"))
#' str(pt.data)
getPlayerTrackingData <- function(year, type = NULL){
  all.types <- c("catchShootData", "defenseData", "drivesData", 
                 "passingData", "touchesData", "pullUpShootData", 
                 "reboundingData", "shootingData", "speedData")
  if (year < 2013) {
    stop("Data is only available from 2013 onwards.")
  }
  if (is.null(type)) {
    type <- all.types
  }
  if (!all(sapply(type, function(t) t %in% all.types))) {
    stop("Invalid type specified.")
  }
  tryCatch({
    res <- lapply(type, function(t) {
      url <- paste0("http://stats.nba.com/js/data/sportvu/", year, "/", t, ".json")
      data <- jsonlite::fromJSON(url)
      df <- jsonToDF(data)
      df$SEASON <- paste(year, (year + 1) %% 2000, sep = "-")
      df
    })
    names(res) <- type
    if (length(res) == 1) {
      return(res[[1]])
    } else {
      return(res)
    }
  }, error = function(e) {
    cat("One or more requests failed. Make sure you are connected to the internet!")
    closeAllConnections()
    }
  )
}

#' Get mappings from player name and team name to PlayerID and TeamID
#' 
#' @description For many endpoints, PlayerID or TeamID are required parameters. This function
#' retrieves a mapping from player name to PlayerID for all players in NBA history and team name 
#' to TeamID for all current teams. These ID's are used as parameters to pass in to 
#' \code{\link{getGenericData}} for several endpoints. If you only need to search for a few specific
#' players or teams, use \code{\link{searchIDMappings}} instead as that function is just a convenient
#' way to search this mapping.
#' @return A list of length two containing data frames with the mappings.
#' @export
#' @examples 
#' m <- getIDMappings()
#' str(m)
getIDMappings <- function() {
  # grab the most up to date mappings: if the current year is 2016 for example, check to see
  # if 2016-17 data has been updated yet, and if not, grab 2015-16 data.
  currYear <- as.numeric(format(Sys.Date(), "%Y"))
  mostRecentSeason <- paste(currYear, (currYear + 1) %% 2000, sep = "-")
  df <- getGenericData("commonallplayers", list(IsOnlyCurrentSeason = 0, LeagueID = "00", Season = mostRecentSeason))
  if (length(unique(df$TEAM_ID)) == 1) {
    mostRecentSeason <- paste(currYear - 1, currYear %% 2000, sep = "-")
    df <- getGenericData("commonallplayers", list(IsOnlyCurrentSeason = 0, LeagueID = "00", Season = mostRecentSeason))
  }
  player <- data.frame(PLAYER_ID = df$PERSON_ID, PLAYER_NAME = df$DISPLAY_FIRST_LAST,
                       FROM_YEAR = df$FROM_YEAR, TO_YEAR = df$TO_YEAR, ROSTERSTATUS = df$ROSTERSTATUS,
                       stringsAsFactors = FALSE)
  team <- data.frame(unique(df[df$TEAM_ID != 0, c("TEAM_ID", "TEAM_CITY", "TEAM_NAME", "TEAM_ABBREVIATION", "TEAM_CODE")]),
                     stringsAsFactors = FALSE)
  rownames(team) <- NULL
  list(player = player, team = team)
}

#' Search ID mappings
#'
#' @description For many endpoints, PlayerID or TeamID are required parameters. This function
#' searches the mapping returned by \code{\link{getIDMappings}} for a specified player or team,
#' and it returns the matches. These ID's are used as parameters to pass in to 
#' \code{\link{getGenericData}} for several endpoints.
#' @param player, player name to search for as a regex pattern (see \link{regex})
#' @param team, team name to search for as a regex pattern (see \link{regex})
#' @param active, flag indicating that only active players should be searched
#' @return Returns a list containing one or two (depending on which of player and team is specified) 
#' data frame(s) containing matches.
#' @export
#' @examples
#' searchIDMappings(player = "curry")
#' searchIDMappings(player = "curry", team = "golden state")
searchIDMappings <- function(player = NA, team = NA, active = TRUE) {
  mapping <- memGetIDMappings()
  if (missing(player) && missing(team)) {
    stop("Must specify either player or team.")
  }
  rv <- list()
  if (!is.na(player)) {
    if (active) {
      mapping$player <- mapping$player[mapping$player$ROSTERSTATUS == 1,]
    }
    rv$player <- mapping$player[grep(player, mapping$player$PLAYER_NAME, ignore.case = TRUE),]
  }
  if (!is.na(team)) {
    rv$team <- mapping$team[grep(team, paste(mapping$team$TEAM_CITY, mapping$team$TEAM_NAME), 
                                 ignore.case = TRUE),]
  }
  rv
}

# Memoised version of retrieval functions for internal use
# Need to use importFrom rather than memoise::memoise because otherwise NOTE
# is generated.
#' @importFrom memoise memoise
memGetGenericData <- memoise(getGenericData)
memGetIDMappings <- memoise(getIDMappings)