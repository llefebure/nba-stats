# Author: Luke Lefebure

if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("stephCurryShots"))
}

#' Get the coordinates of lines on the court
#' 
#' @description This function gets the coordinates of the lines on the court (sideline, 
#' baseline, three point line, etc.) for building custom shot charts. They are expressed
#' in the same coordinates as those returned by the "shotchartdetail" endpoint.
#' @return A data frame containing coordinates of line segments that, when connected,
#' draw the lines of an NBA court. The type column specifies which segment the point
#' belongs to, and the ltype column specifies whether that line segment should be solid
#' or dashed. The coordinates are in units of feet times 10 with the origin at the
#' center of the hoop, the same as that returned by the "shotchartdetail" endpoint.
#' @references See \url{http://www.sportsknowhow.com/basketball/dimensions/nba-basketball-court-dimensions.html}
#' for more information about how an NBA court is laid out.
#' @export
#' @examples
#' ## plot court using base graphics
#' court <- courtOutline()
#' plot(x = NULL, xlim = c(-275, 275), ylim = c(-80, 450), xaxt = "n", yaxt = "n", ann = FALSE)
#' for (nm in unique(court$type)) {
#'    s <- court$type == nm
#'    points(x = court$x[s], y = court$y[s], type = "l", lty = court$ltype[s])
#' }
courtOutline <- function() {
  precision <- .05 # this controls distance between points when plotting arcs
  
  # court dimensions
  court.width <- 50
  court.length <- 94
  backboard.width <- 6
  backboard.offset <- 4 # offset from baseline
  hoop.radius <- .75
  hoop.offset <- .5 # offset from backboard
  key.length <- 19
  key.width <- 16
  ft.arc.radius <- 6
  three.baseline.width <- 44
  three.straight.len <- 14
  three.arc.radius <- 23.75
  restricted.radius <- 4
  
  # sideline and baseline
  side.base <- data.frame(x = c(rep(-court.width/2, 2), rep(court.width/2, 2)),
                          y = c(court.length/2, 0, 0, court.length/2),
                          type = "Sideline / Baseline",
                          ltype = "solid")
  
  # key
  key <- data.frame(x = c(rep(-key.width/2, 2), rep(key.width/2, 2)), 
                    y = c(0, rep(key.length, 2), 0),
                    type = "Key",
                    ltype = "solid")
  
  # upper free throw circle
  ft.circle.x <- seq(from = -ft.arc.radius, to = ft.arc.radius, by = precision)
  ft.circle.upper <- data.frame(x = ft.circle.x, 
                                y = sqrt(ft.arc.radius**2 - ft.circle.x**2) + key.length,
                                type = "Free Throw Circle (Upper)",
                                ltype = "solid")
  
  # lower free throw circle
  ft.circle.lower <- data.frame(x = ft.circle.x, 
                                y = -sqrt(ft.arc.radius**2 - ft.circle.x**2) + key.length,
                                type = "Free Throw Circle (Lower)",
                                ltype = "dashed")
  
  # three point line
  three.arc.x <- seq(from = -three.baseline.width/2, to = three.baseline.width/2, by = precision)
  three.point.line <- data.frame(x = c(rep(-three.baseline.width/2, 2), three.arc.x, rep(three.baseline.width/2, 2)),
                                 y = c(three.straight.len, 0, 
                                       sqrt(three.arc.radius**2 - three.arc.x**2) + backboard.offset + hoop.offset + hoop.radius, 
                                       three.straight.len, 0),
                                 type = "Three Point Line",
                                 ltype = "solid")
  
  # backboard
  backboard <- data.frame(x = c(-backboard.width/2, backboard.width/2),
                          y = rep(backboard.offset, 2),
                          type = "Backboard",
                          ltype = "solid")
  
  # hoop
  hoop.x <- seq(from = -hoop.radius, to = hoop.radius, by = precision)
  hoop <- data.frame(x = c(hoop.x, rev(hoop.x)),
                     y = c(sqrt(hoop.radius**2 - hoop.x**2) + backboard.offset + hoop.offset + hoop.radius,
                           -sqrt(hoop.radius**2 - rev(hoop.x)**2) + backboard.offset + hoop.offset + hoop.radius),
                     type = "Hoop",
                     ltype = "solid")

  # hoop connector
  hoop.connector <- data.frame(x = c(0, 0), 
                               y = c(backboard.offset, backboard.offset + hoop.offset),
                               type = "Hoop Connector",
                               ltype = "solid")
  
  # restricted arc
  restricted.x <- seq(from = -restricted.radius, to = restricted.radius, by = precision)
  restricted.arc <- data.frame(x = restricted.x,
                               y = sqrt(restricted.radius**2 - restricted.x**2) + backboard.offset + hoop.offset + hoop.radius,
                               type = "Restricted Area",
                               ltype = "solid")
  
  court.lines <- rbind(side.base, key, ft.circle.upper, ft.circle.lower, three.point.line, backboard,
                       hoop, hoop.connector, restricted.arc)
  
  # change to appropriate type
  court.lines$type <- as.character(court.lines$type)
  court.lines$ltype <- as.character(court.lines$ltype)
  
  # scale coordinates to match the format from the web
  court.lines$x <- court.lines$x*10
  court.lines$y <- 10*(court.lines$y - hoop.radius - hoop.offset - backboard.offset)
  
  court.lines
}

#' Plot the outline of the court
#'
#' @description This functions creates a ggplot object with the outline of an
#' NBA court for building custom shot charts.
#' @return A ggplot object with the outline of an NBA court.
#' @export
#' @examples
#' courtOutlinePlot()
courtOutlinePlot <- function() {
  court <- courtOutline()
  p <- ggplot() + 
    geom_path(data = court, aes_string(x = "x", y = "y", group = "type", linetype = "ltype")) +
    scale_linetype_manual(values = c(2, 1), guide = FALSE) + 
    theme(axis.title = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), panel.background = element_rect(fill = "white", colour = "white"))
  p
}

#' Build a shot chart for a player
#' 
#' @description This function builds a shot chart from either the specified data frame
#' or the specified parameters.
#' @param d, a data frame resulting from a call to getGenericData("shotchartdetail", ...)
#' @param params, a list of parameters for a call to "shotchartdetail" (see
#' getEndpointParams("shotchartdetail") for valid options). Not all need to be
#' specified as defaults will be used for unspecified parameters.
#' @param color, the attribute on which to color the data points in the shot chart
#' passed as a character vector
#' @return A list containing a ggplot object with the shot chart and a data frame containing
#' the raw data from which the shot chart was built.
#' @import ggplot2
#' @export
#' @examples
#' shotChart()
#' shotChart(params = list(PlayerID = 101106))
shotChart <- function(d = NULL, params = NULL, color = "EVENT_TYPE"){
  if (missing(d) && missing(params)) {
    d <- stephCurryShots
  } else if (missing(d)){
    # default options
    default.params <- list(SeasonType = "Regular+Season", TeamID = 0, PlayerID = 201939, 
                           GameID = "", Outcome = "", Location = "", Month = 0, 
                           SeasonSegment = "", DateFrom = "", DateTo = "", 
                           OpponentTeamID = 0, VsConference = "", VsDivision = "", 
                           Position = "", RookieYear = "", GameSegment = "", Period=0, 
                           LastNGames = 10, ContextMeasure = "FGA", Season = "2015-16")
    for (nm in names(params)) {
      default.params[[nm]] <- params[[nm]]
    }
    d <- memGetGenericData("shotchartdetail", default.params)[[1]]
    if (is.null(d)) {
      return(NULL)
    }
  }
  p <- courtOutlinePlot() +
    geom_point(data = d, aes_string(x = "as.numeric(LOC_X)", y = "as.numeric(LOC_Y)", color = color)) +
    labs(color = NULL, title = d$PLAYER_NAME[1])
  list(plot = p, data = d)
}