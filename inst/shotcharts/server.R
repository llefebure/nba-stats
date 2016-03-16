# Author: Vineet Ahluwalia

library(shiny)

`%>%` <- dplyr::`%>%`
d <- rNBA::stephCurryShots
# The following gives the halfCoutPoint cooridnates
halfCourtPoint <- 417.5

# The following outputs per the input chosen
shinyServer(function(input, output) {

  # The following makes the reactive data  
  d <- reactiveValues(values = rNBA::stephCurryShots)

  # The following makes the color attribute reactive
  # color_type <- reactiveValues(values = )
    
  # The following makes the plot reactive
  plot <- reactive({
  
    
    # The following plots the shot chart with the ggplot points
    if (!is.null(d$values)) {
      # The following decides if the court needs to be full or half
      full <- ifelse(max(as.numeric(d$values$LOC_Y)) > halfCourtPoint, TRUE, FALSE)
      
      p <- rNBA::courtOutlinePlot(full) + ggplot2::geom_point(data = d$values, 
                                                              ggplot2::aes_string(x = "as.numeric(LOC_X)", 
                                                                                  y = "as.numeric(LOC_Y)", 
                                                                                  color = input$colorattr)) + 
        ggplot2::theme(legend.title = ggplot2::element_blank())
    } else {
      p <- rNBA::courtOutlinePlot() + ggplot2::annotate("text", x = 0, y = 100, label = "No data available")
    }
      })
  
  # The following takes in the player names
  observeEvent(input$Player, {
    # to get the ID of a selected player name
    selected <- input$Player
    id <- rNBA::searchIDMappings(player = selected)
    # to get data for plotting
    default.params <- list(SeasonType = "Regular+Season", TeamID = 0, PlayerID = id$player$PLAYER_ID, 
                           GameID = "", Outcome = "", Location = "", Month = 0, 
                           SeasonSegment = "", DateFrom = "", DateTo = "", 
                           OpponentTeamID = 0, VsConference = "", VsDivision = "", 
                           Position = "", RookieYear = "", GameSegment = "", Period=0, 
                           LastNGames = 30, ContextMeasure = "FGA", Season = "2015-16")
    
    # The following uses the memoised version of the getGenericData function
    shotChartData <- rNBA::memGetGenericData("shotchartdetail", default.params) # memoised version
    if (class(shotChartData) != "list") {
      d$values <- NULL  
    } else {
      d$values <- shotChartData[[1]]  
    }
    
  })
  
  # The following renders the plot of the shot chart
  output$plot <- renderPlot({
    print(plot())
  })
  
  # The following creates functionality for downloading the plot
  output$downloadPlot <- downloadHandler(
    filename = "shotchart.png",
    content = function(file) {
      png(file)
      print(plot())
      dev.off()
    }
  )
  
})
