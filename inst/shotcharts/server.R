# Author: Vineet Ahluwalia

library(shiny)

`%>%` <- dplyr::`%>%`
d <- rNBA::stephCurryShots
halfCourtPoint <- 417.5
shinyServer(function(input, output) {
  
  d <- reactiveValues(values = stephCurryShots)
  
# Create reactive data later    
#   filtered.f <- reactive({
#     x <- d %>% dplyr::filter(ORGANIZATION == input$college, 
#                              SEASON >= as.character(input$dateRange[1]), 
#                              SEASON <= as.character(input$dateRange[2]))
#   })
  
  plot <- reactive({
    full <- ifelse(max(as.numeric(d$values$LOC_Y)) > halfCourtPoint, TRUE, FALSE)
      p <- courtOutlinePlot(full) + ggplot2::geom_point(data = d$values, ggplot2::aes(x = as.numeric(LOC_X), y = as.numeric(LOC_Y), color = EVENT_TYPE))
      #     p <- ggplot2::ggplot(filtered.f(), ggplot2::aes(x = reorder(TEAM_ABBREVIATION, TEAM_ABBREVIATION, 
#                                                                 function(x) -length(x)))) + 
#       ggplot2::geom_bar() + 
#       ggplot2::labs(x = "Team", y = "Number of Picks", title = input$college)
  })
  
#   output$playerTable <- renderTable(
#     dplyr::select(filtered.f(), PLAYER_NAME, SEASON, ROUND_NUMBER, ROUND_PICK, 
#                   OVERALL_PICK, TEAM_CITY, TEAM_NAME, TEAM_ABBREVIATION))
  
#   output$teamKey <- renderTable(
#     filtered.f() %>% 
#       dplyr::mutate(TEAM = paste(TEAM_CITY, TEAM_NAME), ABBR = TEAM_ABBREVIATION) %>%
#       dplyr::distinct(TEAM) %>%
#       dplyr::select(TEAM, ABBR) %>%
#       dplyr::arrange(ABBR))
  
  
  
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
    
    #shotChartData <- rNBA::getGenericData("shotchartdetail", default.params)
    shotChartData <- rNBA::memGetGenericData("shotchartdetail", default.params) # memoised version
    d$values <- shotChartData[[1]]
  })
  
    output$plot <- renderPlot({
    print(plot())
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "shotchart.png",
    content = function(file) {
      png(file)
      print(plot())
      dev.off()
    }
  )
  
})
