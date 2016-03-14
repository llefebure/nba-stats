# Author: Luke Lefebure

library(shiny)

`%>%` <- dplyr::`%>%`
d <- rNBA::draftHistory

shinyServer(function(input, output) {

  filtered.f <- reactive({
    x <- d %>% dplyr::filter(ORGANIZATION == input$college, 
                             SEASON >= as.character(input$dateRange[1]), 
                             SEASON <= as.character(input$dateRange[2]))
  })
  
  plot <- reactive({
    p <- ggplot2::ggplot(filtered.f(), ggplot2::aes(x = reorder(TEAM_ABBREVIATION, TEAM_ABBREVIATION, 
                                                                function(x) -length(x)))) + 
      ggplot2::geom_bar() + 
      ggplot2::labs(x = "Team", y = "Number of Picks", title = input$college)
  })
  
  output$playerTable <- renderTable(
    dplyr::select(filtered.f(), PLAYER_NAME, SEASON, ROUND_NUMBER, ROUND_PICK, 
                  OVERALL_PICK, TEAM_CITY, TEAM_NAME, TEAM_ABBREVIATION))
  
  output$teamKey <- renderTable(
    filtered.f() %>% 
      dplyr::mutate(TEAM = paste(TEAM_CITY, TEAM_NAME), ABBR = TEAM_ABBREVIATION) %>%
      dplyr::distinct(TEAM) %>%
      dplyr::select(TEAM, ABBR) %>%
      dplyr::arrange(ABBR))
  
  output$plot <- renderPlot({
    print(plot())
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      png(file)
      print(plot())
      dev.off()
    }
  )
  
})
