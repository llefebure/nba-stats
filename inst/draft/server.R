# Author: Luke Lefebure

library(shiny)
library(ggplot2)
library(dplyr)

d <- rNBA::draftHistory

shinyServer(function(input, output) {

  filtered.f <- reactive({
    x <- d %>% filter(ORGANIZATION == input$college, SEASON >= as.character(input$dateRange[1]),
                      SEASON <= as.character(input$dateRange[2]))
  })
  
  plot <- reactive({
    p <- ggplot(filtered.f(), aes(x = reorder(TEAM_ABBREVIATION, TEAM_ABBREVIATION, 
                                            function(x) -length(x)))) + 
      geom_bar() + 
      labs(x = "Team", y = "Number of Picks", title = input$college)
  })
  
  output$playerTable <- renderTable(
    select(filtered.f(), PLAYER_NAME, SEASON, ROUND_NUMBER, ROUND_PICK, 
           OVERALL_PICK, TEAM_CITY, TEAM_NAME, TEAM_ABBREVIATION))
  
  output$teamKey <- renderTable(
    filtered.f() %>% 
      mutate(TEAM = paste(TEAM_CITY, TEAM_NAME), ABBR = TEAM_ABBREVIATION) %>%
      distinct(TEAM) %>%
      select(TEAM, ABBR) %>%
      arrange(ABBR))
  
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
