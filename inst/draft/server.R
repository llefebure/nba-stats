# Author: Luke Lefebure

library(shiny)

`%>%` <- dplyr::`%>%`
d <- rNBA::draftHistory

shinyServer(function(input, output) {

  filtered.d <- reactive({
    x <- d %>% dplyr::filter(ORGANIZATION == input$college, 
                             SEASON >= as.character(input$dateRange[1]), 
                             SEASON <= as.character(input$dateRange[2])) %>%
      mutate(DecadeStart = floor(as.numeric(SEASON)/10)*10, 
             Decade = paste(DecadeStart, DecadeStart + 10, sep = "-"))
  })
  
  plot <- reactive({
    p <- ggplot2::ggplot(filtered.d(),
                         ggplot2::aes(x = reorder(TEAM_ABBREVIATION, TEAM_ABBREVIATION, 
                                                  function(x) -length(x)), fill = Decade)) + 
      ggplot2::geom_bar() + 
      ggplot2::labs(x = "Team", y = "Number of Picks", 
                    title = paste("Draft Picks from", input$college)) +
      ggplot2::theme_bw()
  })
  
  output$playerTable <- renderTable(
    filtered.d() %>%
      mutate(Team = paste(TEAM_CITY, TEAM_NAME, paste0("(", TEAM_ABBREVIATION, ")"))) %>%
      select(PLAYER_NAME, SEASON, ROUND_NUMBER, ROUND_PICK, OVERALL_PICK, Team) %>%
      rename(Name = PLAYER_NAME, Season = SEASON, Round = ROUND_NUMBER,
             "Round Pick" = ROUND_PICK, "Overall" = OVERALL_PICK))
  
  output$teamKey <- renderTable(
    filtered.d() %>% 
      dplyr::mutate(Team = paste(TEAM_CITY, TEAM_NAME), Abbr = TEAM_ABBREVIATION) %>%
      dplyr::distinct(Team) %>%
      dplyr::select(Team, Abbr) %>%
      dplyr::arrange(Abbr))
  
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
