# Author: Vineet Ahluwalia

library(shiny)

`%>%` <- dplyr::`%>%`

# get valid names
m <- rNBA::getIDMappings()

# valid player name options
player.names <- m$player$PLAYER_NAME[m$player$ROSTERSTATUS == 1]

# valid collor attributes
color.attr <- c(Quarter = "PERIOD", Area = "SHOT_ZONE_AREA", Outcome = "EVENT_TYPE")

# The following initializes the user interface
shinyUI(fluidPage(
  
  # The following makes the title panel
  titlePanel("Player ShotCharts"),
  sidebarLayout(
    sidebarPanel(
      
      # The following initializes an input behavior for the player names 
      selectInput("Player",
                  "Player Name:",
                  choices = player.names,
                  selected = "Stephen Curry"),
      
      # The following initializes an input behavior for the type of chart 
      selectInput("colorattr",
                  "Color Attribute:",
                  choices = color.attr,
                  selected = "EVENT_TYPE"),
      
      # The following  creates a download button
      downloadButton("downloadPlot", label = "Download Plot")
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
))
