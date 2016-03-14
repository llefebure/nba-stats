# Author: Vineet Ahluwalia

library(shiny)

`%>%` <- dplyr::`%>%`

#d <- rNBA::stephCurryShots
# d <- rNBA::draftHistory
# colleges <- d %>% 
#   dplyr::filter(ORGANIZATION_TYPE == "College/University") %>%
#   dplyr::select(ORGANIZATION) %>%
#   dplyr::distinct(ORGANIZATION) %>%
#   dplyr::arrange(ORGANIZATION)

# get valid names
m <- rNBA::getIDMappings()

# valid player name options
player.names <- m$player$PLAYER_NAME[m$player$ROSTERSTATUS == 1]

shinyUI(fluidPage(
  
  titlePanel("Player ShotCharts"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Player",
                  "Player Name:",
                  choices = player.names,
                  selected = "Stephen Curry"),
      
#       dateRangeInput("dateRange",
#                      label = "Date Range",
#                      start = as.Date("1947", format = "%Y"),
#                      end = as.Date("2015", format = "%Y"),
#                      min = as.Date(min(d$SEASON), format = "%Y"), 
#                      max = as.Date(max(d$SEASON), format = "%Y"),
#                      separator = " - ", format = "yyyy",
#                      startview = "year"),
       
      downloadButton("downloadPlot", label = "Download Plot"),
      tableOutput("teamKey")
    ),
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("playerTable")
    )
  )
))
