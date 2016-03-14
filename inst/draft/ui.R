# Author: Luke Lefebure

library(shiny)

`%>%` <- dplyr::`%>%`

d <- rNBA::draftHistory
colleges <- d %>% 
  dplyr::filter(ORGANIZATION_TYPE == "College/University") %>%
  dplyr::select(ORGANIZATION) %>%
  dplyr::distinct(ORGANIZATION) %>%
  dplyr::arrange(ORGANIZATION)

shinyUI(fluidPage(

  titlePanel("NBA Draft History"),

  sidebarLayout(
    sidebarPanel(
      
      selectInput("college",
                  "College:",
                  choices = colleges[,1],
                  selected = "Stanford"),
      
      dateRangeInput("dateRange",
                     label = "Date Range",
                     start = as.Date("1947", format = "%Y"),
                     end = as.Date("2015", format = "%Y"),
                     min = as.Date(min(d$SEASON), format = "%Y"), 
                     max = as.Date(max(d$SEASON), format = "%Y"),
                     separator = " - ", format = "yyyy",
                     startview = "year"),
      
      downloadButton("downloadPlot", label = "Download Plot"),
      tableOutput("teamKey")
    ),

    mainPanel(
      plotOutput("plot"),
      tableOutput("playerTable")
    )
  )
))
