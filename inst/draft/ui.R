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
  titlePanel("Draft Picks by College"),
  fluidRow(
    column(3,
           selectInput("college",
                       "College:",
                       choices = colleges[,1],
                       selected = "Stanford"),
           br(),
           dateRangeInput("dateRange",
                          label = "Date Range",
                          start = as.Date("1947", format = "%Y"),
                          end = as.Date("2015", format = "%Y"),
                          min = as.Date(min(d$SEASON), format = "%Y"), 
                          max = as.Date(max(d$SEASON), format = "%Y"),
                          separator = " - ", format = "yyyy",
                          startview = "year"),
           downloadButton("downloadPlot", label = "Download Plot")
    ),
    column(9,
           plotOutput("plot")
    )
  ),
  hr(),
  fluidRow(
    column(4,
           tableOutput("teamKey")
    ),
    column(8,
           tableOutput("playerTable")
    ))
))
