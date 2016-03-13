# Author: Luke Lefebure

library(shiny)

d <- rNBA::draftHistory
colleges <- sort(unique(d$ORGANIZATION[d$ORGANIZATION_TYPE == "College/University"]))
previous.org <- sort(unique(d$ORGANIZATION[d$ORGANIZATION_TYPE != "College/University"]))

shinyUI(fluidPage(

  titlePanel("NBA Draft History"),

  sidebarLayout(
    sidebarPanel(
      
      selectInput("college",
                  "College:",
                  choices = colleges,
                  selected = "Stanford"),
      
      downloadButton("downloadPlot", label = "Download Plot")
    ),

    mainPanel(
      plotOutput("plot")
    )
  )
))
