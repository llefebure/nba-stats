# Author: Luke Lefebure

library(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    
    x <- as.numeric(d$OVERALL_PICK[d$ORGANIZATION == input$college])  
    hist(x, breaks = seq(from = 0, to = max(x) + 5, by = 5), col = "darkgray", 
         border = "white", main = input$college)

  })

})
