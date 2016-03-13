# Author: Luke Lefebure

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  plot <- reactive({
    x <- d[d$ORGANIZATION == input$college,]  
    p <- ggplot(x, aes(as.numeric(OVERALL_PICK))) + 
      geom_histogram() + 
      labs(x = "Pick Number", y = "Count", title = input$college)
  })
  
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
