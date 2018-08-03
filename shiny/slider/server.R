library(shiny)

shinyServer(function(input, output){
  
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Integer", 
               "Decimal", 
               "Range", 
               "Custom Format", 
               "Animation"),
      Value = as.character(c(input$integer,
                             input$decimal,
                             paste(input$range, collapse=" "),
                             input$format,
                             input$animation)),
      stringsAsFactors = F)
  })
  
  output$values <- renderTable({
    sliderValues()
  })
})
