setwd('C:/Users/Daniel/ADP_guide_book/shiny/dataTable')

library(shiny)

shinyServer(function(input, output) {
  
  output$mytable1 = renderDataTable({
    library(ggplot2)
    diamonds[, input$show_vars, drop=F]
  })
  
  output$mytable2 = renderDataTable({
    mtcars
  }, options = list(bSortClasses = T))
  
  output$mytable3 = renderDataTable({
    iris
  }, options=list(aLengthMenu = c(5,30,50), iDisplayLength=5))
  
})