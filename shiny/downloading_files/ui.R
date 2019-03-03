setwd('C:/Users/Daniel/ADP_guide_book/shiny/downloading_files')
library(shiny)

shinyUI(pageWithSidebar(
  headerPanel('Download Example'),
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:",
                choices=c("rock", "pressure", "cars")),
    downloadButton('downloadData', 'Download')
  ),
  
  mainPanel(
    tableOutput('table')
  )
))

