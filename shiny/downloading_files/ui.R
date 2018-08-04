setwd('C:/Users/dsc/adp_guidebook/shiny/downloading_files')

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