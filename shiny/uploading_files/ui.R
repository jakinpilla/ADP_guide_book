setwd('C:/Users/dsc/adp_guidebook/shiny/uploading_files')

library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("CSV Viewer"),
  
  sidebarPanel(
    fileInput('file1', 'Choose CSV File', 
              accept=c('text/csv', 'text/comma-seperated-values,text/plain', '.csv')),
    tags$hr(),
    checkboxInput('header', 'Header', T),
    radioButtons('sep', 'Seperator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab = '\t'),
                 'Comma'),
    
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'Double Quote')
  ),
  
  mainPanel(
    tableOutput('contents')
  )
))