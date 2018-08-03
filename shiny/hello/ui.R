library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Hello shiny!"),
  
  sidebarPanel(
    sliderInput("obs",
                "Number of observations: ",
                min = 1, 
                max = 1000,
                value = 500)),
  mainPanel(
    plotOutput("distPlot")
  )
))