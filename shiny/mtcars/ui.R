library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Miles Per Gallon"),
    
  sidebarPanel(
    selectInput('variable', "Variable: ", 
                list("Cylinders" = "cyl",
                     "Transmission" = "am",
                     "Gears" = "gear")),
    
    checkboxInput("outliers", "Show outliers", F)
    ),
  
  mainPanel(
    h3(textOutput("caption")),
    
    plotOutput("mpgPlot")
  )
))
