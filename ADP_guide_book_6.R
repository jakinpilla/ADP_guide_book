options(repos=c(RStudio='http://rstudio.org/_packages', getOption('repos')))

install.packages('shiny')
library(shiny)

runExample('01_hello')
runExample('02_text')
runExample('03_reactivity')

# install.packages('vcd')
library(vcd)
library(datasets)

data("Titanic")
str(Titanic)
mosaic(Titanic)
head(Titanic)
mosaic(Titanic, shade=T, legend=T)
mosaic(HairEyeColor, shade=T, legend=T)

strucplot(Titanic, pop=F)
grid.edit("rect:Class=1st, Sex=Male, Age=Adult, Survived=Yes", gp=gpar(fill='red'))













