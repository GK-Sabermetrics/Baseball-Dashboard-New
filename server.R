
library(shiny)
library(bslib)
library(bsicons)
library(shinythemes)

data = read.csv("Data/20241023-MercerUniversity-Private-1_unverified.csv")


# Define server logic required to draw a histogram
function(input, output, session) {
  
    output$tableA = renderTable(
      data
    )
    

}
