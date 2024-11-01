library(shiny)
library(tidyverse)
library(rlang)
library(bslib)
library(bsicons)
library(shinythemes)
library(scales)

data = read.csv("Data/20241023-MercerUniversity-Private-1_unverified.csv")

data =
  data %>% mutate(
    Count = paste(Balls, Strikes, sep = "-"), .after = "Outs"
  )


# Define server logic required to draw a histogram
function(input, output, session) {
  
    output$tableA = renderTable({
      data %>% 
        group_by(across(all_of(input$slicer))) %>% 
        summarise(
          Total = n(),#2
          '%' = percent(n()/length(.$TaggedPitchType)),#3
          Strikes = length(which(PitchCall != 'BallCalled')),#4
          'Strike%' = percent(Strikes/Total),#5
          Balls = length(which(PitchCall == 'BallCalled')),#6
          'Ball%' = percent(Balls/Total),#7
          Swings = length(which(PitchCall %in% c('StrikeSwinging','InPlay', 'FoulBallNotFieldable'))),#8
          'Whiff%' = percent(length(which((PitchCall == "StrikeSwinging")))/Swings),#9
        )%>%.[, c(1,2,3,4,5,6,7,8,9)]
    }, spacing = "xs", width = "100%")
    

}
