library(shiny)
library(tidyverse)
library(rlang)
library(bslib)
library(bsicons)
library(condformat)
library(scales)
library(htmltools)
library(DT)
library(plotly)



game = read.csv("Data/20241023-MercerUniversity-Private-1_unverified.csv")

game =
  game %>% mutate(
    Count = paste(Balls, Strikes, sep = "-"), .after = "Outs",
    Pitch = TaggedPitchType
  )

game$Pitch[grepl("Fastball", game$Pitch)] = "FB"
game$Pitch[grepl("TwoSeamFastBall", game$Pitch)] = "2SFB"
game$Pitch[grepl("Sinker", game$Pitch)] = "SI"
game$Pitch[grepl("Cutter", game$Pitch)] = "CU"
game$Pitch[grepl("Splitter", game$Pitch)] = "SP"
game$Pitch[grepl("ChangeUp", game$Pitch)] = "CH"
game$Pitch[grepl("Slider", game$Pitch)] = "SL"
game$Pitch[grepl("Curveball", game$Pitch)] = "CB"


cols = c('Fastball' = '#d22d49', 'TwoSeamFastBall' = '#93afd4', 'ChangeUp' = '#1dbe3a', 
         'Slider' = '#c3bd0e', 'Curveball' = '#00d1ed', 'Cutter' = '#933f2c', 
         'Sinker' = '#de6a04', 'Splitter' = '#DDB33A', 'Four-Seam' = '#d22d49', 'KnuckleBall' = '#854cb5')


# Define server logic required to draw a histogram
function(input, output, session) {
  
#    output$tableA = renderTable({
#      data %>% 
#        group_by(across(all_of(input$slicer))) %>% 
#        summarise(
#          Total = n(),#2
#          '%' = percent(n()/length(.$TaggedPitchType)),#3
#          Strikes = length(which(PitchCall != 'BallCalled')),#4
#          'Strike%' = percent(Strikes/Total),#5
#          Balls = length(which(PitchCall == 'BallCalled')),#6
#          'Ball%' = percent(Balls/Total),#7
#          Swings = length(which(PitchCall %in% c('StrikeSwinging','InPlay', 'FoulBallNotFieldable'))),#8
#          'Whiff%' = percent(length(which((PitchCall == "StrikeSwinging")))/Swings),#9
#        )%>%.[, c(1,2,3,4,5,6,7,8,9)]
#    }, striped = TRUE, bordered = TRUE)
    
  PitchingDF = reactive({
    game %>% 
      filter(PitcherTeam == "MER_BEA") %>% 
      filter(
        if (input$pitcher != "all") Pitcher == input$pitcher else TRUE,
        if (is.null(input$ball)) TRUE else Balls %in% input$ball
        )
  })
  
  output$PitcherStandingsTable = renderCondformat({
    
    table = 
      PitchingDF() %>% 
      group_by(Pitcher) %>% 
      summarise(
        Pitches = n(),
      )
    
    # Apply conditional formatting
    condformat(table) %>% 
      rule_fill_gradient(Pitches, low = "lightblue", high = "blue") %>% theme_htmlTable(rnames = FALSE)
    
    
    
  })
  
  output$PitcherMetricsTable = renderTable({
    
    PitchingDF() %>% 
      group_by(Pitch) %>% 
      summarise(
        Pitches = n(),
        Usage = percent(n()/length(.$Pitch)),#3
        Max = floor(max(RelSpeed, na.rm = TRUE)) %>% as.integer(),
        Avg = floor(mean(RelSpeed, na.rm = TRUE)) %>% as.integer(),
        Spin = mean(SpinRate, na.rm = T) %>% as.integer(),
        Tilt = Tilt %>% as.POSIXct(format = '%H:%M', tz = 'UTC') %>%
          as.numeric() %>% mean(na.rm = T) %>%
          as.POSIXct(origin = '1970-01-01', tz = 'UTC') %>%
          format(format = "%k:%M", tz = 'UTC'),
        HB = mean(HorzBreak, na.rm = T),
        IVB = mean(InducedVertBreak, na.rm = T),
        VAA = mean(VertApprAngle, na.rm = T),
        HAA = mean(HorzApprAngle, na.rm = T),
        RelH = mean(RelHeight, na.rm = T),
        RelS = mean(RelSide, na.rm = T),
        Ext = mean(Extension, na.rm = T)
        
      )
  }, bordered = TRUE)
  
  
  output$PitcherSlicerTable = renderTable({
    
    group_vars <- list(sym(input$slicerA))
    if (!is.null(input$slicerB) && input$slicerB != "") {
      group_vars <- c(group_vars, sym(input$slicerB))
    }
    
    PitchingDF() %>% 
      group_by(!!!group_vars) %>% 
      summarise(
        Total = n(),#2
        '%' = percent(n()/length(.$Pitch)),#3
        Strikes = length(which(PitchCall != 'BallCalled')),#4
        'Strike%' = percent(Strikes/Total),#5
        Balls = length(which(PitchCall == 'BallCalled')),#6
        'Ball%' = percent(Balls/Total),#7
        Swings = length(which(PitchCall %in% c('StrikeSwinging','InPlay', 'FoulBallNotFieldable'))),#8
        'Whiff%' = percent(length(which((PitchCall == "StrikeSwinging")))/Swings),#9
      )%>%.[, c(1,2,3,5,7,9)]
  }, striped = TRUE, bordered = TRUE)
    

  updateSelectInput(session, 'pitcher', choices = c('all', unique(game$Pitcher)))
  
  
  output$PitchMovementPlot = renderPlotly({
    
    fig = plot_ly(PitchingDF()) %>% 
      add_trace(x = ~HorzBreak, y = ~InducedVertBreak, type = 'scatter', mode = 'markers',
                marker = list(size = 6, color = ~cols[TaggedPitchType]))
    config(fig, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-30,30), showgrid = F),
        yaxis = list(range = c(-30,30), showgrid = F)
      )
  })
  
  output$StrikeZonePlot = renderPlotly({
    
    fig = plot_ly(PitchingDF()) %>% 
      add_trace(x = ~PlateLocSide, y = ~PlateLocHeight, color = ~PitchCall, type = 'scatter', mode = 'markers'
      )
    config(fig, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-3,3), showgrid = F, zeroline = F),
        yaxis = list(range = c(-0.5,5), showgrid = F, zeroline = F),
        showlegend = F,
        shapes = list(
          type = "rect",
          x0 = -0.708,
          x1 = 0.708,
          y0 = 1.5,
          y1 = 3.5
        )
      )
  })
  
}
