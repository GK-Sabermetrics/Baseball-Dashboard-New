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

PitchCallChoices = list(
  'Ball' = 'BallCalled',
  'KC' = 'StrikeCalled',
  'KS' = 'StrikeSwinging'
)




pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 

pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CU', 'SI', 'SP', 'KC'))

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
        if (is.null(input$ball)) TRUE else Balls %in% input$ball,
        if (is.null(input$strike)) TRUE else Strikes %in% input$strike,
        if (is.null(input$pcall)) TRUE else PitchCall %in% input$pcall,
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
  }, striped = TRUE, bordered = TRUE)
  
  
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
  
  updateCheckboxGroupInput(session, 'pcall', choices = c(PitchCallChoices))
  
  output$PitchMovementPlot = renderPlotly({
    
    fig = plot_ly(PitchingDF(), color = ~Pitch, colors = pcolors) %>% 
      add_trace(x = ~HorzBreak, y = ~InducedVertBreak, type = 'scatter', mode = 'markers',
                marker = list(size = 6))
    config(fig, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-30,30), showgrid = F),
        yaxis = list(range = c(-30,30), showgrid = F),
        title = "Pitch Movement",
        showlegend = F,
        legend = list(orientation ='h', 
                      x = 0, 
                      y = -200, 
                      xanchor = 'left',
                      yanchor = 'top',
                      itemwidth = -1,
                      traceorder = 'normal')
        #margin = list(b = 100)  
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
        title = "Strike Zone",
        showlegend = F,
        shapes = list(
          list(
          type = "rect",x0 = -0.708,x1 = 0.708,y0 = 1.5,y1 = 3.5
          ),
          list(
          type = 'line',x0 = -0.708,x1 = 0.708,y0 = 2.167,y1 = 2.167,layer = 'below',
          line = list(dash = 'dash', color = 'grey', width = 3)
          ),
          list(
            type = 'line',x0 = -0.708,x1 = 0.708,y0 = 2.833,y1 = 2.833,layer = 'below',
          line = list(dash = 'dash', color = 'grey', width = 3)
          ),
          list(
            type = 'line',x0 = -0.277,x1 = -0.277,y0 = 1.5,y1 = 3.5,layer = 'below',
            line = list(dash = 'dash', color = 'grey', width = 3)
          ),
          list(
            type = 'line',
            x0 = 0.277,x1 = 0.277,y0 = 1.5,y1 = 3.5,layer = 'below',
            line = list(dash = 'dash', color = 'grey', width = 3)
          )
        )
      )
  })
  
  output$PitcherReleasePlot = renderPlotly({
    fig = plot_ly(PitchingDF()) %>% 
      add_trace(x = ~RelSide, y = ~RelHeight, color = ~Pitch, colors = ~pcolors, type = 'scatter', mode = 'markers', 
                marker = list(size = 6)) 
      config(fig, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-5,5)),
        yaxis = list(range = c(4, 7)),
        title = "Pitch Release Points",
        showlegend = F,
        shapes = list(
          type = 'line',
          x0 = -5,
          x1 = 5,
          y0 = 5,
          y1 = 5,
          layer = 'below'
        )
      )
  })
 
  
  output$PitcherHeatmap = renderPlotly({
    
    plot_ly(PitchingDF(), x = ~PlateLocSide, y = ~PlateLocHeight) %>% 
      add_histogram2d(nbinsx = 25, nbinsy = 25, histfunc = 'count', histnorm = 'density', colorscale = 'hot') %>% 
      config(fig, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-3,3), showgrid = F, zeroline = F),
        yaxis = list(range = c(0,5), showgrid = F, zeroline = F),
        title = "Strike Zone",
        showlegend = F,
        shapes = list(
          list(
            type = "rect",x0 = -0.708,x1 = 0.708,y0 = 1.5,y1 = 3.5,
            line = list(color = 'white', width = 5)
          ),
          list(
            type = 'line',x0 = -0.708,x1 = 0.708,y0 = 2.167,y1 = 2.167,layer = 'above',
            line = list(dash = 'dash', color = 'white', width = 3)
          ),
          list(
            type = 'line',x0 = -0.708,x1 = 0.708,y0 = 2.833,y1 = 2.833,layer = 'above',
            line = list(dash = 'dash', color = 'white', width = 3)
          ),
          list(
            type = 'line',x0 = -0.277,x1 = -0.277,y0 = 1.5,y1 = 3.5,layer = 'above',
            line = list(dash = 'dash', color = 'white', width = 3)
          ),
          list(
            type = 'line',
            x0 = 0.277,x1 = 0.277,y0 = 1.5,y1 = 3.5,layer = 'above',
            line = list(dash = 'dash', color = 'white', width = 3)
          )
        )
      )
  })
   
}
