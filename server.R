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
library(kableExtra)


#### Pre Load Data ####
game = read.csv("Data/20241023-MercerUniversity-Private-1_unverified.csv")

#### Data Manipulation ####
# Add Pitch Column and Count column 
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
game$PitchCall[grepl("BallCalled", game$PitchCall)] = 'Ball'
game$PitchCall[grepl("BallinDirt", game$PitchCall)] = 'Ball'
game$PitchCall[grepl("FoulBallNotFieldable", game$PitchCall)] = 'Foul'


PitchCallChoices = list(
  "Ball" = 'Ball',
  'KC' = 'StrikeCalled',
  'KS' = 'StrikeSwinging',
  'Foul' = 'Foul',
  'In Play' = 'InPlay'
)

PitchChoices = list(
  'FB','2SFB','SI','CU','SP','CH','SL','CB'
)

HitChoices = list(
  'Bunt' = 'Bunt',
  'GB' = 'GroundBall',
  'LD' = 'LineDrive',
  'FB' = 'FlyBall',
  'P' = 'Popup'
)

pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 

pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CU', 'SI', 'SP', 'KC'))

#### End Data Manipulation ####

# Define server logic required to draw a histogram
function(input, output, session) {

#### Old Pitch Slicer Code ####
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
  
  # Filter Data ----  
  PitchingDF = reactive({
    
    game %>% 
      filter(PitcherTeam == "MER_BEA") %>% 
      filter(
        if (input$pitcher != "all") Pitcher == input$pitcher else TRUE,
        if (is.null(input$ball)) TRUE else Balls %in% input$ball,
        if (is.null(input$strike)) TRUE else Strikes %in% input$strike,
        if (is.null(input$outs)) TRUE else Outs %in% input$outs,
        if (is.null(input$pcall)) TRUE else PitchCall %in% input$pcall,
        if (is.null(input$pitch)) TRUE else Pitch %in% input$pitch,
        if (is.null(input$batterhand)) TRUE else BatterSide %in% input$batterhand,
        if (is.null(input$hittype)) TRUE else TaggedHitType %in% input$hittype,
        )
  })
  
  # Pitcher Standings ----
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
  
  # Pitcher Metrics ----
#  output$PitcherMetricsTable = renderTable({
#    
#    PitchingDF() %>% 
#      group_by(Pitch) %>% 
#      summarise(
#        Pitches = n(),
#        Usage = percent(n()/length(.$Pitch)),#3
#        Max = floor(max(RelSpeed, na.rm = TRUE)) %>% as.integer(),
#        Avg = floor(mean(RelSpeed, na.rm = TRUE)) %>% as.integer(),
#        Spin = mean(SpinRate, na.rm = T) %>% as.integer(),
#        Tilt = Tilt %>% as.POSIXct(format = '%H:%M', tz = 'UTC') %>%
#          as.numeric() %>% mean(na.rm = T) %>%
#          as.POSIXct(origin = '1970-01-01', tz = 'UTC') %>%
#          format(format = "%k:%M", tz = 'UTC'),
#        HB = mean(HorzBreak, na.rm = T),
#        IVB = mean(InducedVertBreak, na.rm = T),
#        VAA = mean(VertApprAngle, na.rm = T),
#        HAA = mean(HorzApprAngle, na.rm = T),
#        RelH = mean(RelHeight, na.rm = T),
#        RelS = mean(RelSide, na.rm = T),
#        Ext = mean(Extension, na.rm = T)
#        
#      )
#  }, striped = TRUE, bordered = TRUE)
  
  output$PitcherMetricsTable = renderUI({
    
    pitch_order <- c("FB", "2SFB", "SI", "CU", "CH", "SL", "CB")
    
    tableA = 
    PitchingDF() %>% 
      group_by(Pitch) %>% 
      summarise(
        "#" = n(),
        Usage = percent(n()/length(.$Pitch)),#3
        Max = floor(max(RelSpeed, na.rm = TRUE)) %>% as.integer(),
        Avg = floor(mean(RelSpeed, na.rm = TRUE)) %>% as.integer(),
        Spin = mean(SpinRate, na.rm = T) %>% as.integer(),
        Tilt = Tilt %>% as.POSIXct(format = '%H:%M', tz = 'UTC') %>%
          as.numeric() %>% mean(na.rm = T) %>%
          as.POSIXct(origin = '1970-01-01', tz = 'UTC') %>%
          format(format = "%k:%M", tz = 'UTC'),
        HB = mean(HorzBreak, na.rm = T) %>% round(2),
        IVB = mean(InducedVertBreak, na.rm = T) %>% round(2),
        VAA = mean(VertApprAngle, na.rm = T) %>% round(2),
        HAA = mean(HorzApprAngle, na.rm = T) %>% round(2),
        Ext = mean(Extension, na.rm = T) %>% round(2)
        ) %>% 
      mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
      arrange(Pitch)
    
    
    tableA %>% 
      kable(format = 'html', align = 'c') %>% kable_styling(font_size = 15) %>% 
      kable_styling(bootstrap_options = 'bordered') %>% 
      column_spec(1, border_left = TRUE, bold = T, color = 'white',background = case_when(
        tableA$Pitch == "FB" ~ '#d22d49',
        tableA$Pitch == "2SFB" ~ '#93afd4',
        tableA$Pitch == "SI" ~ '#de6a04',
        tableA$Pitch == "CU" ~ '#933f2c',
        tableA$Pitch == "CH" ~ '#1dbe3a',
        tableA$Pitch == "SL" ~ '#c3bd0e',
        tableA$Pitch == "CB" ~ '#00d1ed'
      )) %>% 
      row_spec(row = 0, color = "white", background = "orange") %>% HTML()
  })
  
  
  # Pitcher Slicer ----
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
        K = length(which(KorBB == 'Strikeout')),
        Walks = length(which(KorBB == "Walk")),
        HBP = length(which(PitchCall == 'HitByPitch')),
        SF = length(which(PlayResult == 'Sacrifice')),
        H = length(which(PitchCall == 'InPlay' & PlayResult != 'Out')), #10
        PA = length(which(Count == '0-0')),
        AB = length(which(PitchCall == 'InPlay' & !PlayResult %in% c('Sacrifice'))) + K,
        AVG = sprintf((H/AB), fmt = '%#.3f'),
        
         
      )
    #%>%.[, c(1,2,3,5,7,9,10)]
  }, striped = TRUE, bordered = TRUE)
    

  # Update Inputs ----
  updateSelectInput(session, 'pitcher', choices = c('all', unique(game$Pitcher)))
  
  updateCheckboxGroupInput(session, 'pcall', choices = c(PitchCallChoices))
  
  updateCheckboxGroupInput(session, 'pitch', choices = c(PitchChoices))
  
  updateCheckboxGroupInput(session, 'hittype', choices = c(HitChoices))
  
  # End Update Inputs
  
  # Pitcher Movmement Plot ----
  output$PitchMovementPlot = renderPlotly({
    
    fig = plot_ly(PitchingDF(), color = ~Pitch, colors = pcolors) %>% 
      add_trace(x = ~HorzBreak, y = ~InducedVertBreak, type = 'scatter', mode = 'markers',
                marker = list(size = 6),
                customdata = ~Pitch,
                hovertemplate = "HB: %{x:.2f} <br>VB: %{y:.2f} <extra>%{customdata}</extra>"
                #text = ~paste(Pitch,
                #              '<br>HB:', round(HorzBreak, 1),'in',
                #              '<br>VB:', round(InducedVertBreak, 1),'in',
                #              '<br>Spin:',round(PitchingDF()$SpinRate),'RPM',
                #              '<br>HRA:',round(PitchingDF()$HorzRelAngle, 2),'º',
                #              '<br>VRA:',round(PitchingDF()$VertRelAngle, 2),'º',
                #              "<extra>test</extra>"
                #              ), 
                #hoverinfo = 'text'
                )
    config(fig, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-30,30)),
        yaxis = list(range = c(-30,30)),
        title = list(
          text = "Pitch Movement",
          x = .55,           # Centers the title
          xanchor = "center"), # Aligns the title from the center
        showlegend = F,
        legend = list(orientation ='h', 
                      x = 0, 
                      y = -200, 
                      xanchor = 'left',
                      yanchor = 'top',
                      itemwidth = -1,
                      traceorder = 'normal')
      )
  })
  
  output$PitchMovementPlotB = renderPlotly({
    
    fig = plot_ly(PitchingDF(), color = ~Pitch, colors = pcolors, source = 'PMB') %>% 
      add_trace(x = ~HorzBreak, y = ~InducedVertBreak, type = 'scatter', mode = 'markers',
                marker = list(size = 6),
                #customdata = I(PitchingDF()[, c(~Pitch, ~SpinRate)]),
                #hovertemplate = "HB: %{x:.2f} <br>VB: %{y:.2f} <extra>%{customdata}</extra>"
                text = ~paste(Pitch,
                              '<br>HB:', round(HorzBreak, 1),'in',
                              '<br>VB:', round(InducedVertBreak, 1),'in',
                              '<br>Spin:',round(PitchingDF()$SpinRate),'RPM',
                              '<br>Ext:', round(PitchingDF()$Extension,2), 'ft'
                              #'<br>HRA:',round(PitchingDF()$HorzRelAngle, 2),'º',
                              #'<br>VRA:',round(PitchingDF()$VertRelAngle, 2),'º'
                              ),
                hoverinfo = 'text'
      )
    config(fig, dragmode = 'selected') %>% 
      layout(
        xaxis = list(range = c(-30,30)),
        yaxis = list(range = c(-30,30)),
        #title = "Pitch Movement",
        showlegend = F,
        legend = list(orientation ='h', 
                      x = 0, 
                      y = -200, 
                      xanchor = 'left',
                      yanchor = 'top',
                      itemwidth = -1,
                      traceorder = 'normal')
      )
  })
  
  output$PitchReleaseAngle = renderPlotly({
    
    fig = plot_ly(PitchingDF(), color = ~Pitch, colors = pcolors) %>% 
      add_trace(x = ~HorzRelAngle, y = ~VertRelAngle, type = 'scatter', mode = 'markers')
    config(fig) %>% 
      layout(
        showlegend = F,
        title = "Pitch Release Angle"
      )
    
  })
  
  
  #### Pitch Movement Data ####
  output$PitchMovementData = renderTable({
    
    d = event_data('plotly_selected', source = 'PMB')
    d
  })
  
  
  # Strikezone Plot ----
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
  
  # Release Plot ----
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
 
  # Heatmap ----
  output$PitcherHeatmap = renderPlotly({
    fig = 
    plot_ly(PitchingDF(), x = ~PlateLocSide, y = ~PlateLocHeight) %>% 
      add_histogram2dcontour(nbinsx = 25, nbinsy = 25, histfunc = 'count', histnorm = 'density', colorscale = 'hot') 
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
   
  # Pitch By Pitch Overview Table ----
  output$PBPOverview = renderTable({
    
    table = 
    game %>% 
      select(PitchNo, Pitcher, BatterSide, Inning, Top.Bottom, Count, Outs, Pitch,
             PitchCall, KorBB, TaggedHitType, PlayResult, RelSpeed, SpinRate, HorzBreak,
             InducedVertBreak, ExitSpeed, Angle, Distance)
    table
    
  })
  
  
}
