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
data = read.csv("Data/Fall Scrimmage Data copy.csv")

#### Data Manipulation ####
# Add Pitch Column and Count column 
game =
  filter(data, TaggedPitchType != 'Other') %>% 
  mutate(
    Count = paste(Balls, Strikes, sep = "-"), .after = "Outs",
    Pitch = TaggedPitchType,
    Pitch = recode(Pitch, Fastball = "FB", TwoSeamFastBall = "2SFB", Sinker = 'SI', 
                   Cutter = 'CT', Splitter = 'SP', ChangeUp = 'CH', Slider = 'SL',
                   Curveball = 'CB', KnuckleBall = 'KC'),
    PitchCall = recode(PitchCall, BallCalled = 'Ball', BallinDirt = 'Ball',
                       FoulBallNotFieldable = 'Foul', FoulBallFieldable = 'Foul'),
    Top.Bottom = recode(Top.Bottom, Top = "T", Bottom = "B"),
    Inn = paste(Top.Bottom, Inning, sep = " "),
    KorBB = recode(KorBB, Strikeout = 'Strikeout', Walk = 'Walk', Undefined = ""),
    ArmRad = atan2(RelHeight, RelSide),
    ArmDeg = ArmRad * (180/pi),
    ArmDeg = ifelse(PitcherThrows == "Right", ArmDeg, 180-ArmDeg)
  ) %>% 
  rename(
    PAOutcome = KorBB,
    PitchType = TaggedPitchType,
    HitType = TaggedHitType,
    Velo = RelSpeed,
    Spin = SpinRate,
    IVB = InducedVertBreak,
    HB = HorzBreak
  )

#game$Pitch[grepl("Fastball", game$Pitch)] = "FB"
#game$Pitch[grepl("TwoSeamFastBall", game$Pitch)] = "2SFB"
#game$Pitch[grepl("Sinker", game$Pitch)] = "SI"
#game$Pitch[grepl("Cutter", game$Pitch)] = "CU"
#game$Pitch[grepl("Splitter", game$Pitch)] = "SP"
#game$Pitch[grepl("ChangeUp", game$Pitch)] = "CH"
#game$Pitch[grepl("Slider", game$Pitch)] = "SL"
#game$Pitch[grepl("Curveball", game$Pitch)] = "CB"
#game$PitchCall[grepl("BallCalled", game$PitchCall)] = 'Ball'
#game$PitchCall[grepl("BallinDirt", game$PitchCall)] = 'Ball'
#game$PitchCall[grepl("FoulBallNotFieldable", game$PitchCall)] = 'Foul'
game$HitType[grepl('Undefined', game$HitType)] = ""
game$PlayResult[grepl('Undefined', game$PlayResult)] = ""

PitchCallChoices = list(
  "Ball" = 'Ball',
  'KC' = 'StrikeCalled',
  'KS' = 'StrikeSwinging',
  'Foul' = 'Foul',
  'In Play' = 'InPlay'
)

PitchChoices = list(
  'FB','2SFB','SI','CT','SP','CH','SL','CB', 'KC'
)

HitChoices = list(
  'Bunt' = 'Bunt',
  'GB' = 'GroundBall',
  'LD' = 'LineDrive',
  'FB' = 'FlyBall',
  'P' = 'Popup'
)

PAChoices = list(
  'BB' = 'Walk',
  'SO' = 'Strikeout'
)

pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 

pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))

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
  #### . > Pitching ####
  PitchingDF = reactive({
    
    game %>% 
      filter(
        if (input$pitcherPitcherDashboard != "all") Pitcher == input$pitcherPitcherDashboard else TRUE,
        if (input$datePitcherDashboard != 'all') Date == input$datePitcherDashboard else TRUE,
        if (is.null(input$ballPitcherDashboard)) TRUE else Balls %in% input$ballPitcherDashboard,
        if (is.null(input$strikePitcherDashboard)) TRUE else Strikes %in% input$strikePitcherDashboard,
        if (is.null(input$outsPitcherDashboard)) TRUE else Outs %in% input$outsPitcherDashboard,
        if (is.null(input$pcallPitcherDashboard)) TRUE else PitchCall %in% input$pcallPitcherDashboard,
        if (is.null(input$pitchPitcherDashboard)) TRUE else Pitch %in% input$pitchPitcherDashboard,
        if (is.null(input$batterhandPitcherDashboard)) TRUE else BatterSide %in% input$batterhandPitcherDashboard,
        if (is.null(input$hittypePitcherDashboard)) TRUE else HitType %in% input$hittypePitcherDashboard,
        if (is.null(input$paoutcomePitcherDashboard)) TRUE else PAOutcome %in% input$paoutcomePitcherDashboard,
        )
  })
  
  PitcherDF = reactive({
    game %>%
      filter(if (input$pitcherPitcherDashboard != "all") Pitcher == input$pitcherPitcherDashboard else TRUE)
  })
  
  PitcherDFOverview = reactive({
    game %>% 
      filter(
        if (input$pitcherPitcherOverview != 'all') Pitcher == input$pitcherPitcherOverview else TRUE,
        if (input$datePitcherOverview != 'all') Date == input$datePitcherOverview else TRUE
        )
  })
  
  #### . > Batting ####
  BattingDF = reactive({
    game %>% 
      filter(
        if (input$batterBatterDashboard != "all") Batter == input$batterBatterDashboard else TRUE,
        if (input$dateBatterDashboard != "all") Date == input$dateBatterDashboard else TRUE
      )
  })
  
  
  # Update Inputs ----
  #### . > Pitching ####
  updateSelectInput(session, 'pitcherPitcherDashboard', choices = c(unique(game$Pitcher[game$PitcherTeam == 'MER_BEA'])))
  
  updateSelectInput(session, 'datePitcherDashboard', choices = c("all", unique(game$Date)))
  
  updateCheckboxGroupInput(session, 'pcallPitcherDashboard', choices = c(PitchCallChoices))
  
  updateCheckboxGroupInput(session, 'pitchPitcherDashboard', choices = c(PitchChoices))
  
  updateCheckboxGroupInput(session, 'hittypePitcherDashboard', choices = c(HitChoices))
  
  updateSelectInput(session, 'pitcherPitcherOverview', choices = c(unique(game$Pitcher[game$PitcherTeam == 'MER_BEA'])))
  
  updateSelectInput(session, 'datePitcherOverview', choices = c('all', unique(game$Date)))
  
  updateCheckboxGroupInput(session, 'paoutcomePitcherDashboard', choices = c(PAChoices))
  
  #### . > Batting ####
  updateSelectInput(session, 'batterBatterDashboard', choices = c(unique(game$Batter[game$BatterTeam == 'MER_BEA'])))
  
  updateSelectInput(session, 'dateBatterDashboard', choices = c('all', unique(game$Date)))
  
  
  # Observers for Inputs ----
  #### . > Pitching ####
  # Display dates in date filter based on selected pitcher
  observeEvent(input$pitcherPitcherDashboard, {
    choices = if (input$pitcherPitcherDashboard == "all") {
      c("all", unique(game$Date))
    } else {
      c("all", unique(game$Date[game$Pitcher == input$pitcherPitcherDashboard]))
    }
    updateSelectInput(session, "datePitcherDashboard", choices = choices, selected = input$datePitcherDashboard) 
  })
  
  # Observe if pitcher name changes only display dates he threw on the pitcher overview page
  observeEvent(input$pitcherPitcherOverview, {
    choices = if (input$pitcherPitcherOverview == "all") {
      c("all", unique(game$Date))
    } else {
      c("all", unique(game$Date[game$Pitcher == input$pitcherPitcherOverview]))
    }
    updateSelectInput(session, "datePitcherOverview", choices = choices, selected = input$datePitcherOverview) 
  })
  
  # Change pitches in filter based on selected pitcher arsenal
  observeEvent(input$pitcherPitcherDashboard,{
    updateCheckboxGroupInput(session, 'pitchPitcherDashboard', choices = PitchChoices[PitchChoices %in% unique(PitcherDF()$Pitch)])
  })
  
  #### . > Batting ####
  observeEvent(input$batterBatterDashboard, {
    choices = if (input$batterBatterDashboard == "all") {
      c("all", unique(game$Date))
    } else {
      c("all", unique(game$Date[game$Batter == input$batterBatterDashboard]))
    }
    updateSelectInput(session, "dateBatterDashboard", choices = choices, selected = input$dateBatterDashboard) 
  })
  
  
  # Pitcher Standings ----
  output$PitcherStandingsTable = renderUI({
    table = 
      game %>% 
      filter(PitcherTeam == 'MER_BEA') %>% 
      group_by(Pitcher) %>% 
      summarise(
        '#' = n(),
        'IPb' = ((sum(OutsOnPlay)+length(which(PAOutcome == 'Strikeout')))/3),
        'IP' = ifelse(IPb %% 1 == 0, IPb + 0.0, 
                      ifelse(between(IPb %% 1, .0, .34), IPb - .2333333, IPb -.4666666)) %>% as.numeric(),
        'H' = length(which(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun'))),
        '1B' = length(which(PlayResult == 'Single')), #5
        '2B' = length(which(PlayResult == 'Double')),
        '3B' = length(which(PlayResult == 'Triple')),
        'HR' = length(which(PlayResult == 'HomeRun')),
        'R' = sum(RunsScored),
        'FC' = length(which(PlayResult == 'FieldersChoice')), #10
        'SO' = length(which(PAOutcome == 'Strikeout')),
        'E' = length(which(PlayResult == 'Error')),
        'O' = length(which(PlayResult == 'Out')),
        'BB' = length(which(PAOutcome == 'Walk')),
        'HBP' = length(which(PitchCall == 'HitByPitch')), #15
        'SF' = length(which(PlayResult == 'Sacrifice')),
        'TB' = (`1B` + `2B`*2 + `3B`*3 + `HR`*4),
        'PA' = length(which(Count == '0-0')),
        'AB' = FC + H + E + O + SO,
        'BA' = sprintf((H/AB), fmt = '%#.3f') %>% as.numeric(), #20
        'OBP' = sprintf((H+BB+HBP)/(AB+BB+HBP+SF), fmt = '%#.3f') %>% as.numeric(),
        'SLG' = sprintf(TB/AB, fmt = '%#.3f') %>% as.numeric(),
        'OPS' = sprintf(OBP+SLG, fmt = '%#.3f') %>% as.numeric(),
        'wOBA' = (((0.69*`BB`)+(0.72*`HBP`)+(0.89*`1B`)+(1.27*`2B`)+(1.62*`3B`)+(2.10*HR))/(AB+BB+SF+HBP)) %>% 
          round(digits = 3),
        'K/9' = ((9*SO)/IP) %>% round(2), #25,
        'WHIP' = ((H+BB)/IP) %>% round(2)
      ) %>% .[, c(1,2,4,20,5,7,8,9,10,12,15,16,21:27)] %>% .[-c(11,22),]
    
    # WHITE rgb(1,1,1)
    # GREEN rgb(0,1,0)
    # YELLOW rgb(1,1,0)
    # RED rgb(1,0,0),
    # BLUE rgb(0,1,1)
    
    # Apply conditional formatting
    condformat(table) %>% 
      rule_fill_gradient2(BA, low = 'green2', mid = 'white', high = 'red2', midpoint = .270) %>% 
      rule_fill_gradient2(`K/9`, low = 'red2', mid = 'white', high = 'green2') %>% 
      rule_fill_gradient2(OBP, low = 'green2', mid = 'white', high = 'red2') %>% 
      rule_fill_gradient2(SLG, low = 'green2', mid = 'white', high = 'red2') %>% 
      rule_fill_gradient2(OPS, low = 'green2', mid = 'white', high = 'red2') %>% 
      rule_fill_gradient2(WHIP, low = 'green2', mid = 'white', high = 'red2', midpoint = 1.4) %>% 
      rule_fill_gradient2(wOBA, low = 'green2', mid = 'white' , high = 'red2', midpoint = .320) %>%
      rule_text_bold(wOBA, OBP > wOBA) %>% 
      theme_htmlTable(rnames = FALSE) %>% 
      condformat2html() %>% HTML()
  })
  
  #### Pitcher Individual Stats Table ####
  output$PitcherIndividualStats = renderUI({
    tableA = PitcherDFOverview() %>% 
      summarise(
        'Pitches' = n(),
        'IPb' = ((sum(OutsOnPlay)+length(which(PAOutcome == 'Strikeout')))/3),
        'IP' = ifelse(IPb %% 1 == 0, IPb + 0.0, 
                      ifelse(between(IPb %% 1, .0, .34), IPb - .2333333, IPb -.4666666)) %>% as.numeric(),
        'H' = length(which(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun'))),
        '1B' = length(which(PlayResult == 'Single')), #5
        '2B' = length(which(PlayResult == 'Double')),
        '3B' = length(which(PlayResult == 'Triple')),
        'HR' = length(which(PlayResult == 'HomeRun')),
        'R' = sum(RunsScored),
        'FC' = length(which(PlayResult == 'FieldersChoice')), #10
        'SO' = length(which(PAOutcome == 'Strikeout')),
        'E' = length(which(PlayResult == 'Error')),
        'O' = length(which(PlayResult == 'Out')),
        'BB' = length(which(PAOutcome == 'Walk')),
        'HBP' = length(which(PitchCall == 'HitByPitch')), #15
        'SF' = length(which(PlayResult == 'Sacrifice')),
        'TB' = (`1B` + `2B`*2 + `3B`*3 + `HR`*4),
        'PA' = length(which(Count == '0-0')),
        'AB' = FC + H + E + O + SO,
        'BA' = sprintf((H/AB), fmt = '%#.3f'), #20
        'OBP' = sprintf((H+BB+HBP)/(AB+BB+HBP+SF), fmt = '%#.3f') %>% as.numeric(),
        'SLG' = sprintf(TB/AB, fmt = '%#.3f') %>% as.numeric(),
        'OPS' = sprintf(OBP+SLG, fmt = '%#.3f'),
        'wOBA' = (((0.69*`BB`)+(0.72*`HBP`)+(0.89*`1B`)+(1.27*`2B`)+(1.62*`3B`)+(2.10*HR))/(AB+BB+SF+HBP)) %>% 
          round(digits = 3),
        'K/9' = ((9*SO)/IP) %>% round(2), #25,
        'WHIP' = ((H+BB)/IP) %>% round(2)
      ) %>% .[, c(1,3,19,4,6,7,8,9,11,14,15,20:26)]
    
    tableA %>% 
      kable(format = 'html', align = 'c') %>% kable_styling(font_size = 15) %>% 
      kable_styling(bootstrap_options = 'bordered') %>% 
      row_spec(row = 0, color = "white", background = "orange") %>% HTML()
    
  })
  
  
  # Pitch Metrics ----
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
  
  #### . > Pitcher Dashboard Metrics Table ####
  output$PitcherMetricsTablePitcherDashboard = renderUI({
    
    pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")
    
    tableA = 
    PitchingDF() %>% 
      group_by(Pitch) %>% 
      summarise(
        "#" = n(),
        Usage = percent(n()/length(.$Pitch)),#3
        Max = floor(max(Velo, na.rm = TRUE)) %>% as.integer(),
        Avg = floor(mean(Velo, na.rm = TRUE)) %>% as.integer(),
        Spin = mean(Spin, na.rm = T) %>% as.integer(),
        Tilt = Tilt %>% as.POSIXct(format = '%H:%M', tz = 'UTC') %>%
          as.numeric() %>% mean(na.rm = T) %>%
          as.POSIXct(origin = '1970-01-01', tz = 'UTC') %>%
          format(format = "%k:%M", tz = 'UTC'),
        HB = mean(HB, na.rm = T) %>% round(2),
        IVB = mean(IVB, na.rm = T) %>% round(2),
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
        tableA$Pitch == "SP" ~ '#ddb33a',
        tableA$Pitch == "CT" ~ '#933f2c',
        tableA$Pitch == "CH" ~ '#1dbe3a',
        tableA$Pitch == "SL" ~ '#c3bd0e',
        tableA$Pitch == "CB" ~ '#00d1ed',
        tableA$Pitch == "KC" ~ '#854cb5'
      )) %>% 
      row_spec(row = 0, color = "white", background = "orange") %>% HTML()
  })
  
  #### . > Pitcher Overview Metrics Table ####
  output$PitcherMetricsTablePitcherOverview = renderUI({
    
    pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")
    
    tableA = 
      PitcherDFOverview() %>% 
      group_by(Pitch) %>% 
      summarise(
        "#" = n(),
        Usage = percent(n()/length(.$Pitch)),#3
        Max = floor(max(Velo, na.rm = TRUE)) %>% as.integer(),
        Avg = floor(mean(Velo, na.rm = TRUE)) %>% as.integer(),
        Spin = mean(Spin, na.rm = T) %>% as.integer(),
        Tilt = Tilt %>% as.POSIXct(format = '%H:%M', tz = 'UTC') %>%
          as.numeric() %>% mean(na.rm = T) %>%
          as.POSIXct(origin = '1970-01-01', tz = 'UTC') %>%
          format(format = "%k:%M", tz = 'UTC'),
        HB = mean(HB, na.rm = T) %>% round(2),
        IVB = mean(IVB, na.rm = T) %>% round(2),
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
        tableA$Pitch == "SP" ~ '#ddb33a',
        tableA$Pitch == "CT" ~ '#933f2c',
        tableA$Pitch == "CH" ~ '#1dbe3a',
        tableA$Pitch == "SL" ~ '#c3bd0e',
        tableA$Pitch == "CB" ~ '#00d1ed',
        tableA$Pitch == "KC" ~ '#854cb5'
      )) %>% 
      row_spec(row = 0, color = "white", background = "orange") %>% HTML()
  })
  
  
  
  #### Pitch Stats ####
  #### . > Pitcher Dashboard Stats Table ####
  output$PitcherStatsTablePitcherDashboard = renderUI({
    
    pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")
    
    tableA = PitchingDF() %>% 
      group_by(Pitch) %>% 
      summarise(
        '#' = n(),
        CStrk = length(which(PitchCall == 'StrikeCalled')),
        Swing = length(which(!PitchCall %in% c('StrikeCalled', 'HitByPitch', 'Ball'))),
        Whiff = length(which(PitchCall == 'StrikeSwinging')),
        'Strk%' = percent(length(which(!PitchCall %in% c('Ball', 'HitByPitch', 'InPlay')))/n()),
        'Whiff%' = percent(Whiff/Swing),
        'CSW%' = percent((CStrk+Whiff)/n()),
        AvgEV = mean(ExitSpeed, na.rm = TRUE) %>% round(),
        'Hard%' = percent(length(which(ExitSpeed > 90))/n()),
        FP = length(which(Count == '0-0')),
        'FPStrk%' = percent(length(which(Count == '0-0' & !PitchCall %in% c('Ball', 'HitByPitch', 'InPlay')))/FP)
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
        tableA$Pitch == "SP" ~ '#ddb33a',
        tableA$Pitch == "CT" ~ '#933f2c',
        tableA$Pitch == "CH" ~ '#1dbe3a',
        tableA$Pitch == "SL" ~ '#c3bd0e',
        tableA$Pitch == "CB" ~ '#00d1ed',
        tableA$Pitch == "KC" ~ '#854cb5'
      )) %>% 
      row_spec(row = 0, color = "white", background = "orange") %>% HTML()
  })
  
  #### . > Pitcher Overview Stats Table ####
  output$PitcherStatsTablePitcherOverview = renderUI({
    
    pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")
    
    tableA = PitcherDFOverview() %>% 
      group_by(Pitch) %>% 
      summarise(
        '#' = n(),
        CStrk = length(which(PitchCall == 'StrikeCalled')),
        Swing = length(which(!PitchCall %in% c('StrikeCalled', 'HitByPitch', 'Ball'))),
        Whiff = length(which(PitchCall == 'StrikeSwinging')),
        'Strk%' = percent(length(which(!PitchCall %in% c('Ball', 'HitByPitch', 'InPlay')))/n()),
        'Whiff%' = percent(Whiff/Swing),
        'CSW%' = percent((CStrk+Whiff)/n()),
        AvgEV = mean(ExitSpeed, na.rm = TRUE) %>% round(),
        'Hard%' = percent(length(which(ExitSpeed > 90))/n()),
        FP = length(which(Count == '0-0')),
        'FPStrk%' = percent(length(which(Count == '0-0' & !PitchCall %in% c('Ball', 'HitByPitch', 'InPlay')))/FP)
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
        tableA$Pitch == "SP" ~ '#ddb33a',
        tableA$Pitch == "CT" ~ '#933f2c',
        tableA$Pitch == "CH" ~ '#1dbe3a',
        tableA$Pitch == "SL" ~ '#c3bd0e',
        tableA$Pitch == "CB" ~ '#00d1ed',
        tableA$Pitch == "KC" ~ '#854cb5'
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
        K = length(which(PAOutcome == 'Strikeout')),
        Walks = length(which(PAOutcome == "Walk")),
        HBP = length(which(PitchCall == 'HitByPitch')),
        SF = length(which(PlayResult == 'Sacrifice')),
        H = length(which(PitchCall == 'InPlay' & PlayResult != 'Out')), #10
        PA = length(which(Count == '0-0')),
        AB = length(which(PitchCall == 'InPlay' & !PlayResult %in% c('Sacrifice'))) + K,
        AVG = sprintf((H/AB), fmt = '%#.3f'),
        
         
      )
    #%>%.[, c(1,2,3,5,7,9,10)]
  }, striped = TRUE, bordered = TRUE)
    
  
  # End Update Inputs
  
  #### Pitch Movement Plot ####
  #### . > Pitch Movement Dashboard ####
  output$PitchMovementPlotMain = renderPlotly({
    
    p = PitchingDF()
    
    hand = p$PitcherThrows[1]
    
    x1 = ifelse(hand == "Left", -8, 8)
    
    y1 = abs(3*tan(mean(p$ArmRad, na.rm = T)))
    
    a = list(x = x1, y = y1, text = paste(paste(round(mean(PitchingDF()$ArmDeg, na.rm=T)),"º", sep = ""),"Arm Angle"), 
             showarrow = F, yshift = 10)
    
    fig = plot_ly(PitchingDF(), color = ~Pitch, colors = pcolors) %>% 
      add_trace(x = ~HB, y = ~IVB, type = 'scatter', mode = 'markers',
                marker = list(size = 8, line = list(color = 'black',width = 1)),
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
        annotations = a,
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
                      traceorder = 'normal'),
        shapes = list(
          list(type = 'line', x0 = 0, x1 = x1, y0 = 0, y1 = y1, line = list(dash = 'dash'))
        )
        
      )
  })
  
  #### . > Pitch Movement Tab ####
  output$PitchMovementPlotSub = renderPlotly({
    
    fig = plot_ly(PitchingDF(), color = ~Pitch, colors = pcolors, source = 'PMB') %>% 
      add_trace(x = ~HB, y = ~IVB, type = 'scatter', mode = 'markers',
                marker = list(size = 8, line = list(color = 'black',width = 1)), # ADD COMMA BACK HERE
                #text = ~paste(Pitch,
                #              '<br>HB:', round(HB, 1),'in',
                #              '<br>VB:', round(IVB, 1),'in',
                #              '<br>Spin:',round(PitchingDF()$Spin),'RPM',
                #              '<br>Ext:', round(PitchingDF()$Extension,2), 'ft'
                #              ),
                #hoverinfo = 'text'
                hovertemplate = "HB:%{x:.1f}<br>VB:%{y:.1f}"
      )
    config(fig, displaylogo = F, modeBarButtonsToRemove = c("zoomin2d", 'zoomOut2d', 'lasso2d', 'autoscale2d', 'pan2d')) %>% 
      layout(
        xaxis = list(range = c(-30,30)),
        yaxis = list(range = c(-30,30)),
        title = "Pitch Movement",
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
  
  #### . > Pitch Movement Overview ####
  
  output$PitchMovementPlotOverview = renderPlotly({
    
    fig = plot_ly(PitcherDFOverview(), color = ~Pitch, colors = pcolors) %>% 
      add_trace(x = ~HB, y = ~IVB, type = 'scatter', mode = 'markers',
                marker = list(size = 8, line = list(color = 'black',width = 1)))
    config(fig, staticPlot = T) %>% 
      layout(
        xaxis = list(range = c(-30,30)),
        yaxis = list(range = c(-30,30)),
        title = "Pitch Movement",
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
  
  
  #### Release Angle ####
  output$PitchReleaseAngle = renderPlotly({
    
    fig = plot_ly(PitchingDF(), color = ~Pitch, colors = pcolors) %>% 
      add_trace(x = ~HorzRelAngle, y = ~VertRelAngle, type = 'scatter', mode = 'markers',
                marker = list(size = 8, line = list(color = 'black', width = 1)))
    config(fig) %>% 
      layout(
        showlegend = F,
        title = "Pitch Release Angle"
      )
    
  })
  
  #### Data Point Display ####
  
  #### . > Movement Data ####
  output$PitchMovementData = renderTable({
    
    d = event_data('plotly_selected', source = 'PMB')
    
    
    MovementData = game %>% select(Pitcher, Pitch, HB, IVB, Count, Outs, 
                                   PitchCall, PAOutcome, HitType, PlayResult)
 
    filter(MovementData, HB %in% d[,3])
  })
  
  #### . > Strike Zone Data ####
  output$StrikeZoneData = renderTable({
    d = event_data('plotly_selected', source = 'SZP')
    
    
    SZData = filter(game, PlateLocSide %in% d[,3])
    
    SZData %>% select(Pitcher, Pitch, HB, IVB, Count, Outs, PitchCall, PAOutcome, 
                    HitType, PlayResult)
    
    
    
  })
  
  # Strike Zone Plot ----
  
  #### . > SZ Dashboard ####
  output$StrikeZonePlotMain = renderPlotly({
    
    fig = plot_ly(PitchingDF(), color = ~Pitch, colors = pcolors) %>% 
      add_trace(x = ~PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers',
                marker = list(size = 8, opacity = 1, line = list(color = 'black',width = 1)), fill = 'none',
                #text = ~paste(
                #  PitchingDF()$PitchCall,
                #  "<br>",PitchingDF()$HitType,
                #  "<br>",PitchingDF()$PlayResult
                #              ), 
                #hoverinfo = 'text'
                text = ~PitchCall,
                customdata = paste0(PitchingDF()$HitType, "\n", PitchingDF()$PlayResult),
                hovertemplate = "%{text}<extra>%{customdata}</extra>"
                )
    fig = fig %>% 
    config(fig, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-3,3), showgrid = T, zeroline = F, title = NA),
        yaxis = list(range = c(-0.5,5), showgrid = T, zeroline = F, title = NA),
        title = "Strike Zone",
        showlegend = F,
        shapes = list(
          list(
          type = "rect",x0 = -0.708,x1 = 0.708,y0 = 1.5,y1 = 3.5, layer = 'below'
          ),
          #Draw Plate
          list(
            type = "line",x0 = -0.708,x1 = 0.708,y0 = 0.15,y1 = 0.15, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = -0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          #End Draw Plate
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
  
  #### . > SZ Tab ####
  output$StrikeZonePlotSub = renderPlotly({
    
    fig = plot_ly(PitchingDF(), color = ~Pitch, colors = pcolors, source = 'SZP') %>% 
      add_trace(x = ~PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers',
                marker = list(size = 8, line = list(color = 'black',width = 1)
                )
      )
    config(fig) %>% 
      layout(
        xaxis = list(range = c(-3,3), showgrid = T, zeroline = F),
        yaxis = list(range = c(-0.5,5), showgrid = T, zeroline = F),
        title = "Strike Zone",
        showlegend = F,
        shapes = list(
          list(
            type = "rect",x0 = -0.708,x1 = 0.708,y0 = 1.5,y1 = 3.5, layer = 'below'
          ),
          #Draw Plate
          list(
            type = "line",x0 = -0.708,x1 = 0.708,y0 = 0.15,y1 = 0.15, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = -0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          #End Draw Plate
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
  
  #### . > SZ Overview ####
  output$StrikeZonePlotOverview = renderPlotly({
    
    fig = plot_ly(PitcherDFOverview(), color = ~Pitch, colors = pcolors, source = 'SZP') %>% 
      add_trace(x = ~PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers',
                marker = list(size = 8, line = list(color = 'black',width = 1)
                )
      )
    config(fig, staticPlot = T) %>% 
      layout(
        xaxis = list(range = c(-3,3), showgrid = T, zeroline = F),
        yaxis = list(range = c(-0.5,5), showgrid = T, zeroline = F),
        title = "Strike Zone",
        showlegend = F,
        shapes = list(
          list(
            type = "rect",x0 = -0.708,x1 = 0.708,y0 = 1.5,y1 = 3.5, layer = 'below'
          ),
          #Draw Plate
          list(
            type = "line",x0 = -0.708,x1 = 0.708,y0 = 0.15,y1 = 0.15, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = -0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          #End Draw Plate
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
  
  
  #### Release Plot ####
  
  #### . > Release Plot Dashboard ####
  output$PitcherReleasePlotMain = renderPlotly({
    
    fig = plot_ly(PitchingDF()) %>% 
      add_trace(x = ~RelSide, y = ~RelHeight, color = ~Pitch, colors = ~pcolors, type = 'scatter', mode = 'markers', 
                marker = list(size = 8, line = list(color = 'black', width = 1)))
      config(fig, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-5,5)),
        yaxis = list(range = c(4, 7)),
        title = "Pitch Release Points",
        showlegend = F,
        shapes = list(
          list(
          type = 'line',x0 = -5,x1 = 5,y0 = 5,y1 = 5,layer = 'below'
          )
      ))
  })
  
  
  #### . > Release Plot Tab ####
  output$PitcherReleasePlotSub = renderPlotly({
    fig = plot_ly(PitchingDF()) %>% 
      add_trace(x = ~RelSide, y = ~RelHeight, color = ~Pitch, colors = ~pcolors, type = 'scatter', mode = 'markers', 
                marker = list(size = 8, line = list(color = 'black', width = 1))) 
    config(fig) %>% 
      layout(
        xaxis = list(range = c(-5,5)),
        yaxis = list(range = c(0, 7)),
        title = "Pitch Release Points",
        showlegend = F,
        shapes = list()
      )
  })
  
  #### . > Release Plot Overview ####
  output$PitcherReleasePlotOverview = renderPlotly({
    fig = plot_ly(PitcherDFOverview()) %>% 
      add_trace(x = ~RelSide, y = ~RelHeight, color = ~Pitch, colors = ~pcolors, type = 'scatter', mode = 'markers', 
                marker = list(size = 8, line = list(color = 'black', width = 1))) 
    config(fig, staticPlot = T) %>% 
      layout(
        xaxis = list(range = c(-5,5)),
        yaxis = list(range = c(0, 7)),
        title = "Pitch Release Points",
        showlegend = F,
        shapes = list()
      )
  })
 
  
  #### Extension Plot ####
  
  output$PitcherExtensionPlot = renderPlotly({
    
    fig = plot_ly(PitchingDF()) %>% 
      add_trace(x = ~Extension*12, y = ~RelHeight*12, color = ~Pitch, colors = ~pcolors,
                marker = list(size = 8, line = list(color = 'black', width = 1)))
    config(fig) %>% 
      layout(
        xaxis = list(range = c(0,90), title = 'Extension (in)'),
        yaxis = list(range = c(0,80), title = 'Release Height (in)'),
        showlegend = F,
        title = 'Extension'
      )
  })
  
  
  # Heatmap ----
  output$PitcherHeatmap = renderPlotly({
    fig = 
    plot_ly(PitchingDF(), x = ~PlateLocSide, y = ~PlateLocHeight) %>% 
      add_histogram2dcontour(
        #nbinsx = 25, 
         #                    nbinsy = 25, 
                             histfunc = 'count', histnorm = 'density', colorscale = 'hot') 
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
   
  # Pitch By Pitch ----
  output$PBPOverview = renderTable({
    
    table = 
    PitchingDF() %>% 
      select(PitchNo, Pitcher, BatterSide, Inn, Count, Outs, Pitch,
             PitchCall, PAOutcome, HitType, PlayResult, Velo, Spin, HB,
             IVB, ExitSpeed, Angle, Distance)
    table
    
  })
  
  #### Batter Metrics Table ####
  output$BatterMetricsTable = renderUI({
    
    tableA = BattingDF() %>% 
      filter(BatterTeam == 'MER_BEA') %>% 
      group_by(PitcherThrows) %>% 
      summarise(
        P = n(),
        'Max EV' = max(ExitSpeed, na.rm = T) %>% round(),
        'Avg EV' = mean(ExitSpeed, na.rm = T) %>% round(),
        'Avg LA' = mean(Angle, na.rm = T) %>% round(1),
        'Max Hit Spin' = max(HitSpinRate, na.rm = T) %>% round(),
        'Avg Hit Spin' = mean(HitSpinRate, na.rm = T) %>% round(),
        "Avg Distance" = mean(Distance, na.rm = T) %>% round(),
        "Avg Hang Time" = mean(HangTime, na.rm = T) %>% round(1)
      ) 
    tableA %>% 
      kable(format = 'html', align = 'c') %>% kable_styling(font_size = 15) %>% 
      kable_styling(bootstrap_options = 'bordered') %>% 
      row_spec(row = 0, color = "white", background = "orange") %>% HTML()
    
  })
  
  #### Batter Stats Table ####
  output$BatterStatsTable = renderUI({
    
    tableA = BattingDF() %>% 
      filter(BatterTeam == "MER_BEA") %>% 
      group_by(PitcherThrows) %>% 
      summarise(
        P = n(),
        PA = length(which(Count == '0-0')),
        H = length(which(PitchCall == 'InPlay' & !PlayResult %in% 
                           c('Out', 'Sacrifice', 'FieldersChoice', 'Error', 'Undefined'))),
        '2B' = length(which(PlayResult == 'Double')),
        '3B' = length(which(PlayResult == 'Triple')),
        'HR' = sum(PlayResult == 'HomeRun'),
        K = length(which(PAOutcome == 'Strikeout')),
        BB = length(which(PAOutcome == 'Walk')),
        HBP = length(which(PitchCall == 'HitByPitch')),
        BIP = sum(PitchCall == 'InPlay'),
        H = sum(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun')),
        XBH = sum(PlayResult %in% c('Double', 'Triple', 'HomeRun')),
        R = sum(RunsScored),
        BAA = sprintf(H/(PA-BB-HBP-sum(PlayResult == 'Sacrifice')), fmt = '%#.3f')
      )
    
   tableA %>% 
     kable(format = 'html', align = 'c') %>% kable_styling(font_size = 15) %>% 
     kable_styling(bootstrap_options = 'bordered') %>% 
     row_spec(row = 0, color = "white", background = "orange") %>% HTML()
    
  })
  
  #### Pitcher Name Output ####
  output$PitcherNamePitcherOverview = renderText({input$pitcherPitcherOverview})
  
  
  #### Batter Strikezone Plot ####
  output$BatterStrikeZonePlot = renderPlotly({
    fig = plot_ly(BattingDF(), color = ~Pitch, colors = pcolors) %>% 
      add_trace(x = ~-PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers',
                marker = list(size = 8, opacity = 1, line = list(color = 'black',width = 1)), fill = 'none')
    fig = fig %>% 
      config(fig, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-3,3), showgrid = T, zeroline = F, title = NA),
        yaxis = list(range = c(-0.5,5), showgrid = T, zeroline = F, title = NA),
        title = "Strike Zone",
        showlegend = F,
        shapes = list(
          list(
            type = "rect",x0 = -0.708,x1 = 0.708,y0 = 1.5,y1 = 3.5, layer = 'below'
          ),
          #Draw Plate
          list(
            type = "line",x0 = -0.708,x1 = 0.708,y0 = 0.15,y1 = 0.15, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = -0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          #End Draw Plate
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
   

  
  }
