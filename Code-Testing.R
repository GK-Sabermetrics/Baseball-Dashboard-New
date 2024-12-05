# Code Testing Space
library(tidyverse)
library(scales)
library(htmlTable)

#### Pre Load Data ####
data = read.csv("Data/Fall Scrimmage Data copy.csv")

#### Data Manipulation ####
# Add Pitch Column and Count column
game =
  filter(data, TaggedPitchType != 'Other' & PitcherTeam == "MER_BEA") %>% 
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
    ArmDeg = ArmRad * (180/pi)
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


#### Old Data Manipulation ####
data = read.csv("Data/20241023-MercerUniversity-Private-1_unverified.csv")

dataA = data

dataA$TaggedPitchType[grepl("Fastball", dataA$TaggedPitchType)] = "FB"
dataA$TaggedPitchType[grepl("TwoSeamFastBall", dataA$TaggedPitchType)] = "2SFB"
dataA$TaggedPitchType[grepl("Sinker", dataA$TaggedPitchType)] = "SI"
dataA$TaggedPitchType[grepl("Cutter", dataA$TaggedPitchType)] = "CU"
dataA$TaggedPitchType[grepl("Splitter", dataA$TaggedPitchType)] = "SP"
dataA$TaggedPitchType[grepl("ChangeUp", dataA$TaggedPitchType)] = "CH"
dataA$TaggedPitchType[grepl("Slider", dataA$TaggedPitchType)] = "SL"
dataA$TaggedPitchType[grepl("Curveball", dataA$TaggedPitchType)] = "CB"

dataA$Top.Bottom[grepl("Top", dataA$Top.Bottom)] = "T"
dataA$Top.Bottom[grepl("Bottom", dataA$Top.Bottom)] = "B"

dataA =   
dataA %>% mutate(
  Count = paste(Balls, Strikes, sep = "-"), .after = 'Outs',
  Inn = paste(Top.Bottom, Inning, sep = " "), .after = "Inning"
)


#### Conditional Format Testing ####

library(condformat)

pt = dataA %>% 
  group_by(Pitcher) %>% 
  summarise(
    Pitches = n()
  ) %>% as.data.frame()

rownames(pt) = FALSE

dt = data %>% select(Pitcher, Pitch, RelSpeed)

condformat2widget(condformat(pt) %>% rule_fill_gradient2(Pitches)) %>% theme_htmlWidget(rownames = FALSE)

condformat(pt) %>% rule_fill_gradient(Pitches) %>% theme_htmlTable(rnames = FALSE)

htmlTable(condformat(pt) %>% rule_fill_gradient2(Balls))

summary(data$RelSpeed)

#### Plotly ####

library(plotly)

cols = c('Fastball' = '#d22d49', 'TwoSeamFastBall' = '#93afd4', 'ChangeUp' = '#1dbe3a', 
         'Slider' = '#c3bd0e', 'Curveball' = '#00d1ed', 'Cutter' = '#933f2c', 
         'Sinker' = '#de6a04', 'Splitter' = '#DDB33A', 'Four-Seam' = '#d22d49', 'KnuckleBall' = '#854cb5')

bks = c('Four-Seam','Fastball','TwoSeamFastBall','Sinker','Cutter','Splitter','ChangeUp','Curveball','Slider', 'KnuckleBall')
lbs = c('FB','FB','2SFB','SI','CU','SP','CH','CB','SL','KC')


testData = data

plot_ly(testData, type = 'scatter', x = ~HorzBreak, y = ~InducedVertBreak,
        marker = list(size = 15, color = ~cols[Pitch])) %>% 
  layout(
    xaxis = list(range = c(-30,30), showgrid = F),
    yaxis = list(range = c(-30,30), showgrid = F)
    )

plot_ly(testData, type = 'scatter', mode = 'markers', x = ~PlateLocSide, y = ~PlateLocHeight, color = ~PitchCall ) %>% 
  layout(
    xaxis = list(range = c(-3,3), zeroline = FALSE),
    yaxis = list(range = c(-0.5,5), zeroline = FALSE),
    shapes = list(
      type = "rect",
      x0 = -0.708,
      x1 = 0.708,
      y0 = 1.5,
      y1 = 3.5
    )
  )

plot_ly(data, x = ~PlateLocSide, y = ~PlateLocHeight, type = 'heatmap')



fig <- plot_ly(data, x = ~PlateLocSide, y = ~PlateLocHeight)
fig2 <- subplot(
  fig %>% add_markers(alpha = 0.2),
  fig %>% add_histogram2d()
)

fig2


plot_ly(data, x = ~PlateLocSide, y = ~PlateLocHeight) %>% 
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

gamevelo = 
  dataA %>% group_by(TaggedPitchType, Inning) %>% 
  summarise(Avg = mean(RelSpeed, na.rm = TRUE))


plot_ly(data, x = ~HorzRelAngle, y = ~VertRelAngle, color = ~TaggedPitchType, mode = 'markers')


game %>% mutate(
  KorBB = 
  case_when(
    KorBB == "Strikeout" ~ "Strikeout",
    KorBB == 'Walk' ~ 'Walk'
    #KorBB == "Undefined" ~ NA
            )
) %>% view()

test = head(game, 5)

paste(test$Pitcher, 'threw a', test$TaggedPitchType, "for a", test$PitchCall,
      ifelse(test$TaggedHitType == 'Undefined', "",paste("resulting in a",test$TaggedHitType))) %>% 
  as.data.frame()

separate(test$PitchCall, "")

sapply(test$Pitcher, function(name) {
  parts <- strsplit(name, ", ")[[1]]  # Split by comma and space
  name = paste(parts[2], parts[1])  # Reorder to "First Last"
  print(name)
})

cols = c('Fastball' = '#d22d49', 'TwoSeamFastBall' = '#93afd4', 'ChangeUp' = '#1dbe3a', 
         'Slider' = '#c3bd0e', 'Curveball' = '#00d1ed', 'Cutter' = '#933f2c', 
         'Sinker' = '#de6a04', 'Splitter' = '#DDB33A', 'Four-Seam' = '#d22d49', 'KnuckleBall' = '#854cb5')

pitch_order <- c("FB", "2SFB", "SI", "CU", "CH", "SL", "CB")

testtable = 
dataA %>% 
  group_by(TaggedPitchType) %>% 
  summarise(Pitches = n()) %>% 
  mutate(TaggedPitchType = factor(TaggedPitchType, levels = pitch_order)) %>%
  arrange(TaggedPitchType)
  
testtable %>%  
kable(format = 'html') %>% kable_styling(font_size = 10, position = 'center') %>% 
  column_spec(1, border_left = TRUE, color = 'white',background = case_when(
    testtable$TaggedPitchType == "FB" ~ '#d22d49',
    testtable$TaggedPitchType == "2SFB" ~ '#93afd4',
    testtable$TaggedPitchType == "SI" ~ '#de6a04',
    testtable$TaggedPitchType == "CU" ~ '#933f2c',
    testtable$TaggedPitchType == "CH" ~ '#1dbe3a',
    testtable$TaggedPitchType == "SL" ~ '#c3bd0e',
    testtable$TaggedPitchType == "CB" ~ '#00d1ed',
    TRUE ~ 'black'
  )) %>% 
  row_spec(row = 0, color = "white", background = "orange")



data %>% mutate(KorBB = recode(KorBB, Strikeout = 'Strikeout', Walk = 'Walk', Undefined = "")) %>% view()

# Arm Angle ----


library(plotly)

gametest = filter(game, Pitcher == 'Ackerman, Jess')

gametest = 
  gametest %>%
  mutate(
    ArmRad = atan2(RelHeight, RelSide),
    ArmDeg = ArmRad * (180/pi),
    ArmDeg180 = 90 - ArmDeg
  )

fig = plot_ly(gametest) %>% 
  add_trace(x = ~RelSide, y = ~RelHeight, color = ~Pitch, colors = ~pcolors, type = 'scatter', mode = 'markers', 
            marker = list(size = 8, line = list(color = 'black', width = 1))) 
config(fig, displayModeBar = T) %>% 
  layout(
    xaxis = list(range = c(-5,5)),
    yaxis = list(range = c(0, 7)),
    title = "Pitch Release Points",
    showlegend = F,
    shapes = list(
      #list(type = 'line',x0 = -5,x1 = 5,y0 = 5,y1 = 5,layer = 'below'),
      list(type = 'line', x0 = 0, x1 = 3, y0 = 0, y1 = 3*tan(mean(gametest$ArmRad, na.rm = T)), line = list(dash = 'dash'))
    )
  )

#### At Bat Testing ####

attach(gameTest)

gameTest = filter(game, Pitcher == 'Kalkbrenner, Craig')

PA = length(which(Count == "0-0"))

FC = length(which(PlayResult == 'FieldersChoice'))

H = length(which(PitchCall == 'InPlay'))

K = length(which(PAOutcome == 'Strikeout'))


H+K

gameTest %>% select(PitchCall, PAOutcome, HitType, PlayResult) %>% 
  filter(PAOutcome == "Strikeout")

sprintf(400/600, fmt = '%#.3f') %>% as.numeric() %>% str()

gameTest %>% 
  group_by(Date) %>% view()

OutsOnPlay

((sum(OutsOnPlay)+length(which(PAOutcome == 'Strikeout')))/3)

23.333 %% 1

testDF = c(24/3, 23/3, 22/3) %>% as.data.frame()

c(24/3, 23/3, 22/3) %>% as.data.frame()

ifelse(testDF$.%% 1 < .34, testDF$.-.2333333, ifelse(testDF$.%% 1 > .4, testDF$.-.4666666, 'N'))

ifelse(testDF$.%% 1 > .4, testDF$.-.4666666, ifelse(testDF$.%% 1 < .34, testDF$.--.2333333, 'N'))

ifelse(testDF$.%% 1 == 0, testDF$. + 0.0, ifelse(between(testDF$.%%1, .0, .34), testDF$.-.2333333, testDF$.-.4666666))

between(testDF$.%%1, , .4)

PP =
game %>% 
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
  ) %>% colnames() %>% as.data.frame()


PTable = 
game %>% 
  group_by(Pitcher) %>% 
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
  ) %>% .[, c(1,2,4,19,6,7,8,9,11,14,15,20:26)]

condformat(PTable) %>% 
  rule_fill_gradient(Pitches) %>% condformat2html()


data.frame(Student = c("Alice", "Bob", "Charlie"),
           Evaluation = c("Great", "Well done", "Good job!")) %>%
  condformat() %>%
  condformat2grob()

data$Pitcher[data$PitcherTeam == "MER_BEA"]

data$Pitcher

