# Code Testing Space
library(tidyverse)
library(scales)
library(htmlTable)


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

dataA =
dataA %>% mutate(
  Count = paste(Balls, Strikes, sep = "-"), .after = "Outs"
)

pt =
dataA %>% 
  group_by(BatterSide) %>% 
  summarise(
    Total = n(),
    '%' = percent(n()/length(.$TaggedPitchType)),
    Strikes = length(which(PitchCall != 'BallCalled')),
    StrikeP = percent(Strikes/Total),
    Balls = length(which(PitchCall == 'BallCalled')),
    BallP = percent(Balls/Total)
  )

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


