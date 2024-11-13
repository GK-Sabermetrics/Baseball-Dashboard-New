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

dataA$Top.Bottom[grepl("Top", dataA$Top.Bottom)] = "T"
dataA$Top.Bottom[grepl("Bottom", dataA$Top.Bottom)] = "B"

dataA =   
dataA %>% mutate(
  Count = paste(Balls, Strikes, sep = "-"),
  Inn = paste(Top.Bottom, Inning, sep = " "),
  .after = "Outs"
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




