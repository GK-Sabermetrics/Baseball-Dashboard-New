# Code Testing Space
library(tidyverse)
library(scales)


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

