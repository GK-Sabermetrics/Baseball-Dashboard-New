library(shiny)
library(tidyverse)
library(rlang)
library(bslib)
library(bsicons)
library(shinythemes)
library(scales)

# Define UI
page_navbar(
  # Home Page ----
  nav_panel(
    "Home",
    accordion(
      open = FALSE,
      accordion_panel(title = 'Updates', icon = bs_icon('bell')),
      accordion_panel(title = 'Info', icon = bs_icon('info-square'))
    ),
  ),
  # Team Pitching Page ----
  nav_panel("Team Pitching", "Team Pitching Content"),
  # Team Hitting Page ----
  nav_panel("Team Hitting", "Team Hitting Content"),
  # Pitching Page ----
  nav_panel("Pitching",
    layout_sidebar(
    sidebar = sidebar(
      width = 250,
      open = 'open',
      title = 'Filters',
      div(fluidRow(
        column(6, checkboxGroupInput("ball", "Balls", c('a'))),
        column(6, checkboxGroupInput("strike", "Strikes", c('b')))
      )),
    ),
    selectInput('slicer', 'Slicer', c('TaggedPitchType', 'BatterSide', 'Count')),
    tableOutput('tableA')
  ), 
        
  
  ),
  # Hitting Page ----
  nav_panel("Hitting", "Page C content"),
  # Game Reports Page ----
  nav_panel("Game Reports", "Game reports content"),
  # Umpire Reports Page ----
  nav_panel("Umpire Reports", "Umpire reports  content"),
  title = "MU Baseball",
  id = "page",
)
