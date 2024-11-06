library(shiny)
library(tidyverse)
library(rlang)
library(bslib)
library(bsicons)
library(scales)
library(condformat)
library(htmltools)
library(DT)
library(plotly)

# Define UI
fluidPage(theme = bs_theme(version = 5, bootswatch = 'united'),
page_navbar(
  title = "MU Baseball",
  # Home Page ----
#  nav_panel(
#    "Home",
#    accordion(
#      open = FALSE,
#      accordion_panel(title = 'Updates', icon = bs_icon('bell')),
#      accordion_panel(title = 'Info', icon = bs_icon('info-square'))
#    ),
#  ),
#  # Team Pitching Page ----
#  nav_panel("Team Pitching", "Team Pitching Content"),
#  # Team Hitting Page ----
#  nav_panel("Team Hitting", "Team Hitting Content"),
  # Pitching Page ----
  nav_panel("Pitching",
    page_sidebar(
    sidebar = sidebar(
      width = 250,
      open = 'open',
      title = 'Filters',
        fluidRow(
          selectInput('pitcher', 'Pitcher', c("all"))
        ),
        fluidRow(
        column(6, checkboxGroupInput("ball", "Balls", c('0','1','2','3'))),
        column(6, checkboxGroupInput("strike", "Strikes", c('b')))
      ),
    ),
    navset_tab(
      nav_panel("Pitcher Table",
                page_fillable(
                  layout_columns( col_widths = 4,
                    #condformatOutput('PitcherStandingsTable'),
                    plotlyOutput('PitchMovementPlot'),
                    plotlyOutput('StrikeZonePlot'),
                  ),
                  layout_columns(
                    tableOutput("PitcherMetricsTable") 
                  )
                )
                  
                
                ),
      nav_panel("Pitcher Slicing",
    fluidRow(
      selectInput('slicerA', 'SlicerA', c('Pitch', 'BatterSide', 'Count')),
      selectInput('slicerB', 'SlicerB', c("None" = "", 'Pitch', 'BatterSide', 'Count'), selected = "", selectize = TRUE),
    ),
    fluidRow(
      column(width = 12, tableOutput('PitcherSlicerTable'))
    ),
    ), # NavTab Pitcher Table
      
  ),
    )
        
  
  ),
  # Hitting Page ----
  nav_panel("Hitting", "Page C content"),
  # Game Reports Page ----
  nav_panel("Game Reports", "game reports content"),
  # Umpire Reports Page ----
  nav_panel("Umpire Reports", "Umpire reports  content"),
  
)
)
