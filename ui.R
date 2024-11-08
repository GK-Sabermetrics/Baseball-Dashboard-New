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
  # Team Pitching Page ----
#  nav_panel("Team Pitching", "Team Pitching Content",
#            condformatOutput('PitcherStandingsTable'),
#            ),
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
        column(4, checkboxGroupInput("ball", "Balls", c('0','1','2','3'))),
        column(4, checkboxGroupInput("strike", "Strikes", c('0','1','2'))),
        column(4, checkboxGroupInput("outs", "Outs", c('0','1','2'))),
      ),
        fluidRow(
          column(4, checkboxGroupInput('pcall', 'Pitch Call', c(0)))
        )
    ),
    navset_tab(
      nav_panel("Pitch Metrics",
                page_fillable(
                  layout_columns(
                    tableOutput("PitcherMetricsTable") 
                  ),
                  layout_columns( col_widths = 4,
                    plotlyOutput('PitchMovementPlot'),
                    plotlyOutput('StrikeZonePlot'),
                    plotlyOutput('PitcherReleasePlot')
                  ),
                  
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
    ), # NavTab Pitcher Slicing
      nav_panel("Heat Map",
                tags$h1("Strikezone Heatmap"),
                layout_columns(col_widths = 6,
                               plotlyOutput('PitcherHeatmap', height = '500px', width = '700px')
                               )
                
                )
      
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
