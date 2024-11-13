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
library(kableExtra)

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
          column(3, checkboxGroupInput("ball", "Balls", c('0','1','2','3'))),
          column(3, checkboxGroupInput("strike", "Strikes", c('0','1','2'))),
          column(3, checkboxGroupInput("outs", "Outs", c('0','1','2'))),
          column(3, checkboxGroupInput('batterhand', 'BH', choiceNames = c('L','R'), choiceValues = c('Left','Right'))),
        ),
      fluidRow(
          column(6, checkboxGroupInput('pcall', 'Pitch Call', c(0))),
          column(6, checkboxGroupInput('hittype', 'Hit Type', c('0'))),
        ),
        fluidRow(
          column(4, checkboxGroupInput('pitch', 'Pitch', c(0))),
        )
    ), # End Sidebar
    navset_tab(
      nav_panel("Pitcher Overview",
          page_fillable(
            layout_columns(col_widths = 12,
              #tableOutput("PitcherMetricsTable")
              htmlOutput('PitcherMetricsTable'),
              htmlOutput('PitcherStatsTable')
            ),
            layout_columns( col_widths = 4,
              plotlyOutput('PitchMovementPlotMain'),
              plotlyOutput('StrikeZonePlotMain'),
              plotlyOutput('PitcherReleasePlotMain')
            ),
            ) # Page Fillable
        ), # Nav Panel Pitcher Overview
      nav_panel('Pitch by Pitch Overview',
                tableOutput("PBPOverview")
        ),
      nav_panel('Pitch Locations',
                h1(""),
                layout_columns(col_widths = 6,
                               plotlyOutput('StrikeZonePlotSub', width = '700px', height = '500px')               
                ),
                tableOutput('StrikeZoneData')  
      ),
      nav_panel('Pitch Movement & Release Angle',
                h1(""),
            layout_columns(col_widths = 6,
                plotlyOutput('PitchMovementPlotSub', height = '500px'),
                plotlyOutput('PitchReleaseAngle', height = '500px')
            ),
            tableOutput('PitchMovementData')
      ),
      nav_panel('Release Point & Extension',
                h1(""),
              layout_columns(col_widths = 6,
                plotlyOutput('PitcherReleasePlotSub'),
                plotlyOutput('PitcherExtensionPlot')
              )
                ),
      nav_panel("Pitcher Slicing",
        fluidRow(
          selectInput('slicerA', 'SlicerA', c('Pitch', 'BatterSide', 'Count')),
          selectInput('slicerB', 'SlicerB', c("None" = "", 'Pitch', 'BatterSide', 'Count'), selected = "", selectize = TRUE),
        ),
          layout_columns(col_widths = 6,
            tableOutput('PitcherSlicerTable')
          ),
    ), # NavTab Pitcher Slicing
      nav_panel("Heat Map",
                tags$h1("Strike Zone Heatmap", align = 'center'),
                layout_columns(col_widths = 3,
                               "",
                               plotlyOutput('PitcherHeatmap', height = '500px', width = '700px')
                )
                
        ), # Nav Panel Heatmap End
      
  ), # Navset Tab End
) # Page Sidebar End
        
  
  ),
  # Hitting Page ----
  nav_panel("Hitting", "Page C content"),
  # Game Reports Page ----
  nav_panel("Game Reports", "game reports content"),
  # Umpire Reports Page ----
  nav_panel("Umpire Reports", "Umpire reports  content"),
  
)
)
