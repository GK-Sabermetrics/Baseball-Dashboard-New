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
  nav_menu("Pitching",
    nav_panel('Pitcher Overview',
              layout_columns(col_widths = 6,
              selectInput('datePitcherOverview', 'Date', c('all')),
              selectInput('pitcherPitcherOverview', 'Pitcher', c('all'))
              ),
              htmlOutput('PitcherIndividualStats'),
              h1(textOutput('PitcherNamePitcherOverview')),
              h2("Pitch Metrics", align = 'center'),
              htmlOutput('PitcherMetricsTablePitcherOverview'),
              h2("Pitch Stats", align = 'center'),
              htmlOutput('PitcherStatsTablePitcherOverview'),
              layout_columns(col_widths = 4,
                plotlyOutput("PitchMovementPlotOverview"),
                plotlyOutput("StrikeZonePlotOverview"),
                plotlyOutput("PitcherReleasePlotOverview")
                             
                             )
              ),
    nav_panel('Pitcher Standings',
              htmlOutput('PitcherStandingsTable')
              ),
    nav_panel('Pitcher Dashboard',
    page_sidebar(
    sidebar = sidebar(
      width = 250,
      open = 'open',
      title = 'Filters',
      selectInput('seasonPitcherDashboard', 'Season (Not Yet Functional)', c('all')),
      selectInput('pitcherPitcherDashboard', 'Pitcher', c("all")),
      selectInput('datePitcherDashboard', 'Date', c("all")),
      fluidRow(
          column(3, checkboxGroupInput("ballPitcherDashboard", "Balls", c('0','1','2','3'))),
          column(3, checkboxGroupInput("strikePitcherDashboard", "Strikes", c('0','1','2'))),
          column(3, checkboxGroupInput("outsPitcherDashboard", "Outs", c('0','1','2'))),
          column(3, checkboxGroupInput('batterhandPitcherDashboard', 'BH', choiceNames = c('L','R'), choiceValues = c('Left','Right'))),
        ),
      fluidRow(
          column(6, checkboxGroupInput('pcallPitcherDashboard', 'Pitch Call', c(0))),
          column(6, checkboxGroupInput('hittypePitcherDashboard', 'Hit Type', c('0'))),
        ),
        fluidRow(
          column(4, checkboxGroupInput('pitchPitcherDashboard', 'Pitch', c(0))),
          column(4, checkboxGroupInput('paoutcomePitcherDashboard', 'PA Result', c(0))),
        )
    ), # End Sidebar
    navset_tab(
      nav_panel("Pitcher Overview",
          page_fillable(
            layout_columns(col_widths = 12,
              #tableOutput("PitcherMetricsTable")
              htmlOutput('PitcherMetricsTablePitcherDashboard'),
              htmlOutput('PitcherStatsTablePitcherDashboard')
            ),
            layout_columns(col_widths = 4,
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
), # Page Sidebar End
        
    )),

  # Hitting Page ----
  nav_menu("Batting",
    nav_panel('Batter Overview'),
    nav_panel('Batter Standings'),
    nav_panel('Batter Dashboard',
      page_sidebar(
        sidebar = sidebar(
          width = 250,
          open = 'open',
          title = 'Filters',
          selectInput('seasonBatterDashboard', 'Season (Not Yet Functional)', c(0)),
          selectInput('batterBatterDashboard', 'Batter', c(0)),
          selectInput('dateBatterDashboard', 'Date', c(0)),
          fluidRow(
            column(3, checkboxGroupInput('ballBatterDashboard', 'Balls', c(0,1,2,3))),
            column(3, checkboxGroupInput('strikeBatterDashboard', 'Strikes', c(0,1,2))),
            column(3, checkboxGroupInput('outsBatterDashboard', 'Outs', c(0,1,2))),
            column(3, checkboxGroupInput('pitcherthrowsBatterDashboard', 'PH', c('L','R')))
          ),
          fluidRow(
            column(6, checkboxGroupInput('pcallBatterDashboard', 'Pitch Call', c(0))),
            column(6, checkboxGroupInput('hittypeBatterDashboard', 'Hit Type', c('0'))),
          ),
          fluidRow(
            column(4, checkboxGroupInput('pitchBatterDashboard', 'Pitch', c(0))),
            column(4, checkboxGroupInput('paoutcomeBatterDashboard', 'PA Result', c(0))),
          )
        ), #### Main Batter Dashboard Page ####
      navset_tab(
        nav_panel("Batter Overview",
          htmlOutput('BatterMetricsTable'),
          htmlOutput('BatterStatsTable'),
          layout_columns(col_widths = 4, 
                         plotlyOutput('BatterStrikeZonePlot')
                         
          ),
        ),
        nav_panel("AB Pitch by Pitch"),
        nav_panel("Pitch Locations"),
        nav_panel("Contact Chart"),
        nav_panel("Spray Chart")
      )
      )
    )
  ),
  nav_panel("Positional Info",
            selectInput('testPitcher', 'Test Pitcher', c('A','B','C')),
            selectInput('testDate', 'Test Date', c('all','1', '2', '3'))
            
            
            ),
  # Game Reports Page ----
  nav_panel("Game Reports", "game reports content"),
  # Umpire Reports Page ----
  nav_panel("Umpire Reports", "Umpire reports  content"),
  
)
)
