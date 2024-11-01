#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(bsicons)
library(shinythemes)

# Define UI for application that draws a histogram


page_navbar(
  nav_panel("Home",
            accordion(open = FALSE,
              accordion_panel(title = 'Updates',
                              icon = bs_icon('bell')
                              ),
              accordion_panel(title = 'Info',
                              icon = bs_icon('info-square')
              )
            )
            ),
  nav_panel("Team Pitching", "Team Pitching Content"),
  nav_panel("Team Hitting", "Team Hitting Content"),
  nav_panel("Pitching", 
            layout_sidebar(sidebar = sidebar(width = 250, open = 'open', title = 'Filters',
                                             div(fluidRow(
                                                          column(6, checkboxGroupInput("ball", "Balls", c('a'))),
                                                          column(6, checkboxGroupInput("strike", "Balls", c('b')))
                                                        )
                                                ),
                                            ),
            
                          ),
            ), 
  nav_panel("Hitting", "Page C content"),
  nav_panel("Game Reports", "Game reports content"),
  nav_panel("Umpire Reports", "Umpire reports  content"),
  title = "MU Baseball", 
  id = "page", 
)
