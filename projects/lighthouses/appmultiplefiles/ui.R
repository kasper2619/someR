#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(formattable)
library(reactable)
library(reactablefmtr)

### STYLING ###
dbHeader <- dashboardHeader(disable = T)
#dbHeader$children[[2]]$children <-  tags$a(tags$img(src="la_logo.png",height='45',width='45'))


# Define UI for application that draws a histogram
dashboardPage(

  ### HEADER ###

  dbHeader,

  ### SIDEBAR ###
  dashboardSidebar(disable = T),

  ### BODY ###
  dashboardBody(

    # extend screen automatically
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),

    # current week
    fluidRow(
      column(width = 12,
         box(title = "Twitter Analytics Dashboard for Danske Folketingsmedlemmer", width = 12, icon = NULL, background = "yellow"
         ),
         box(title = "Indeværende Uge", width = 12, icon = NULL, collapsible = T,
          reactableOutput("curweek_table")
        ),
        box(title = "Indeværende Måned", width = 12, icon = NULL, collapsible = T,
            reactableOutput("curmonth_table")
        ),
        box(title = "Indeværende År", width = 12, icon = NULL, collapsible = T,
            reactableOutput("curyear_table")
        ),
        box(title = "X", width = 12, icon = NULL, collapsible = T,
            tableOutput("tab")
        )
      )
    )
  )
)

#
# shinyUI(fluidPage(
#
#   # Application title
#   titlePanel("DKPOL Twitter Performance by @kasper2619"),
#
#     # Show a plot of the generated distribution
#     mainPanel(
#        formattableOutput("table")
#     )
#     # mainPanel(
#     #   tableOutput("table")
#     # )
#
# ))
