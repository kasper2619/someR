#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(someR)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage("Welcome to Fast4Ward. Landing page is work in progress",
  theme = shinythemes::shinytheme("cosmo"),  # <--- Specify theme here
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
})

# Run the application
shinyApp(ui = ui, server = server)




