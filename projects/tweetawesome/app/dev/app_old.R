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
library(reactable)
library(reactablefmtr)
library(dplyr)
library(shiny)
library(shinycssloaders)
library(auth0)

#auth0::use_auth0(overwrite = T)
usethis::edit_r_environ()

source("~/someR/projects/backend/get_user_timeline_xdays.R")

ui <- fluidPage(
  shiny::tableOutput("tl"),
  logoutButton(),
  verbatimTextOutput("user_info"),
  verbatimTextOutput("credential_info")
)

server <- function(input, output, session) {

  # get user data from login
  dat_tl <- reactive({
    #handle <- session$userData$auth0_info$twitter_handle
    handle <- "kasper2619"
    dat <- get_user_timeline_xdays(handle,10)
    return(dat)
  })



  output$tl <- renderTable({
    return(dat_tl())
  })


  output$user_info <- renderPrint({
    #session$userData$auth0_info
    names(session$userData$auth0_info)
    #names(session)
  })

  output$credential_info <- renderPrint({
    session$userData$auth0_info$twitter_handle
  })
}

# note that here we're using a different version of shinyApp!
#options(shiny.port = 8080)
# auth0::shinyAppAuth0(ui, server)
shinyApp(ui = ui, server = server)

# https://www.youtube.com/watch?v=4TfJqDEjvD0
# https://community.auth0.com/t/get-twitter-handle-from-from-spa/102998
