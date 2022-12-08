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
library(shiny.semantic)
library(shinycssloaders)

ui <- # Define UI for application that draws a histogram

  dashboardPage(skin = "black",title="#twittertinget",

    ### HEADER ###
    dbHeader <- dashboardHeader(disable = F, title = "User Input Form"),

    ### SIDEBAR ###
    dashboardSidebar(disable = T, collapsed = T),

    ### BODY ###
    dashboardBody(
      fluidRow(
        box(width = 12,
          fluidRow(
            column(2,
              textInput("username", "Username", value = "", width = NULL, placeholder = NULL)
            ),
            column(2,
              textInput("affil", "Affiliation", value = "", width = NULL, placeholder = NULL)
            ),
            column(2,
              textInput("party", "Parti", value = "", width = NULL, placeholder = NULL)
            ),
            column(2,
              textInput("blok", "Blok", value = "", width = NULL, placeholder = NULL)
            ),
            column(2,
              shiny::actionButton("add", "Add User"),
              shiny::actionButton("delete", "Delete User")
            )
          ),
          fluidRow(
            column(2,
              selectizeInput("list", "List",
                choices = c(
                  "twittertinget",
                  "lighthouselisten"
                )
              )
            ),
            column(2,
              selectizeInput("country", "Country",
                choices = c(
                  "denmark",
                  "other"
                )
              )
            )
          )
        )
      ),
      fluidRow(
        box(width = 12,
          dataTableOutput('table')
        )
      )
    )
  )

# Define server logic required to draw a histogram

dat_db <- openxlsx::read.xlsx(
  "/home/kasper/someR/projects/backend/data/users.xlsx"
)

server <- shinyServer(function(input, output, session) {

  # Load Data for Scoreboard ----
  db <- reactiveVal(dat_db)

  # Data that needs to be Added ----
  observeEvent(input$add,{

    dat_add <- data.frame(
      user = input$username,
      list = input$list,
      country = input$country,
      affiliation = input$affil,
      party = input$party,
      blok = input$blok
    )

    dat <- rbind(
      dat_add,
      db()
    )

    dat_users<- openxlsx::write.xlsx(
      dat,
      "/home/kasper/someR/projects/backend/data/users.xlsx",
      rownames = F
    )

    db(dat)

  })

  observeEvent(input$delete,{

    db() %>% dplyr::filter(
      user != !!input$username
    ) -> dat

    dat_users<- openxlsx::write.xlsx(
      dat,
      "/home/kasper/someR/projects/backend/data/users.xlsx",
      rownames = F
    )

    db(dat)

  })

  # table
  output$table <- renderDataTable(
    db()
  )

})


# Run the application
shinyApp(ui = ui, server = server)
