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
library(shinyBS)
library(shinyLP)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- shinyUI(
    # Include a fliudPage above the navbar to incorporate a icon in the header
    # Source: http://stackoverflow.com/a/24764483
    fluidPage(
      list(tags$head(HTML('<link rel="icon", href="logo.png", type="image/png" />'))),
      div(style="padding: 15px 15px; width: '100%'",titlePanel(title="", windowTitle="Window Tab title")),
      navbarPage(title=div("Fast4Ward"),
        inverse = F, # for diff color view
        theme = shinytheme("flatly"),
        tabPanel("Forside", icon = icon("home"),
          jumbotron("Velkommen til Fast4Ward!", "", button = F, buttonLabel = "Klik Her"),
          column(6,
            thumbnail_label(
              image = "twittertinget.png",
              label = 'Twittertinget',
              content = 'Vores app der følger danske folketingspolitikere på Twitter!',
              button_link = 'http://www.fast4ward.tech/twittertinget/',
              button_label = 'Klik Her!'
            )
          ),
          column(6,
            thumbnail_label(
              image = "lighthouselisten.png",
              label = 'Lighthouselisten',
              content = 'Vores app der følger danske lighthouses på Twitter!',
              button_link = 'http://www.fast4ward.tech/lighthouselisten/',
              button_label = 'Klik Her!'
            )
          ),
          fluidRow(
            column(4, panel_div(class_type = "", panel_title = "Instruktioner", content = "Du tilgår vores apps ved at klikke på de respektive links")),
            column(4, panel_div(class_type = "", panel_title = "Kontakt", content = "Kontakt os ved at skrive til @Fast4Ward_ på Twitter")),
            column(4, panel_div("success", "Status", "Alt kører!"))
          ),  # end of fluidRow
          fluidRow(
            tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
              bsModal("modalExample", "Instructional Video", "tabBut", size = "large" ,
              p("Additional text and widgets can be added in these modal boxes. Video plays in chrome browser"),
              iframe(width = "560", height = "315", url_link = "https://www.youtube.com/embed/0fKg7e37bQE")
            )
          )
      )
      # tabPanel("Home Page 2", icon = icon("cog"),
      #   wells(content = "Imporant Info can go up here before a user starts exploring the application and its features",size = "default"),
      #     h1("Hello Visitors!", align = "center"),hr(),list_group(div(list_item("Application Updates", badge_value = 27), list_item("User Updates", badge_value = 24)))
      # ),
      # tabPanel("Home Page 3", icon = icon("calendar"),
      #   jumbotron("Hello shinyLP!", "Dock Several Applications on a page as a portal", button = FALSE),
      #     hr(),
      #     fluidRow(column(4,thumbnail_label(image = 'Rlogo.png', label = 'Application 1',
      #       content = 'Havana brown cornish rex bombay but bombay,
      #                  but havana brown devonshire rex and devonshire rex.
      #                  Tomcat egyptian mau. Cornish rex sphynx sphynx yet
      #                  cougar and panther. Panther siberian. Lynx munchkin
      #                  american shorthair. Norwegian forest. ',
      #                  button_link = 'http://getbootstrap.com/', button_label = 'Click me'
      #       )),
      #       column(4, thumbnail_label(image = 'Rlogo.png', label = 'Application 2',
      #         content = 'Havana brown cornish rex bombay but bombay,
      #                   but havana brown devonshire rex and devonshire rex.
      #                   Tomcat egyptian mau. Cornish rex sphynx sphynx yet
      #                   cougar and panther. Panther siberian. Lynx munchkin
      #                   american shorthair. Norwegian forest. ',
      #                   button_link = 'http://getbootstrap.com/', button_label = 'Click me')),
      #       column(4, thumbnail_label(image = 'Rlogo.png', label = 'Application 3',
      #         content = 'Havana brown cornish rex bombay but bombay,
      #                   but havana brown devonshire rex and devonshire rex.
      #                   Tomcat egyptian mau. Cornish rex sphynx sphynx yet
      #                   cougar and panther. Panther siberian. Lynx munchkin
      #                   american shorthair. Norwegian forest. ',
      #             button_link = 'http://getbootstrap.com/', button_label = 'Click me'))
      #         )
      #   )
      )
    ) # end of fluid page
  ) # end of shiny

# Define server logic
server <- shinyServer(function(input, output, session) {

  ### WRITE TO LOG ###
  # write to log
  log <- data.frame(
    time = Sys.time() + 60*60*2
  )
  write.table(
    log,
    "/home/kasper/someR/projects/landingpage/logs/visitors.csv", row.names = F, append = T, col.names = F
  )

})

# Run the application
shinyApp(ui = ui, server = server)



