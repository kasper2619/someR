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
library(shinycssloaders)
library(highcharter)
library(auth0)
library(twitterwidget)

# function that gets timeline
source("~/someR/projects/backend/get_user_timeline_xdays.R")

# Define UI for application that draws a histogram
ui <- # Define UI for application that draws a histogram

  dashboardPage(skin = "black",title="#tweetawesome",

    ### HEADER ###
    dbHeader <- dashboardHeader(disable = F, title = span(img(src = "logo.jpg", height = 40)), dropdownMenuOutput("ddmenu")),

    ### SIDEBAR ###
    dashboardSidebar(disable = T, collapsed = T),

    ### BODY ###
    dashboardBody(

      # extend screen automatically and enable twitter widget
      tags$head(
        tags$style(
          HTML('.content-wrapper { overflow: auto; }' ),
        ),
        tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
      ),

      # Activity Overview
      fluidRow(
        column(
          width = 4,
            box(width = 12,collapsible = T, solidHeader = T, title = "Activity",
              highchartOutput("activityChart", width = "100%", height = "150px") %>% withSpinner(type = 1, color.background = "white", color="#b6112e", size = 1)
            )
          ),
        column(
          width = 4,
          box(width = 12,collapsible = T, solidHeader = T, title = "Impressions",
              highchartOutput("impressionsChart", width = "100%", height = "150px") %>% withSpinner(type = 1, color.background = "white", color="#fb610d", size = 1)
          )
        ),
        column(
          width = 4,
          box(width = 12, collapsible = T, solidHeader = T, title = "Likes",
              highchartOutput("likesChart", width = "100%", height = "150px") %>% withSpinner(type = 1, color.background = "white", color="#2e2e41", size = 1)
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
            box(width = 12, collapsible = T, solidHeader = F, title = "Overview",
              reactableOutput("activity_table") %>% withSpinner(type = 3, color.background = "white", color="#2e2e41", size = 3)
            )
          )
        ),
      # Tweets
      fluidRow(
        column(width = 4,
               box(width = 12, solidHeader = T,title = "Latest Tweet",collapsible = T,
                   uiOutput("tweet_latest") %>% withSpinner(type = 1, color.background = "white", color="#fb610d", size = 3)
               )
        ),
        column(
          width = 4,
          box(width = 12, solidHeader = T,title = "Tweet with Most Likes",collapsible = T,
              uiOutput("tweet_mostlikes") %>% withSpinner(type = 1, color.background = "white", color="#b6112e", size = 3)
          )
        ),
        column(
          width = 4,
          box(width = 12, solidHeader = T,title = "Tweet with Most Replies",collapsible = T,
              uiOutput("tweet_mostreplies") %>% withSpinner(type = 1, color.background = "white", color="#2e2e41", size = 3)
          )
        )
      ),
      )
    )

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {

  # Pop-Up Box ----

  # Load Timeline Data ----
  dat_tl <- reactive({

    # get
    #handle <- session$userData$auth0_info$twitter_handle
    handle <- "kasper2619"
    #handle <- "ONdbdetliungsit"
    dat <- get_user_timeline_xdays(handle,xdays = 30, maxtweets = 500)

    return(dat)
  })

  # design dropdownmenu
  output$ddmenu <- renderMenu(
    dropdownMenu(headerText = "",
      type = "notifications",
      messageItem(
        from = "Need help?",
        message = "Write to @Fast4Ward_ on Twitter",
        icon = icon("life-ring"),
      ),
      notificationItem(
        logoutButton(label = "Log Out"),
        icon = shiny::icon("off", lib = "glyphicon")
      )
    )
  )

  ## Tweet Latest
  output$tweet_latest <- renderUI({

    dat_tl()  %>% dplyr::filter(
      referenced_tweets_type == "Tweet"
    ) %>% dplyr::slice_head(
      n = 1
    ) -> dat

    tweet_str <- paste0("https://twitter.com/",dat$username,"/status/",dat$id)

    tagList(
      tags$blockquote(
        class = "twitter-tweet",
        tags$a(href = tweet_str)),
        tags$script('twttr.widgets.load(document.getElementById("tweet"));'
      )
    )
  })

  ## Tweet Most Likes
  output$tweet_mostlikes <- renderUI({

    dat_tl() %>% dplyr::filter(
      referenced_tweets_type == "Tweet"
    ) %>% dplyr::arrange(
      desc(like_count)
    ) %>% dplyr::slice_head(
      n = 1
    ) -> dat

    tweet_str <- paste0("https://twitter.com/",dat$username,"/status/",dat$id)

    tagList(
      tags$blockquote(
        class = "twitter-tweet",
        tags$a(href = tweet_str)),
        tags$script('twttr.widgets.load(document.getElementById("tweet"));'
      )
    )
  })

  ## Tweet Most Replies
  output$tweet_mostreplies <- renderUI({

    dat_tl() %>% dplyr::filter(
      referenced_tweets_type == "Tweet"
    ) %>% dplyr::arrange(
      desc(reply_count)
    ) %>% dplyr::slice_head(
      n = 1
    ) -> dat

    tweet_str <- paste0("https://twitter.com/",dat$username,"/status/",dat$id)

    tagList(
      tags$blockquote(
        class = "twitter-tweet",
        tags$a(href = tweet_str)),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));'
      )
    )
  })

  ## Activity Chart ----
  output$activityChart <- renderHighchart({

    dat_tl() %>% dplyr::mutate(
      created_at = substr(created_at,1,10)
    ) %>% dplyr::group_by(
      created_at
    ) %>% dplyr::summarise(
      Activity = n()
    ) %>% dplyr::mutate(
      created_at = as.Date(created_at)
    ) -> dat_plot

    dat_plot %>% tidyr::complete(
      created_at = seq.Date(min(created_at), max(created_at), by="day"), fill = list(Activity = 0)
    ) -> dat_plot

    dat_plot %>%
      hchart(
        "column",
        hcaes(x = created_at, y = Activity), color = "#2e2e41"
      ) %>% hc_xAxis(
        title = "",
        stackLabels = list(enabled = TRUE)
      ) %>% hc_yAxis(
        title = "",
        stackLabels = list(enabled = TRUE)
      ) %>% hc_tooltip(
        crosshairs = F
      ) -> plot

    return(plot)

  })

  ## Impressions Chart ----
  output$impressionsChart <- renderHighchart({

    dat_tl()  %>% dplyr::mutate(
      created_at = substr(created_at,1,10)
    ) %>% dplyr::group_by(
      created_at
    ) %>% dplyr::summarise(
      Activity = sum(impression_count,na.rm = T)
    ) %>% dplyr::mutate(
      created_at = as.Date(created_at)
    ) -> dat_plot

    dat_plot %>% tidyr::complete(
      created_at = seq.Date(min(created_at), max(created_at), by="day"), fill = list(Activity = 0)
    ) -> dat_plot

    dat_plot %>%
      hchart(
        "column",
        hcaes(x = created_at, y = Activity), color = "#b6112e"
      ) %>% hc_xAxis(
        title = "",
        stackLabels = list(enabled = TRUE)
      ) %>% hc_yAxis(
        title = "",
        stackLabels = list(enabled = TRUE)
      ) -> plot

    return(plot)

  })

  ## Likes Chart ----
  output$likesChart <- renderHighchart({

    dat_tl() %>% dplyr::mutate(
      created_at = substr(created_at,1,10)
    ) %>% dplyr::group_by(
      created_at
    ) %>% dplyr::summarise(
      Activity = sum(like_count,na.rm = T)
    ) %>% dplyr::mutate(
      created_at = as.Date(created_at)
    ) -> dat_plot

    dat_plot %>% tidyr::complete(
      created_at = seq.Date(min(created_at), max(created_at), by="day"), fill = list(Activity = 0)
    ) -> dat_plot

    dat_plot %>%
      hchart(
        "column",
        hcaes(x = created_at, y = Activity), color = "#fb610d"
      ) %>% hc_xAxis(
        title = "",
        stackLabels = list(enabled = TRUE)
      ) %>% hc_yAxis(
        title = "",
        stackLabels = list(enabled = TRUE)
      ) -> plot

    return(plot)

  })

  ## Activity Overview ----
  output$activity_table <- renderReactable({

    dat_tl() %>% dplyr::select(
      id,
      text,
      created_at,
      referenced_tweets_type,
      impression_count,
      like_count,
      reply_count,
      retweet_count
    ) -> dat

    reactable(
      theme = fivethirtyeight(),
      dat,
      resizable = TRUE,
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      pagination = TRUE,
      fullWidth = TRUE,
      wrap = FALSE,
      defaultColDef = colDef(
        #header = function(value) gsub("_", "_", value, fixed = TRUE),
        #cell = function(value) format(value, nsmall = 1),
        align = "center",
        minWidth = 120,
        vAlign = "center"
      ),
      columns = list(
        id = colDef(
          name = paste(emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tl()$username[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          minWidth = 720,
          align = "left"
        ),
        #username = colDef(
        #  name = paste(emo::ji("star"),emo::ji("link"), sep = " "),
        #  align = "center",
        #  minWidth = 60,
        #  cell = function(value) {
        #    url <- paste0("https://twitter.com/", value)
        #    shiny::tags$a(href = url, style="text-decoration: none;", target = "_blank", emo::ji("link"))
        #  }
        #),
        created_at = colDef(
          name = paste0(emo::ji("date"), " Date & Time"),
          align = "center",
          minWidth = 140
        ),
        like_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80
        ),
        reply_count = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Replies", sep = " "),
          align = "center",
          minWidth = 80
        ),
        retweet_count = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80
        ),
        referenced_tweets_type = colDef(
          name = paste(emo::ji("abc"),"Type", sep = " "),
          align = "center",
          minWidth = 80
        ),
        impression_count = colDef(
          name = paste(emo::ji("eyes"),"Impressions Type", sep = " "),
          align = "center",
          minWidth = 80
        )
      )
    )
  })
})


# Run the application
#auth0::shinyAppAuth0(ui, server)
shinyApp(ui = ui, server = server)
