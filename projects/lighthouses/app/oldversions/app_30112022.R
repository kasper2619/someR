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

# Define UI for application that draws a histogram
ui <- # Define UI for application that draws a histogram

  dashboardPage(skin = "black",title="#lighthouselisten",

    ### HEADER ###
    dbHeader <- dashboardHeader(disable = F, title = "#lighthouselisten", dropdownMenuOutput("ddmenu")),

    ### SIDEBAR ###
    dashboardSidebar(disable = T, collapsed = T),

    ### BODY ###
    dashboardBody(

      # extend screen automatically
      tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),

      # current week
      fluidRow(
        column(
          width = 12,
            box(title = "", width = 12, collapsible = T,solidHeader = T,
              tabsetPanel(type = "pills",
                tabPanel(
                  "Denne Uge",
                  h3("Scoreboard"),
                  reactableOutput("curweek_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3),
                  h3("Tweets"),
                  reactableOutput("curweek_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Sidste Uge",
                  h3("Scoreboard"),
                  reactableOutput("lastweek_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3),
                  h3("Tweets"),
                  reactableOutput("lastweek_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Denne Måned",
                  h3("Scoreboard"),
                  reactableOutput("curmonth_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3),
                  h3("Tweets"),
                  reactableOutput("curmonth_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Sidste Måned",
                  h3("Scoreboard"),
                  reactableOutput("lastmonth_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3),
                  h3("Tweets"),
                  reactableOutput("lastmonth_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Dette År",
                  h3("Scoreboard"),
                  reactableOutput("curyear_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3),
                  h3("Tweets"),
                  reactableOutput("curyear_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Sidste År",
                  h3("Scoreboard"),
                  reactableOutput("lastyear_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3),
                  h3("Tweets"),
                  reactableOutput("lastyear_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                )
              )
            )
          )
        )
      )
    )

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {

  ### DATA FOR SCOREBOARD ###
  dat <- reactive({

    # load data
    con <- someR::con_sql()
    res <- dbSendQuery(con, "SELECT * FROM twitter_lighthouses_tl_stats")
    dat <- dbFetch(res, n = -1)
    dbClearResult(res)
    DBI::dbDisconnect(con)

    return(dat)

  })

  ### DATA FOR TWEETS ###
  dat_tweets <- reactive({

    # load data
    con <- someR::con_sql()
    res <- dbSendQuery(
      con,
      "SELECT
        reply_to_status_id,
        is_retweet,
        profile_image_url,
        name,
        screen_name,
        created_at,
        favorite_count,
        text,
        status_id
      FROM twitter_lighthouses_tl_clean"
    )
    dat <- dbFetch(res, n = -1)
    dbClearResult(res)
    DBI::dbDisconnect(con)

    # select columns and do filters
    dat %>% dplyr::filter(
      is.na(reply_to_status_id) == T,
      is_retweet == 0
    ) %>% dplyr::select(
      profile_image_url,
      name,
      screen_name,
      favorite_count,
      text,
      created_at,
      status_id
    ) %>% dplyr::arrange(
      desc(created_at)
    ) -> dat_tweets

    # derive time variables
    dat_tweets %>% dplyr::mutate(
      date = as.Date(created_at),
      year = lubridate::year(created_at),
      month = lubridate::month(created_at, label = T),
      week = lubridate::week(created_at),
      wday = lubridate::wday(created_at, label = T),
      hour = lubridate::hour(created_at)
    ) -> dat_tweets

    # this week
    dat_tweets %>% dplyr::filter(
      year == lubridate::year(Sys.Date()),
      week == lubridate::week(Sys.Date())
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) -> dat_tweets_curweek

    # last week
    dat_tweets %>% dplyr::filter(
      year == lubridate::year(Sys.Date()),
      week == lubridate::week(Sys.Date())-1
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) -> dat_tweets_lastweek

    # this month
    dat_tweets %>% dplyr::filter(
      year == lubridate::year(Sys.Date()),
      month == lubridate::month(Sys.Date(), label = T)
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) -> dat_tweets_curmonth

    # last month
    dat_tweets %>% dplyr::filter(
      year == lubridate::year(Sys.Date()),
      month == lubridate::month(lubridate::rollback(Sys.Date()), label = T)
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) -> dat_tweets_lastmonth

    # this year
    dat_tweets %>% dplyr::filter(
      year == lubridate::year(Sys.Date())
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) -> dat_tweets_curyear

    # last year
    dat_tweets %>% dplyr::filter(
      year == lubridate::year(Sys.Date())-1
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) -> dat_tweets_lastyear

    out <- list(
      curweek = dat_tweets_curweek,
      lastweek = dat_tweets_lastweek,
      curmonth = dat_tweets_curmonth,
      lastmonth = dat_tweets_lastmonth,
      curyear = dat_tweets_curyear,
      lastyear = dat_tweets_lastyear
    )

    return(out)

  })

  dblastupdate <- reactive({

    # load data
    con <- someR::con_sql()
    res <- dbSendQuery(con, "SELECT max(timestamp) FROM twitter_lighthouses_tl_clean")
    dat <- dbFetch(res, n = -1)
    dbClearResult(res)
    DBI::dbDisconnect(con)

    val <- dat$`max(timestamp)`

    return(dat)

  })

  # design dropdownmenu
  output$ddmenu <- renderMenu(
    dropdownMenu(headerText = "",
      type = "notifications",
      messageItem(
        from = "Brug for hjælp?",
        message = "Skriv til @Fast4Ward_ på Twitter",
        icon = icon("life-ring"),
      ),
      notificationItem(
        text = paste0("Sidst Opdateret: ",dblastupdate()),
        icon = shiny::icon("refresh", ib = "glyphicon")
      )
    )
  )

  ### GET DATA ###
  dat_tables <- reactive({

    ### WRITE TO LOG ###
    # write to log
    log <- data.frame(
      time = Sys.time() + 60*60*2
    )
    write.table(
      log,
      "/home/kasper/someR/projects/lighthouses/logs/visitors.csv", row.names = F, append = T, col.names = F
    )

    # need to join this in for later
    dat() %>% dplyr::select(
      screen_name,name,profile_image_url
    ) %>% distinct(
      screen_name,
      .keep_all = T
    ) -> dat_master

    # make table
    dat() %>% dplyr::filter(
      variable %in% c(
        "activity_curweek",
        "tweets_curweek",
        "comments_curweek",
        "retweets_curweek",
        "likes_curweek",
        "likes_mean_curweek",
        "commentsprtweet_curweek",
        "followers_curweek",

        "activity_curmonth",
        "tweets_curmonth",
        "comments_curmonth",
        "retweets_curmonth",
        "likes_curmonth",
        "likes_mean_curmonth",
        "commentsprtweet_curmonth",
        "followers_curmonth",

        "activity_curyear",
        "tweets_curyear",
        "comments_curyear",
        "retweets_curyear",
        "likes_curyear",
        "likes_mean_curyear",
        "commentsprtweet_curyear",
        "followers_curyear",

        "activity_lastweek",
        "tweets_lastweek",
        "comments_lastweek",
        "retweets_lastweek",
        "likes_lastweek",
        "likes_mean_lastweek",
        "commentsprtweet_lastweek",
        "followers_lastweek",

        "activity_lastmonth",
        "tweets_lastmonth",
        "comments_lastmonth",
        "retweets_lastmonth",
        "likes_lastmonth",
        "likes_mean_lastmonth",
        "commentsprtweet_lastmonth",
        "followers_lastmonth",

        "activity_lastyear",
        "tweets_lastyear",
        "comments_lastyear",
        "retweets_lastyear",
        "likes_lastyear",
        "likes_mean_lastyear",
        "commentsprtweet_lastyear",
        "followers_lastyear"

      )
    ) -> table_scoreboard

    table_scoreboard <- reshape2::dcast(
      table_scoreboard,
      "screen_name ~ variable",
      value.var = "value",
      fun.aggregate = sum
    )

    ### CURRENT WEEK ###
    table_scoreboard %>% dplyr::select(
      screen_name,
      followers_curweek,
      activity_curweek,
      tweets_curweek,
      comments_curweek,
      retweets_curweek,
      likes_curweek,
      likes_mean_curweek,
      commentsprtweet_curweek
    ) %>% dplyr::arrange(
      desc(likes_curweek)
    ) -> table_curweek
    table_curweek[["likes_mean_curweek"]] <- round(table_curweek[["likes_mean_curweek"]],2)
    table_curweek[["placering"]] <- 1:nrow(table_curweek)

    table_curweek <- dplyr::left_join(
      table_curweek,dat_master, by = c("screen_name"="screen_name")
    )
    table_curweek %>% dplyr::select(
      profile_image_url,
      name,
      followers_curweek,

      placering,
      likes_curweek,
      likes_mean_curweek,
      commentsprtweet_curweek,
      activity_curweek,
      tweets_curweek,
      comments_curweek,
      retweets_curweek
    ) -> table_curweek

    ### LAST WEEK ###
    table_scoreboard %>% dplyr::select(
      screen_name,
      followers_lastweek,

      activity_lastweek,
      tweets_lastweek,
      comments_lastweek,
      retweets_lastweek,
      likes_lastweek,
      likes_mean_lastweek,
      commentsprtweet_lastweek
    ) %>% dplyr::arrange(
      desc(likes_lastweek)
    ) -> table_lastweek
    table_lastweek[["likes_mean_lastweek"]] <- round(table_lastweek[["likes_mean_lastweek"]],2)
    table_lastweek[["placering"]] <- 1:nrow(table_lastweek)

    table_lastweek <- dplyr::left_join(
      table_lastweek,dat_master, by = c("screen_name"="screen_name")
    )
    table_lastweek %>% dplyr::select(
      profile_image_url,
      name,
      followers_lastweek,

      placering,
      likes_lastweek,
      likes_mean_lastweek,
      commentsprtweet_lastweek,
      activity_lastweek,
      tweets_lastweek,
      comments_lastweek,
      retweets_lastweek
    ) -> table_lastweek

    ### CURRENT MONTH ###
    table_scoreboard %>% dplyr::select(
      screen_name,
      followers_curmonth,

      activity_curmonth,
      tweets_curmonth,
      comments_curmonth,
      retweets_curmonth,
      likes_curmonth,
      likes_mean_curmonth,
      commentsprtweet_curmonth
    ) %>% dplyr::arrange(
      desc(likes_curmonth)
    ) -> table_curmonth
    table_curmonth[["likes_mean_curmonth"]] <- round(table_curmonth[["likes_mean_curmonth"]],2)
    table_curmonth[["placering"]] <- 1:nrow(table_curmonth)

    table_curmonth <- dplyr::left_join(
      table_curmonth,dat_master, by = c("screen_name"="screen_name")
    )
    table_curmonth %>% dplyr::select(
      profile_image_url,
      name,
      followers_curmonth,

      placering,
      likes_curmonth,
      likes_mean_curmonth,
      commentsprtweet_curmonth,
      activity_curmonth,
      tweets_curmonth,
      comments_curmonth,
      retweets_curmonth
    ) -> table_curmonth

    ### LASTMONTH ###
    table_scoreboard %>% dplyr::select(
      screen_name,
      followers_lastmonth,

      activity_lastmonth,
      tweets_lastmonth,
      comments_lastmonth,
      retweets_lastmonth,
      likes_lastmonth,
      likes_mean_lastmonth,
      commentsprtweet_lastmonth
    ) %>% dplyr::arrange(
      desc(likes_lastmonth)
    ) -> table_lastmonth
    table_lastmonth[["likes_mean_lastmonth"]] <- round(table_lastmonth[["likes_mean_lastmonth"]],2)
    table_lastmonth[["placering"]] <- 1:nrow(table_lastmonth)

    table_lastmonth <- dplyr::left_join(
      table_lastmonth,dat_master, by = c("screen_name"="screen_name")
    )
    table_lastmonth %>% dplyr::select(
      profile_image_url,
      name,
      followers_lastmonth,

      placering,
      likes_lastmonth,
      likes_mean_lastmonth,
      commentsprtweet_lastmonth,
      activity_lastmonth,
      tweets_lastmonth,
      comments_lastmonth,
      retweets_lastmonth
    ) -> table_lastmonth

    ### CURRENT YEAR ###
    table_scoreboard %>% dplyr::select(
      screen_name,
      followers_curyear,

      activity_curyear,
      tweets_curyear,
      comments_curyear,
      retweets_curyear,
      likes_curyear,
      likes_mean_curyear,
      commentsprtweet_curyear
    ) %>% dplyr::arrange(
      desc(likes_curyear)
    ) -> table_curyear
    table_curyear[["likes_mean_curyear"]] <- round(table_curyear[["likes_mean_curyear"]],2)
    table_curyear[["placering"]] <- 1:nrow(table_curyear)

    table_curyear <- dplyr::left_join(
      table_curyear,dat_master, by = c("screen_name"="screen_name")
    )
    table_curyear %>% dplyr::select(
      profile_image_url,
      name,
      followers_curyear,

      placering,
      likes_curyear,
      likes_mean_curyear,
      commentsprtweet_curyear,
      activity_curyear,
      tweets_curyear,
      comments_curyear,
      retweets_curyear
    ) -> table_curyear

    ### LAST YEAR ###
    table_scoreboard %>% dplyr::select(
      screen_name,
      followers_lastyear,

      activity_lastyear,
      tweets_lastyear,
      comments_lastyear,
      retweets_lastyear,
      likes_lastyear,
      likes_mean_lastyear,
      commentsprtweet_lastyear
    ) %>% dplyr::arrange(
      desc(likes_lastyear)
    ) -> table_lastyear
    table_lastyear[["likes_mean_lastyear"]] <- round(table_lastyear[["likes_mean_lastyear"]],2)
    table_lastyear[["placering"]] <- 1:nrow(table_lastyear)

    table_lastyear <- dplyr::left_join(
      table_lastyear,dat_master, by = c("screen_name"="screen_name")
    )
    table_lastyear %>% dplyr::select(
      profile_image_url,
      name,
      followers_lastyear,
      placering,
      likes_lastyear,
      likes_mean_lastyear,
      commentsprtweet_lastyear,
      activity_lastyear,
      tweets_lastyear,
      comments_lastyear,
      retweets_lastyear
    ) -> table_lastyear

    # put into list
    out <- list(
      curweek = table_curweek,
      lastweek = table_lastweek,
      curmonth = table_curmonth,
      lastmonth = table_lastmonth,
      curyear = table_curyear,
      lastyear = table_lastyear
    )

    return(out)

  })

  ### CURRENT WEEK SCOREBOARD ###
  output$curweek_table <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tables()[["curweek"]],
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
        minWidth = 120
      ),
      columns = list(
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_curweek = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 80
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"), sep = " "),
          align = "center",
          minWidth = 40,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_curweek = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_curweek = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_curweek = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curweek = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curweek = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curweek = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curweek = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_curweek")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curweek", "likes_mean_curweek","commentsprtweet_curweek"))
      )
    )
  })

  ### CURRENT WEEK TWEETS ###
  output$curweek_tweets <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tweets()[["curweek"]],
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
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        screen_name = colDef(
          name = paste(emo::ji("star"),emo::ji("link"), sep = " "),
          align = "center",
          minWidth = 60,
          cell = function(value) {
            url <- paste0("https://twitter.com/", value)
            shiny::tags$a(href = url, style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        ),
        created_at = colDef(
          name = paste0(emo::ji("date"), " Tidspunkt"),
          align = "center",
          minWidth = 140
        ),
        favorite_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        status_id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["curweek"]]$screen_name[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ### LAST WEEK TABLE ###
  output$lastweek_table <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tables()[["lastweek"]],
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
        minWidth = 120
      ),
      columns = list(
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_lastweek = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 80
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"), sep = " "),
          align = "center",
          minWidth = 40,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_lastweek = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_lastweek = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_lastweek = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_lastweek = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_lastweek = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_lastweek = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_lastweek = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_lastweek")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastweek", "likes_mean_lastweek","commentsprtweet_lastweek"))
      )
    )
  })

  ### LAST WEEK TWEETS ###
  output$lastweek_tweets <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tweets()[["lastweek"]],
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
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        screen_name = colDef(
          name = paste(emo::ji("star"),emo::ji("link"), sep = " "),
          align = "center",
          minWidth = 60,
          cell = function(value) {
            url <- paste0("https://twitter.com/", value)
            shiny::tags$a(href = url, style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        ),
        created_at = colDef(
          name = paste0(emo::ji("date"), " Tidspunkt"),
          align = "center",
          minWidth = 140
        ),
        favorite_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        status_id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["lastweek"]]$screen_name[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ### CURRENT MONTH TABLE ###
  output$curmonth_table <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tables()[["curmonth"]],
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
        minWidth = 120
      ),
      columns = list(
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_curmonth = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 80
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"), sep = " "),
          align = "center",
          minWidth = 40,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_curmonth = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_curmonth = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_curmonth = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curmonth = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curmonth = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curmonth = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curmonth = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_curmonth")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curmonth", "likes_mean_curmonth","commentsprtweet_curmonth"))
      )
    )
  })

  ### CURRENT MONTH TWEETS ###
  output$curmonth_tweets <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tweets()[["curmonth"]],
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
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        screen_name = colDef(
          name = paste(emo::ji("star"),emo::ji("link"), sep = " "),
          align = "center",
          minWidth = 60,
          cell = function(value) {
            url <- paste0("https://twitter.com/", value)
            shiny::tags$a(href = url, style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        ),
        created_at = colDef(
          name = paste0(emo::ji("date"), " Tidspunkt"),
          align = "center",
          minWidth = 140
        ),
        favorite_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        status_id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["curmonth"]]$screen_name[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ### LAST MONTH TABLE ###
  output$lastmonth_table <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tables()[["lastmonth"]],
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
        minWidth = 120
      ),
      columns = list(
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_lastmonth = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 80
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"), sep = " "),
          align = "center",
          minWidth = 40,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_lastmonth = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_lastmonth = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_lastmonth = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_lastmonth = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_lastmonth = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_lastmonth = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_lastmonth = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_lastmonth")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastmonth", "likes_mean_lastmonth","commentsprtweet_lastmonth"))
      )
    )
  })

  ### LAST MONTH TWEETS ###
  output$lastmonth_tweets <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tweets()[["lastmonth"]],
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
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        screen_name = colDef(
          name = paste(emo::ji("star"),emo::ji("link"), sep = " "),
          align = "center",
          minWidth = 60,
          cell = function(value) {
            url <- paste0("https://twitter.com/", value)
            shiny::tags$a(href = url, style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        ),
        created_at = colDef(
          name = paste0(emo::ji("date"), " Tidspunkt"),
          align = "center",
          minWidth = 140
        ),
        favorite_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        status_id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["lastmonth"]]$screen_name[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ### CURRENT YEAR TABLE ###
  output$curyear_table <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tables()[["curyear"]],
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
        minWidth = 120
      ),
      columns = list(
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_curyear = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 80
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"), sep = " "),
          align = "center",
          minWidth = 40,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_curyear = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_curyear = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_curyear = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curyear = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curyear = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curyear = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curyear = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_curyear")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curyear", "likes_mean_curyear","commentsprtweet_curyear"))
      )
    )
  })

  ### CURRENT YEAR TWEETS ###
  output$curyear_tweets <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tweets()[["curyear"]],
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
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        screen_name = colDef(
          name = paste(emo::ji("star"),emo::ji("link"), sep = " "),
          align = "center",
          minWidth = 60,
          cell = function(value) {
            url <- paste0("https://twitter.com/", value)
            shiny::tags$a(href = url, style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        ),
        created_at = colDef(
          name = paste0(emo::ji("date"), " Tidspunkt"),
          align = "center",
          minWidth = 140
        ),
        favorite_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        status_id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["curyear"]]$screen_name[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ### LAST YEAR TABLE ###
  output$lastyear_table <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tables()[["lastyear"]],
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
        minWidth = 120
      ),
      columns = list(
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_lastyear = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 80
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"), sep = " "),
          align = "center",
          minWidth = 40,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_lastyear = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_lastyear = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_lastyear = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_lastyear = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_lastyear = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_lastyear = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_lastyear = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_lastyear")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastyear", "likes_mean_lastyear","commentsprtweet_lastyear"))
      )
    )
  })

  ### LAST YEAR TWEETS ###
  output$lastyear_tweets <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tweets()[["lastyear"]],
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
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img(),
          html = TRUE
        ),
        name = colDef(
          name = paste(emo::ji("star"),"Navn", sep = " "),
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        screen_name = colDef(
          name = paste(emo::ji("star"),emo::ji("link"), sep = " "),
          align = "center",
          minWidth = 60,
          cell = function(value) {
            url <- paste0("https://twitter.com/", value)
            shiny::tags$a(href = url, style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        ),
        created_at = colDef(
          name = paste0(emo::ji("date"), " Tidspunkt"),
          align = "center",
          minWidth = 140
        ),
        favorite_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        status_id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["lastyear"]]$screen_name[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })
})


# Run the application
shinyApp(ui = ui, server = server)
