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
            box(title = "Scoreboard", width = 12, collapsible = T,solidHeader = T,
              tabsetPanel(type = "pills",
                tabPanel(
                  "Denne Uge",
                  reactableOutput("curweek_table")
                ),
                tabPanel(
                  "Sidste Uge",
                  reactableOutput("lastweek_table")
                ),
                tabPanel(
                  "Denne Måned",
                  reactableOutput("curmonth_table")
                ),
                tabPanel(
                  "Sidste Måned",
                  reactableOutput("lastmonth_table")
                ),
                tabPanel(
                  "Dette År",
                  reactableOutput("curyear_table")
                ),
                tabPanel(
                  "Sidste År",
                  reactableOutput("lastyear_table")
                )
              )
            )
          )
        )
      )
    )

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {

  ### DATA ###
  dat <- reactive({

    # load data
    con <- someR::con_sql()
    res <- dbSendQuery(con, "SELECT * FROM twitter_lighthouses_tl_stats")
    dat <- dbFetch(res, n = -1)
    dbClearResult(res)
    DBI::dbDisconnect(con)

    return(dat)

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

        "activity_curmonth",
        "tweets_curmonth",
        "comments_curmonth",
        "retweets_curmonth",
        "likes_curmonth",
        "likes_mean_curmonth",
        "commentsprtweet_curmonth",

        "activity_curyear",
        "tweets_curyear",
        "comments_curyear",
        "retweets_curyear",
        "likes_curyear",
        "likes_mean_curyear",
        "commentsprtweet_curyear",

        "activity_lastweek",
        "tweets_lastweek",
        "comments_lastweek",
        "retweets_lastweek",
        "likes_lastweek",
        "likes_mean_lastweek",
        "commentsprtweet_lastweek",

        "activity_lastmonth",
        "tweets_lastmonth",
        "comments_lastmonth",
        "retweets_lastmonth",
        "likes_lastmonth",
        "likes_mean_lastmonth",
        "commentsprtweet_lastmonth",

        "activity_lastyear",
        "tweets_lastyear",
        "comments_lastyear",
        "retweets_lastyear",
        "likes_lastyear",
        "likes_mean_lastyear",
        "commentsprtweet_lastyear",

        "followers_count"
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
      followers_count,
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
      followers_count,

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
      followers_count,

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
      followers_count,

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
      followers_count,

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
      followers_count,

      placering,
      likes_curmonth,
      likes_mean_curmonth,
      commentsprtweet_curmonth,
      activity_curmonth,
      tweets_curmonth,
      comments_curmonth,
      retweets_curmonth
    ) -> table_curmonth

    ### CURRENT LASTMONTH ###
    table_scoreboard %>% dplyr::select(
      screen_name,
      followers_count,

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
      followers_count,

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
      followers_count,

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
      followers_count,

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
      followers_count,

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
      followers_count,
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

  ### CURRENT WEEK TABLE ###
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
          minWidth = 150,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_count = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 90
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"),"Placering", sep = " "),
          align = "center",
          minWidth = 120,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_curweek = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_curweek = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_curweek = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curweek = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curweek = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curweek = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curweek = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_count")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curweek", "likes_mean_curweek","commentsprtweet_curweek"))
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
          minWidth = 150,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_count = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 90
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"),"Placering", sep = " "),
          align = "center",
          minWidth = 120,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_lastweek = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_lastweek = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_lastweek = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_lastweek = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_lastweek = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_lastweek = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_lastweek = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_count")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastweek", "likes_mean_lastweek","commentsprtweet_lastweek"))
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
          minWidth = 150,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_count = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 90
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"),"Placering", sep = " "),
          align = "center",
          minWidth = 120,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_curmonth = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_curmonth = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_curmonth = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curmonth = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curmonth = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curmonth = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curmonth = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_count")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curmonth", "likes_mean_curmonth","commentsprtweet_curmonth"))
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
          minWidth = 150,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_count = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 90
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"),"Placering", sep = " "),
          align = "center",
          minWidth = 120,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_lastmonth = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_lastmonth = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_lastmonth = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_lastmonth = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_lastmonth = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_lastmonth = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_lastmonth = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_count")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastmonth", "likes_mean_lastmonth","commentsprtweet_lastmonth"))
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
          minWidth = 150,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_count = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 90
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"),"Placering", sep = " "),
          align = "center",
          minWidth = 120,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_curyear = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_curyear = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_curyear = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curyear = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curyear = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curyear = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curyear = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_count")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curyear", "likes_mean_curyear","commentsprtweet_curyear"))
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
          minWidth = 150,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        followers_count = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 90
        ),
        placering = colDef(
          name = paste(emo::ji("trophy"),"Placering", sep = " "),
          align = "center",
          minWidth = 120,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_lastyear = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_lastyear = colDef(
          name = paste(emo::ji("fist_right"),"Impact", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_lastyear = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_lastyear = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_lastyear = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_lastyear = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_lastyear = colDef(
          name = paste(emo::ji("exclamation"),"Retweets", sep = " "),
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Lighthouse", columns = c("name", "profile_image_url", "followers_count")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastyear", "likes_mean_lastyear","commentsprtweet_lastyear"))
      )
    )
  })

  #})

})


# Run the application
shinyApp(ui = ui, server = server)
