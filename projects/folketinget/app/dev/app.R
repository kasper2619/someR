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

# Define UI for application that draws a histogram
ui <- # Define UI for application that draws a histogram
  dashboardPage(skin = "black",

    ### HEADER ###
    dbHeader <- dashboardHeader(disable = F, dropdownMenuOutput("ddmenu")),

    ### SIDEBAR ###
    dashboardSidebar(disable = T, collapsed = T),

    ### BODY ###
    dashboardBody(

      # extend screen automatically
      tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),

      # current week
      fluidRow(
        column(width = 12,
               box(title = "Twitter Performance for Folketingsmedlemmer", width = 12, height = 54, icon = NULL, background = "black"
               ),
               box(title = "Performance: Uge", width = 12, icon = NULL, collapsible = T,
                   reactableOutput("curweek_table")
               ),
               box(title = "Performance: Måned", width = 12, icon = NULL, collapsible = T,
                   reactableOutput("curmonth_table")
               ),
               box(title = "Performance: År", width = 12, icon = NULL, collapsible = T,
                   reactableOutput("curyear_table")
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
    res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_tl_stats")
    dat <- dbFetch(res, n = -1)
    dbClearResult(res)
    DBI::dbDisconnect(con)

    return(dat)

  })

  dblastupdate <- reactive({

    # load data
    con <- someR::con_sql()
    res <- dbSendQuery(con, "SELECT max(timestamp) FROM twitter_folketing_tl_clean")
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
        message = "Skriv til @RansHosling på Twitter",
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
      "/home/kasper/someR/projects/folketinget/logs/visitors.csv", row.names = F, append = T, col.names = F
    )

    # need to join this in for later
    dat() %>% dplyr::select(
      screen_name,name,party,profile_image_url
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
        "quotes_curweek",
        "likes_curweek",
        "likes_mean_curweek",
        "activity_curmonth",
        "tweets_curmonth",
        "comments_curmonth",
        "retweets_curmonth",
        "quotes_curmonth",
        "likes_curmonth",
        "likes_mean_curmonth",
        "activity_curyear",
        "tweets_curyear",
        "comments_curyear",
        "retweets_curyear",
        "quotes_curyear",
        "likes_curyear",
        "likes_mean_curyear",
        "followers_count",
        "friends_count"
      )
    ) -> table_currentx

    table_currentx <- reshape2::dcast(
      table_currentx,
      "screen_name ~ variable",
      value.var = "value",
      fun.aggregate = sum
    )

    ### CURRENT WEEK ###
    table_currentx %>% dplyr::select(
      screen_name,
      followers_count,
      friends_count,
      activity_curweek,
      tweets_curweek,
      comments_curweek,
      retweets_curweek,
      quotes_curweek,
      likes_curweek,
      likes_mean_curweek
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
      party,
      followers_count,
      friends_count,
      placering,
      likes_curweek,
      likes_mean_curweek,
      activity_curweek,
      tweets_curweek,
      comments_curweek,
      retweets_curweek,
      quotes_curweek
    ) -> table_curweek

    ### CURRENT MONTH ###
    table_currentx %>% dplyr::select(
      screen_name,
      followers_count,
      friends_count,
      activity_curmonth,
      tweets_curmonth,
      comments_curmonth,
      retweets_curmonth,
      quotes_curmonth,
      likes_curmonth,
      likes_mean_curmonth
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
      party,
      followers_count,
      friends_count,
      placering,
      likes_curmonth,
      likes_mean_curmonth,
      activity_curmonth,
      tweets_curmonth,
      comments_curmonth,
      retweets_curmonth,
      quotes_curmonth
    ) -> table_curmonth

    ### CURRENT YEAR ###
    table_currentx %>% dplyr::select(
      screen_name,
      followers_count,
      friends_count,
      activity_curyear,
      tweets_curyear,
      comments_curyear,
      retweets_curyear,
      quotes_curyear,
      likes_curyear,
      likes_mean_curyear
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
      party,
      followers_count,
      friends_count,
      placering,
      likes_curyear,
      likes_mean_curyear,
      activity_curyear,
      tweets_curyear,
      comments_curyear,
      retweets_curyear,
      quotes_curyear
    ) -> table_curyear

    # put into list
    out <- list(
      curweek = table_curweek,
      curmonth = table_curmonth,
      curyear = table_curyear
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
          cell = embed_img()
        ),
        name = colDef(
          name = "Politiker",
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party = colDef(
          name = "Parti",
          align = "center",
          minWidth = 90
        ),
        followers_count = colDef(
          name = "Følgere",
          align = "center",
          minWidth = 90
        ),
        friends_count = colDef(
          name = "Venner",
          align = "center",
          minWidth = 90
        ),
        placering = colDef(
          name = "Placering",
          align = "center",
          minWidth = 90,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_curweek = colDef(
          name = "Likes I Alt",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_curweek = colDef(
          name = "Likes Gns.",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curweek = colDef(
          name = "Aktivitet",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curweek = colDef(
          name = "Tweets",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curweek = colDef(
          name = "Kommentarer",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curweek = colDef(
          name = "Retweets",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        quotes_curweek = colDef(
          name = "Quotes",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
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
      defaultColDef = colDef(
        #header = function(value) gsub("//.", "_", value, fixed = TRUE),
        #cell = function(value) format(value, nsmall = 1),
        align = "center",

        minWidth = 120
      ),
      columns = list(
        profile_image_url = colDef(
          name = "",
          minWidth = 40,
          filterable = F,
          cell = embed_img()
        ),
        name = colDef(
          name = "Politiker",
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party = colDef(
          name = "Parti",
          align = "center",
          minWidth = 90
        ),
        followers_count = colDef(
          name = "Følgere",
          align = "center",
          minWidth = 90
        ),
        friends_count = colDef(
          name = "Venner",
          align = "center",
          minWidth = 90
        ),
        placering = colDef(
          name = "Placering",
          align = "center",
          minWidth = 90,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_curmonth = colDef(
          name = "Likes I Alt",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_curmonth = colDef(
          name = "Likes Gns.",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curmonth = colDef(
          name = "Aktivitet",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curmonth = colDef(
          name = "Tweets",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curmonth = colDef(
          name = "Kommentarer",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curmonth = colDef(
          name = "Retweets",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        quotes_curmonth = colDef(
          name = "Quotes",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
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
          cell = embed_img()
        ),
        name = colDef(
          name = "Politiker",
          align = "left",
          minWidth = 180,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party = colDef(
          name = "Parti",
          align = "center",
          minWidth = 90,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        followers_count = colDef(
          name = "Følgere",
          align = "center",
          minWidth = 90
        ),
        friends_count = colDef(
          name = "Venner",
          align = "center",
          minWidth = 90
        ),
        placering = colDef(
          name = "Placering",
          align = "center",
          minWidth = 90,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == 1) emo::ji("1st_place_medal") else if (value == 2) emo::ji("2nd_place_medal") else if (value == 3) emo::ji("3rd_place_medal") else value
          }
        ),
        likes_curyear = colDef(
          name = "Likes I Alt",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        likes_mean_curyear = colDef(
          name = "Likes Gns.",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curyear = colDef(
          name = "Aktivitet",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curyear = colDef(
          name = "Tweets",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curyear = colDef(
          name = "Kommentarer",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curyear = colDef(
          name = "Retweets",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        quotes_curyear = colDef(
          name = "quotes",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        )
      )
    )
  })

  #})

})


# Run the application
shinyApp(ui = ui, server = server)
