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

### STYLING ###
dbHeader <- dashboardHeader(disable = T)

# Define UI for application that draws a histogram
ui <- # Define UI for application that draws a histogram
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
    #dat <- read.csv("/home/kasper/someR/projects/folketinget/app/data/tables.csv")
    con <- someR::con_sql()
    res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_tl_stats")
    dat <- dbFetch(res, n = -1)
    dbClearResult(res)
    DBI::dbDisconnect(con)
    #print(dat)
    return(dat)
  })

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

    # get data
    #dat <- read.csv("/home/kasper/someR/projects/folketinget/app/data/tables.csv")
    #con <- someR::con_sql()
    #res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_tl_stats")
    #dat <- dbFetch(res, n = -1)
    #dbClearResult(res)
    #DBI::dbDisconnect(con)

    # need to join this in for later
    dat() %>% dplyr::select(
      user,name,party
    ) %>% distinct(
      user,
      .keep_all = T
    ) -> dat_master

    # make table
    dat() %>% dplyr::filter(
      variable %in% c(
        "activity_curweek",
        "tweets_curweek",
        "comments_curweek",
        "likes_curweek",
        "likes_mean_curweek",
        "activity_curmonth",
        "tweets_curmonth",
        "comments_curmonth",
        "likes_curmonth",
        "likes_mean_curmonth",
        "activity_curyear",
        "tweets_curyear",
        "comments_curyear",
        "likes_curyear",
        "likes_mean_curyear"
      )
    ) -> table_currentx

    table_currentx <- reshape2::dcast(
      table_currentx,
      "user ~ variable",
      value.var = "value",
      fun.aggregate = sum
    )

    ### CURRENT WEEK ###
    table_currentx %>% dplyr::select(
      user,
      activity_curweek,
      tweets_curweek,
      comments_curweek,
      likes_curweek,
      likes_mean_curweek
    ) %>% dplyr::arrange(
      desc(likes_curweek)
    ) -> table_curweek
    table_curweek[["likes_mean_curweek"]] <- round(table_curweek[["likes_mean_curweek"]],2)
    table_curweek[["placering"]] <- 1:nrow(table_curweek)

    table_curweek <- dplyr::left_join(
      table_curweek,dat_master, by = c("user"="user")
    )
    table_curweek %>% dplyr::select(
      name,
      party,
      placering,
      likes_curweek,
      likes_mean_curweek,
      activity_curweek,
      tweets_curweek,
      comments_curweek
    ) -> table_curweek

    ### CURRENT MONTH ###
    table_currentx %>% dplyr::select(
      user,
      activity_curmonth,
      tweets_curmonth,
      comments_curmonth,
      likes_curmonth,
      likes_mean_curmonth
    ) %>% dplyr::arrange(
      desc(likes_curmonth)
    ) -> table_curmonth
    table_curmonth[["likes_mean_curmonth"]] <- round(table_curmonth[["likes_mean_curmonth"]],2)
    table_curmonth[["placering"]] <- 1:nrow(table_curmonth)

    table_curmonth <- dplyr::left_join(
      table_curmonth,dat_master, by = c("user"="user")
    )
    table_curmonth %>% dplyr::select(
      name,
      party,
      placering,
      likes_curmonth,
      likes_mean_curmonth,
      activity_curmonth,
      tweets_curmonth,
      comments_curmonth
    ) -> table_curmonth

    ### CURRENT YEAR ###
    table_currentx %>% dplyr::select(
      user,
      activity_curyear,
      tweets_curyear,
      comments_curyear,
      likes_curyear,
      likes_mean_curyear
    ) %>% dplyr::arrange(
      desc(likes_curyear)
    ) -> table_curyear
    table_curyear[["likes_mean_curyear"]] <- round(table_curyear[["likes_mean_curyear"]],2)
    table_curyear[["placering"]] <- 1:nrow(table_curyear)

    table_curyear <- dplyr::left_join(
      table_curyear,dat_master, by = c("user"="user")
    )
    table_curyear %>% dplyr::select(
      name,
      party,
      placering,
      likes_curyear,
      likes_mean_curyear,
      activity_curyear,
      tweets_curyear,
      comments_curyear
    ) -> table_curyear

    # put into list
    out <- list(
      curweek = table_curweek,
      curmonth = table_curmonth,
      curyear = table_curyear
    )

    #print(out)

    return(out)

  })

  ### CURRENT WEEK TABLE ###
  output$curweek_table <- renderReactable({

    reactable(
      theme = nytimes(),
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
        name = colDef(
          name = "Politiker",
          align = "left",
          minWidth = 180
        ),
        party = colDef(
          name = "Parti",
          align = "center",
          minWidth = 180
        ),
        placering = colDef(
          name = "Placering",
          align = "center",
          minWidth = 180
        ),
        likes_curweek = colDef(
          name = "Likes I Alt",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#FFD700")
        ),
        likes_mean_curweek = colDef(
          name = "Likes Gns.",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#FFD700")
        ),
        activity_curweek = colDef(
          name = "Aktivitet",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#FFD700")
        ),
        tweets_curweek = colDef(
          name = "Tweets",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#FFD700")
        ),
        comments_curweek = colDef(
          name = "Kommentarer",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#FFD700")
        )
      )
    )

  })

  ### CURRENT MONTH TABLE ###
  output$curmonth_table <- renderReactable({

    reactable(
      theme = nytimes(),
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
        name = colDef(
          name = "Politiker",
          align = "left",
          minWidth = 180
        ),
        party = colDef(
          name = "Parti",
          align = "center",
          minWidth = 180
        ),
        placering = colDef(
          name = "Placering",
          align = "center",
          minWidth = 180
        ),
        likes_curmonth = colDef(
          name = "Likes I Alt",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#FFD700")
        ),
        likes_mean_curmonth = colDef(
          name = "Likes Gns.",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#FFD700")
        ),
        activity_curmonth = colDef(
          name = "Aktivitet",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#FFD700")
        ),
        tweets_curmonth = colDef(
          name = "Tweets",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#FFD700")
        ),
        comments_curmonth = colDef(
          name = "Kommentarer",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#FFD700")
        )
      )
    )

  })

  ### CURRENT YEAR TABLE ###
  output$curyear_table <- renderReactable({

    reactable(
      theme = nytimes(),
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
        name = colDef(
          name = "Politiker",
          align = "left",
          minWidth = 180
        ),
        party = colDef(
          name = "Parti",
          align = "center",
          minWidth = 180
        ),
        placering = colDef(
          name = "Placering",
          align = "center",
          minWidth = 180
        ),
        likes_curyear = colDef(
          name = "Likes I Alt",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#FFD700")
        ),
        likes_mean_curyear = colDef(
          name = "Likes Gns.",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#FFD700")
        ),
        activity_curyear = colDef(
          name = "Aktivitet",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#FFD700")
        ),
        tweets_curyear = colDef(
          name = "Tweets",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#FFD700")
        ),
        comments_curyear = colDef(
          name = "Kommentarer",
          align = "center",
          minWidth = 120,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#FFD700")
        )
      )
    )
  })

  #})

})


# Run the application
shinyApp(ui = ui, server = server)
