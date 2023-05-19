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
library(highcharter)

#auth0::use_auth0(overwrite = T)
#usethis::edit_r_environ()

# Define UI for application that draws a histogram
ui <- # Define UI for application that draws a histogram

  dashboardPage(skin = "black",title="#twittertinget",

    ### HEADER ###
    dbHeader <- dashboardHeader(disable = F, title = "#twittertinget v2.5", dropdownMenuOutput("ddmenu")),

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
            fluidRow(
              box(title = h3("Twittertinget Mandater"), width = 6, collapsible = T, solidHeader = F,
                tabsetPanel(type = "pills",
                  tabPanel("Twittertinget",
                    highchartOutput("twittertinget") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                  )
                )
              ),
              box(title = h3("Twittertinget Likes"), width = 6, collapsible = T, solidHeader = F,
                tabsetPanel(type = "pills", selected = "Denne Uge",
                  tabPanel(
                    "Denne Uge",
                    highchartOutput("twittertinget_curweek_likes") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                  ),
                  tabPanel(
                    "Denne Måned",
                    highchartOutput("twittertinget_curmonth_likes") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                  ),
                  tabPanel(
                    "Dette År",
                    highchartOutput("twittertinget_curyear_likes") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                  )
                )
              )
            ),
            box(title = h3("Scoreboard"), width = 12, collapsible = T, solidHeader = F,
              tabsetPanel(type = "pills", selected = "Idag",
                tabPanel(
                  "Idag",
                  reactableOutput("curday_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Igår",
                  reactableOutput("lastday_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Denne Uge",
                  reactableOutput("curweek_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Sidste Uge",
                  reactableOutput("lastweek_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Denne Måned",
                  reactableOutput("curmonth_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Sidste Måned",
                  reactableOutput("lastmonth_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Dette År",
                  reactableOutput("curyear_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Sidste År",
                  reactableOutput("lastyear_table") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                )
              )
            ),
            box(title = h3("Tweets"), width = 12, collapsible = T, solidHeader = T,
              tabsetPanel(type = "pills", selected = "Idag",
                tabPanel(
                  "Idag",
                  reactableOutput("curday_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Igår",
                  reactableOutput("lastday_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Denne Uge",
                  reactableOutput("curweek_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Sidste Uge",
                  reactableOutput("lastweek_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Denne Måned",
                  reactableOutput("curmonth_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Sidste Måned",
                  reactableOutput("lastmonth_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Dette År",
                  reactableOutput("curyear_tweets") %>% withSpinner(type = 2, color.background = "white", color="#438ccd", size = 3)
                ),
                tabPanel(
                  "Sidste År",
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

  # Load Data for Scoreboard ----
  dat <- reactive({

    # load data
    con <- someR::con_sql()
    res <- dbSendQuery(
      con,
      "SELECT
        username,
        name,
        profile_image_url,
        affiliation,
        party,
        variable,
        value
      FROM twitter_scoreboard
      WHERE list IN ('twittertinget')"
    )
    dat <- dbFetch(res, n = -1)
    dbClearResult(res)
    DBI::dbDisconnect(con)

    dat <- reshape2::dcast(
      dat,
      "username + name + profile_image_url + affiliation + party ~ variable",
      value.var = "value"
    )

    return(dat)

  })

  # Orchestrate Data for Vizulisation ----
  dat_twittertinget <- reactive({

    dat() %>% dplyr::select(
      username,
      name,
      affiliation,
      party,
      likes_curday,
      likes_curweek,
      likes_curmonth,
      likes_curyear,
      likes_lastday,
      likes_lastweek,
      likes_lastmonth,
      likes_lastyear
    ) -> dat

    # reorder
    dat[["affiliation"]] <- as.factor(dat[["affiliation"]])
    dat_viz[["affiliation"]] <- factor(dat_viz[["affiliation"]], levels = c(
      "Enhedslisten","Alternativet","Socialistisk Folkeparti",
      "Socialdemokratiet","Grønland","Radikale Venstre",
      "Moderaterne","Konservative","Færøerne",
      "Venstre","Liberal Alliance","Danmarksdemokraterne",
      "Dansk Folkeparti","Nye Borgerlige","Frigænger")
    )

    # summarize
    dat %>% dplyr::group_by(
      affiliation
    ) %>% dplyr::summarise(
      count = n()
    ) -> dat

    dat %>% dplyr::mutate(
      color = "#202123",
      color = ifelse(party == "A","#F40526",color),
      color = ifelse(party == "Æ","#4E77A3",color),
      color = ifelse(party == "B","#E82E8A",color),
      color = ifelse(party == "C","#165738",color),
      color = ifelse(party == "D","#134851",color),
      color = ifelse(party == "F","#EE9C9F",color),
      color = ifelse(party == "FRI","#BDBDBD",color),
      color = ifelse(party == "FØ","#BDBDBD",color),
      color = ifelse(party == "GL","#BDBDBD",color),
      color = ifelse(party == "I","#EEA925",color),
      color = ifelse(party == "M","#842990",color),
      color = ifelse(party == "O","#235CA9",color),
      color = ifelse(party == "Ø","#D0004E",color),
      color = ifelse(party == "V","#19438E",color),
      color = ifelse(party == "Å","#37BD00",color)
    ) -> dat

    return(dat)

  })

  # Load Data for Tweets ----
  dat_tweets <- reactive({

    # load data
    con <- someR::con_sql()
    res <- dbSendQuery(
      con,
      "SELECT
        *
        FROM twitter_tweets
        WHERE list IN ('twittertinget')
        AND variable IN (
          'name',
          'profile_image_url',
          'public_metrics_like_count',
          'public_metrics_retweet_count',
          'public_metrics_reply_count',
          'tweet_type',
          'text',
          'sentiment_mean',
          'sentiment_total',
          'impact'
        )
      "
    )
    dat <- dbFetch(res, n = -1)
    dbClearResult(res)
    DBI::dbDisconnect(con)

    dat <- reshape2::dcast(
      dat,
      formula = "id + username + created_at ~ variable"
    )

    # select columns and do filters
    dat %>% dplyr::select(
      profile_image_url,
      username,
      name,
      created_at,
      tweet_type,
      public_metrics_like_count,
      public_metrics_reply_count,
      public_metrics_retweet_count,
      impact,
      sentiment_total,
      text,
      id
    ) %>% dplyr::arrange(
      desc(created_at)
    ) -> dat_tweets

    # remove retweets and comments
    dat_tweets %>% dplyr::filter(
      tweet_type == "tweet"
    ) %>% dplyr::select(
      -tweet_type
    ) -> dat_tweets

    # derive time variables
    dat_tweets %>% dplyr::mutate(
      date = as.Date(created_at),
      year = lubridate::year(created_at),
      month = lubridate::month(created_at, label = T),
      week = lubridate::epiweek(date+1),
      wday = lubridate::wday(created_at, label = T),
      hour = lubridate::hour(created_at)
    ) -> dat_tweets

    # format
    dat_tweets %>% dplyr::mutate(
      id = as.character(id),
      username = as.character(username),
      name = as.character(name),
      profile_image_url = as.character(profile_image_url),
      created_at = as.character(created_at),
      public_metrics_like_count = as.numeric(public_metrics_like_count),
      impact = as.numeric(impact),
      public_metrics_retweet_count = as.numeric(public_metrics_retweet_count),
      public_metrics_reply_count = as.numeric(public_metrics_reply_count),
      sentiment_total = as.numeric(sentiment_total),
      text = as.character(text)
    ) -> dat_tweets

    # this day
    dat_tweets %>% dplyr::filter(
      date == Sys.Date(),
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) %>% dplyr::arrange(
      desc(sentiment_total)
    ) -> dat_tweets_curday

    # last day
    dat_tweets %>% dplyr::filter(
      date == Sys.Date()-1,
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) %>% dplyr::arrange(
      desc(sentiment_total)
    ) -> dat_tweets_lastday

    # this week
    dat_tweets %>% dplyr::filter(
      year == lubridate::year(Sys.Date()),
      week == lubridate::epiweek(Sys.Date()+1)
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) %>% dplyr::arrange(
      desc(sentiment_total)
    ) -> dat_tweets_curweek

    # last week
    dat_tweets %>% dplyr::filter(
      year == lubridate::year(Sys.Date()),
      week == lubridate::epiweek(Sys.Date()+1)-1
    ) %>% dplyr::select(
      -date,
      -year,
      -month,
      -week,
      -wday,
      -hour
    ) %>% dplyr::arrange(
      desc(sentiment_total)
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
    ) %>% dplyr::arrange(
      desc(sentiment_total)
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
    ) %>% dplyr::arrange(
      desc(sentiment_total)
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
    ) %>% dplyr::arrange(
      desc(sentiment_total)
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
    ) %>% dplyr::arrange(
      desc(sentiment_total)
    ) -> dat_tweets_lastyear

    out <- list(
      curday = dat_tweets_curday,
      lastday = dat_tweets_lastday,
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
    res <- dbSendQuery(con, "SELECT max(timestamp) FROM twitter_tweets_raw")
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
      ),
      notificationItem(
        logoutButton(label = "Log Out"),
        icon = shiny::icon("off", lib = "glyphicon")
      )
    )
  )

  ## Vizualize ----

  ## Twittertinget Mandater ----
  output$twittertinget <- renderHighchart({

    dat() %>% dplyr::select(
      username,
      name,
      affiliation,
      party
    ) -> dat

    m <- nrow(dat)

    # reorder
    dat[["affiliation"]] <- as.factor(dat[["affiliation"]])
    dat[["affiliation"]] <- factor(dat[["affiliation"]], levels = c(
      "Enhedslisten","Alternativet","Socialistisk Folkeparti",
      "Socialdemokratiet","Grønland","Radikale Venstre",
      "Moderaterne","Konservative","Færøerne",
      "Venstre","Liberal Alliance","Danmarksdemokraterne",
      "Dansk Folkeparti","Nye Borgerlige","Frigænger")
    )

    # summarize
    dat %>% dplyr::group_by(
      affiliation,party
    ) %>% dplyr::summarise(
      count = n()
    ) -> dat

    dat %>% dplyr::mutate(
      color = "#202123",
      color = ifelse(party == "A","#F40526",color),
      color = ifelse(party == "Æ","#4E77A3",color),
      color = ifelse(party == "B","#E82E8A",color),
      color = ifelse(party == "C","#165738",color),
      color = ifelse(party == "D","#134851",color),
      color = ifelse(party == "F","#EE9C9F",color),
      color = ifelse(party == "FRI","#BDBDBD",color),
      color = ifelse(party == "FØ","#BDBDBD",color),
      color = ifelse(party == "GL","#BDBDBD",color),
      color = ifelse(party == "I","#EEA925",color),
      color = ifelse(party == "M","#842990",color),
      color = ifelse(party == "O","#235CA9",color),
      color = ifelse(party == "Ø","#D0004E",color),
      color = ifelse(party == "V","#19438E",color),
      color = ifelse(party == "Å","#37BD00",color)
    ) -> dat

    plot <- hchart(
      dat,
      "item",
      hcaes(
        name = affiliation,
        y = count,
        #label = abbrv,
        color = color
      ),
      name = "",
      showInLegend = TRUE,
      size = "100%",
      center = list("50%", "75%"),
      startAngle = -100,
      endAngle  = 100
    ) %>%
      hc_title(text = "") %>%
      hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>') %>%
      hc_caption(text = paste0("Viser fordelingen af de ",m," folketingspolitikere der blev valgt ind ved sidste valg, som er på Twitter!")) %>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "output"
      )

    return(plot)

  })

  ## Twittertinget Likes på Ugen ----
  output$twittertinget_curweek_likes <- renderHighchart({

    dat() %>% dplyr::select(
      username,
      name,
      affiliation,
      party,
      likes_curday,
      likes_curweek,
      likes_curmonth,
      likes_curyear,
      likes_lastday,
      likes_lastweek,
      likes_lastmonth,
      likes_lastyear
    ) -> dat

    # reorder
    dat[["affiliation"]] <- as.factor(dat[["affiliation"]])
    dat[["affiliation"]] <- factor(dat[["affiliation"]], levels = c(
      "Enhedslisten","Alternativet","Socialistisk Folkeparti",
      "Socialdemokratiet","Grønland","Radikale Venstre",
      "Moderaterne","Konservative","Færøerne",
      "Venstre","Liberal Alliance","Danmarksdemokraterne",
      "Dansk Folkeparti","Nye Borgerlige","Frigænger")
    )

    # summarize
    m <- round(sum(dat[["likes_curweek"]],na.rm=T)/nrow(dat),0)
    dat %>% dplyr::group_by(
      affiliation,party
    ) %>% dplyr::summarise(
      count = round(sum(likes_curweek,na.rm=T)/m,0)
    ) -> dat

    dat %>% dplyr::mutate(
      color = "#202123",
      color = ifelse(party == "A","#F40526",color),
      color = ifelse(party == "Æ","#4E77A3",color),
      color = ifelse(party == "B","#E82E8A",color),
      color = ifelse(party == "C","#165738",color),
      color = ifelse(party == "D","#134851",color),
      color = ifelse(party == "F","#EE9C9F",color),
      color = ifelse(party == "FRI","#BDBDBD",color),
      color = ifelse(party == "FØ","#BDBDBD",color),
      color = ifelse(party == "GL","#BDBDBD",color),
      color = ifelse(party == "I","#EEA925",color),
      color = ifelse(party == "M","#842990",color),
      color = ifelse(party == "O","#235CA9",color),
      color = ifelse(party == "Ø","#D0004E",color),
      color = ifelse(party == "V","#19438E",color),
      color = ifelse(party == "Å","#37BD00",color)
    ) -> dat

    plot <- hchart(
      dat,
      "item",
      hcaes(
        name = affiliation,
        y = count,
        #label = abbrv,
        color = color
      ),
      name = "",
      showInLegend = TRUE,
      size = "100%",
      center = list("50%", "75%"),
      startAngle = -100,
      endAngle  = 100
    ) %>%
      hc_title(text = "") %>%
      hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>') %>%
      hc_caption(text = paste0("Viser fordelingen af mandater baseret på likes. I indeværende periode kræver et mandat pt. ",m," likes!")) %>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "output"
      )


    return(plot)

  })

  ## Twittertinget Likes på Måneden ----
  output$twittertinget_curmonth_likes <- renderHighchart({

    dat() %>% dplyr::select(
      username,
      name,
      affiliation,
      party,
      likes_curday,
      likes_curweek,
      likes_curmonth,
      likes_curyear,
      likes_lastday,
      likes_lastweek,
      likes_lastmonth,
      likes_lastyear
    ) -> dat

    # reorder
    dat[["affiliation"]] <- as.factor(dat[["affiliation"]])
    dat[["affiliation"]] <- factor(dat[["affiliation"]], levels = c(
      "Enhedslisten","Alternativet","Socialistisk Folkeparti",
      "Socialdemokratiet","Grønland","Radikale Venstre",
      "Moderaterne","Konservative","Færøerne",
      "Venstre","Liberal Alliance","Danmarksdemokraterne",
      "Dansk Folkeparti","Nye Borgerlige","Frigænger")
    )

    # summarize
    m <- round(sum(dat[["likes_curmonth"]],na.rm=T)/nrow(dat),0)
    dat %>% dplyr::group_by(
      affiliation,party
    ) %>% dplyr::summarise(
      count = round(sum(likes_curmonth,na.rm=T)/m,0)
    ) -> dat

    dat %>% dplyr::mutate(
      color = "#202123",
      color = ifelse(party == "A","#F40526",color),
      color = ifelse(party == "Æ","#4E77A3",color),
      color = ifelse(party == "B","#E82E8A",color),
      color = ifelse(party == "C","#165738",color),
      color = ifelse(party == "D","#134851",color),
      color = ifelse(party == "F","#EE9C9F",color),
      color = ifelse(party == "FRI","#BDBDBD",color),
      color = ifelse(party == "FØ","#BDBDBD",color),
      color = ifelse(party == "GL","#BDBDBD",color),
      color = ifelse(party == "I","#EEA925",color),
      color = ifelse(party == "M","#842990",color),
      color = ifelse(party == "O","#235CA9",color),
      color = ifelse(party == "Ø","#D0004E",color),
      color = ifelse(party == "V","#19438E",color),
      color = ifelse(party == "Å","#37BD00",color)
    ) -> dat

    plot <- hchart(
      dat,
      "item",
      hcaes(
        name = affiliation,
        y = count,
        #label = abbrv,
        color = color
      ),
      name = "",
      showInLegend = TRUE,
      size = "100%",
      center = list("50%", "75%"),
      startAngle = -100,
      endAngle  = 100
    ) %>%
      hc_title(text = "") %>%
      hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>') %>%
      hc_caption(text = paste0("Viser fordelingen af mandater baseret på likes. I indeværende periode kræver et mandat pt. ",m," likes!")) %>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "output"
      )

    return(plot)

  })

  ## Twittertinget Likes på Året ----
  output$twittertinget_curyear_likes <- renderHighchart({

    dat() %>% dplyr::select(
      username,
      name,
      affiliation,
      party,
      likes_curday,
      likes_curweek,
      likes_curmonth,
      likes_curyear,
      likes_lastday,
      likes_lastweek,
      likes_lastmonth,
      likes_lastyear
    ) -> dat

    # reorder
    dat[["affiliation"]] <- as.factor(dat[["affiliation"]])
    dat[["affiliation"]] <- factor(dat[["affiliation"]], levels = c(
      "Enhedslisten","Alternativet","Socialistisk Folkeparti",
      "Socialdemokratiet","Grønland","Radikale Venstre",
      "Moderaterne","Konservative","Færøerne",
      "Venstre","Liberal Alliance","Danmarksdemokraterne",
      "Dansk Folkeparti","Nye Borgerlige","Frigænger")
    )

    # summarize
    m <- round(sum(dat[["likes_curyear"]],na.rm=T)/nrow(dat),0)
    dat %>% dplyr::group_by(
      affiliation,party
    ) %>% dplyr::summarise(
      count = round(sum(likes_curyear,na.rm=T)/m,0)
    ) -> dat

    dat %>% dplyr::mutate(
      color = "#202123",
      color = ifelse(party == "A","#F40526",color),
      color = ifelse(party == "Æ","#4E77A3",color),
      color = ifelse(party == "B","#E82E8A",color),
      color = ifelse(party == "C","#165738",color),
      color = ifelse(party == "D","#134851",color),
      color = ifelse(party == "F","#EE9C9F",color),
      color = ifelse(party == "FRI","#BDBDBD",color),
      color = ifelse(party == "FØ","#BDBDBD",color),
      color = ifelse(party == "GL","#BDBDBD",color),
      color = ifelse(party == "I","#EEA925",color),
      color = ifelse(party == "M","#842990",color),
      color = ifelse(party == "O","#235CA9",color),
      color = ifelse(party == "Ø","#D0004E",color),
      color = ifelse(party == "V","#19438E",color),
      color = ifelse(party == "Å","#37BD00",color)
    ) -> dat

    plot <- hchart(
      dat,
      "item",
      hcaes(
        name = affiliation,
        y = count,
        #label = abbrv,
        color = color
      ),
      name = "",
      showInLegend = TRUE,
      size = "100%",
      center = list("50%", "75%"),
      startAngle = -100,
      endAngle  = 100
    ) %>%
      hc_title(text = "") %>%
      hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>') %>%
      hc_caption(text = paste0("Viser fordelingen af mandater baseret på likes. I indeværende periode kræver et mandat pt. ",m," likes!")) %>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "output"
      )

    return(plot)

  })

  # Orchestrate Data for Scoreboard ----
  dat_tables <- reactive({

    ## Write to Log ----
    log <- data.frame(
      time = Sys.time() + 60*60*2
    )
    write.table(
      log,
      "/home/kasper/someR/projects/folketinget/logs/visitors.csv", row.names = F, append = T, col.names = F
    )

    # Data for Current Day ----
    dat() %>% dplyr::select(
      username,
      name,
      profile_image_url,
      affiliation,
      party,

      followers_curday,
      activity_curday,
      tweets_curday,
      comments_curday,
      retweets_curday,
      likes_curday,
      impact_curday,
      commentsprtweet_curday
    ) %>% dplyr::arrange(
      desc(likes_curday)
    ) -> table_curday
    table_curday[["placering"]] <- 1:nrow(table_curday)
    #table_curday[is.na(table_curday)] <- 0

    table_curday %>% dplyr::select(
      profile_image_url,
      name,
      followers_curday,
      affiliation,
      party,

      placering,
      likes_curday,
      impact_curday,
      commentsprtweet_curday,
      retweets_curday,
      activity_curday,
      tweets_curday,
      comments_curday
    ) -> table_curday

    # Data for Last Day ----
    dat() %>% dplyr::select(
      username,
      name,
      profile_image_url,
      affiliation,
      party,

      followers_lastday,
      activity_lastday,
      tweets_lastday,
      comments_lastday,
      retweets_lastday,
      likes_lastday,
      impact_lastday,
      commentsprtweet_lastday
    ) %>% dplyr::arrange(
      desc(likes_lastday)
    ) -> table_lastday
    table_lastday[["placering"]] <- 1:nrow(table_lastday)
    #table_lastday[is.na(table_lastday)] <- 0

    table_lastday %>% dplyr::select(
      profile_image_url,
      name,
      followers_lastday,
      affiliation,
      party,

      placering,
      likes_lastday,
      impact_lastday,
      commentsprtweet_lastday,
      retweets_lastday,
      activity_lastday,
      tweets_lastday,
      comments_lastday
    ) -> table_lastday

    # Data for Current Week ----
    dat() %>% dplyr::select(
      username,
      name,
      profile_image_url,
      affiliation,
      party,

      followers_curweek,
      activity_curweek,
      tweets_curweek,
      comments_curweek,
      retweets_curweek,
      likes_curweek,
      impact_curweek,
      commentsprtweet_curweek
    ) %>% dplyr::arrange(
      desc(likes_curweek)
    ) -> table_curweek
    table_curweek[["placering"]] <- 1:nrow(table_curweek)
    #table_curweek[is.na(table_curweek)] <- 0

    table_curweek %>% dplyr::select(
      profile_image_url,
      name,
      followers_curweek,
      affiliation,
      party,

      placering,
      likes_curweek,
      impact_curweek,
      commentsprtweet_curweek,
      retweets_curweek,
      activity_curweek,
      tweets_curweek,
      comments_curweek
    ) -> table_curweek

    # Data for Last Week ----
    dat() %>% dplyr::select(
      username,
      name,
      profile_image_url,
      affiliation,
      party,

      followers_lastweek,
      activity_lastweek,
      tweets_lastweek,
      comments_lastweek,
      retweets_lastweek,
      likes_lastweek,
      impact_lastweek,
      commentsprtweet_lastweek
    ) %>% dplyr::arrange(
      desc(likes_lastweek)
    ) -> table_lastweek
    table_lastweek[["placering"]] <- 1:nrow(table_lastweek)
    #table_lastweek[is.na(table_lastweek)] <- 0

    table_lastweek %>% dplyr::select(
      profile_image_url,
      name,
      followers_lastweek,
      affiliation,
      party,

      placering,
      likes_lastweek,
      impact_lastweek,
      commentsprtweet_lastweek,
      retweets_lastweek,
      activity_lastweek,
      tweets_lastweek,
      comments_lastweek
    ) -> table_lastweek

    # Data for Current Month ----
    dat() %>% dplyr::select(
      username,
      name,
      profile_image_url,
      affiliation,
      party,

      followers_curmonth,
      activity_curmonth,
      tweets_curmonth,
      comments_curmonth,
      retweets_curmonth,
      likes_curmonth,
      impact_curmonth,
      commentsprtweet_curmonth
    ) %>% dplyr::arrange(
      desc(likes_curmonth)
    ) -> table_curmonth
    table_curmonth[["placering"]] <- 1:nrow(table_curmonth)
    #table_curmonth[is.na(table_curmonth)] <- 0

    table_curmonth %>% dplyr::select(
      profile_image_url,
      name,
      followers_curmonth,
      affiliation,
      party,

      placering,
      likes_curmonth,
      impact_curmonth,
      commentsprtweet_curmonth,
      retweets_curmonth,
      activity_curmonth,
      tweets_curmonth,
      comments_curmonth
    ) -> table_curmonth

    # Data for Last Month ----
    dat() %>% dplyr::select(
      username,
      name,
      profile_image_url,
      affiliation,
      party,

      followers_lastmonth,
      activity_lastmonth,
      tweets_lastmonth,
      comments_lastmonth,
      retweets_lastmonth,
      likes_lastmonth,
      impact_lastmonth,
      commentsprtweet_lastmonth
    ) %>% dplyr::arrange(
      desc(likes_lastmonth)
    ) -> table_lastmonth
    table_lastmonth[["placering"]] <- 1:nrow(table_lastmonth)
    #table_lastmonth[is.na(table_lastmonth)] <- 0

    table_lastmonth %>% dplyr::select(
      profile_image_url,
      name,
      followers_lastmonth,
      affiliation,
      party,

      placering,
      likes_lastmonth,
      impact_lastmonth,
      commentsprtweet_lastmonth,
      retweets_lastmonth,
      activity_lastmonth,
      tweets_lastmonth,
      comments_lastmonth
    ) -> table_lastmonth

    # Data for Current Year ----
    dat() %>% dplyr::select(
      username,
      name,
      profile_image_url,
      affiliation,
      party,

      followers_curyear,
      activity_curyear,
      tweets_curyear,
      comments_curyear,
      retweets_curyear,
      likes_curyear,
      impact_curyear,
      commentsprtweet_curyear
    ) %>% dplyr::arrange(
      desc(likes_curyear)
    ) -> table_curyear
    table_curyear[["placering"]] <- 1:nrow(table_curyear)
    #table_curyear[is.na(table_curyear)] <- 0

    table_curyear %>% dplyr::select(
      profile_image_url,
      name,
      followers_curyear,
      affiliation,
      party,

      placering,
      likes_curyear,
      impact_curyear,
      commentsprtweet_curyear,
      retweets_curyear,
      activity_curyear,
      tweets_curyear,
      comments_curyear
    ) -> table_curyear

    # Data for Last Year ----
    dat() %>% dplyr::select(
      username,
      name,
      profile_image_url,
      affiliation,
      party,

      followers_lastyear,
      activity_lastyear,
      tweets_lastyear,
      comments_lastyear,
      retweets_lastyear,
      likes_lastyear,
      impact_lastyear,
      commentsprtweet_lastyear
    ) %>% dplyr::arrange(
      desc(likes_lastyear)
    ) -> table_lastyear
    table_lastyear[["placering"]] <- 1:nrow(table_lastyear)
    #table_lastyear[is.na(table_lastyear)] <- 0

    table_lastyear %>% dplyr::select(
      profile_image_url,
      name,
      followers_lastyear,
      affiliation,
      party,

      placering,
      likes_lastyear,
      impact_lastyear,
      commentsprtweet_lastyear,
      retweets_lastyear,
      activity_lastyear,
      tweets_lastyear,
      comments_lastyear
    ) -> table_lastyear

    # put into list
    out <- list(
      curday = table_curday,
      lastday = table_lastday,
      curweek = table_curweek,
      lastweek = table_lastweek,
      curmonth = table_curmonth,
      lastmonth = table_lastmonth,
      curyear = table_curyear,
      lastyear = table_lastyear
    )

    return(out)

  })

  # Scoreboards ----

  ## Current Day Scoreboard ----
  output$curday_table <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tables()[["curday"]],
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
        followers_curday = colDef(
          name = paste(emo::ji("man"),emo::ji("woman"),"Følgere", sep = " "),
          align = "center",
          minWidth = 80
        ),
        affiliation  = colDef(
          name = paste(emo::ji("office"),"", sep = " "),
          align = "center",
          minWidth = 80,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party  = colDef(
          name = paste(emo::ji("abc"),"", sep = " "),
          align = "center",
          minWidth = 40,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
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
        likes_curday = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        impact_curday = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_curday = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_curday = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_curday = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_curday = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_curday = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curday"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Politiker", columns = c("name", "profile_image_url", "followers_curday")),
        colGroup(name = "Parti", columns = c("affiliation", "party")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curday", "impact_curday","commentsprtweet_curday","retweets_curday"))
      )
    )
  })

  # Current Day Tweets ----
  output$curday_tweets <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tweets()[["curday"]],
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
        username = colDef(
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
        public_metrics_like_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 40
        ),
        impact = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 40
        ),
        sentiment_total = colDef(
          name = paste(emo::ji("yellow heart"),"Happiness (BETA)", sep = " "),
          align = "center",
          minWidth = 160,
          sortNALast = TRUE
        ),
        public_metrics_reply_count = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 40
        ),
        public_metrics_retweet_count = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 40
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["curday"]]$username[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ## Last Day Scoreboard ----
  output$lastday_table <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tables()[["lastday"]],
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
        followers_lastday = colDef(
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
        affiliation  = colDef(
          name = paste(emo::ji("office"),"", sep = " "),
          align = "center",
          minWidth = 80,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party  = colDef(
          name = paste(emo::ji("abc"),"", sep = " "),
          align = "center",
          minWidth = 40,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        likes_lastday = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        impact_lastday = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        commentsprtweet_lastday = colDef(
          name = paste(emo::ji("handshake"),"Engagement", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        activity_lastday = colDef(
          name = paste(emo::ji("runner"),"Aktivitet", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        tweets_lastday = colDef(
          name = paste(emo::ji("bird"),"Tweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        comments_lastday = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastday"]], font_color = "black", highlighter = "#ECECEC")
        ),
        retweets_lastday = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastday"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Politiker", columns = c("name", "profile_image_url", "followers_lastday")),
        colGroup(name = "Parti", columns = c("affiliation", "party")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastday", "impact_lastday","commentsprtweet_lastday","retweets_lastday"))
      )
    )
  })

  # Last Day Tweets ----
  output$lastday_tweets <- renderReactable({

    reactable(
      theme = fivethirtyeight(),
      dat_tweets()[["lastday"]],
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
        username = colDef(
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
        public_metrics_like_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 40
        ),
        impact = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 40
        ),
        sentiment_total = colDef(
          name = paste(emo::ji("yellow heart"),"Happiness (BETA)", sep = " "),
          align = "center",
          minWidth = 160,
          sortNALast = TRUE
        ),
        public_metrics_reply_count = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 40
        ),
        public_metrics_retweet_count = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 40
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["lastday"]]$username[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ## Current Week Scoreboard ----
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
        affiliation  = colDef(
          name = paste(emo::ji("office"),"", sep = " "),
          align = "center",
          minWidth = 80,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party  = colDef(
          name = paste(emo::ji("abc"),"", sep = " "),
          align = "center",
          minWidth = 40,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        likes_curweek = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        impact_curweek = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
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
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curweek"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Politiker", columns = c("name", "profile_image_url", "followers_curweek")),
        colGroup(name = "Parti", columns = c("affiliation", "party")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curweek", "impact_curweek","commentsprtweet_curweek","retweets_curweek"))
      )
    )
  })

  # Current Week Tweets ----
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
        username = colDef(
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
        public_metrics_like_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 40
        ),
        impact = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 40
        ),
        sentiment_total = colDef(
          name = paste(emo::ji("yellow heart"),"Happiness (BETA)", sep = " "),
          align = "center",
          minWidth = 160,
          sortNALast = TRUE
        ),
        public_metrics_reply_count = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 40
        ),
        public_metrics_retweet_count = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 40
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["curweek"]]$username[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ## Last Week Scoreboard ----
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
        affiliation  = colDef(
          name = paste(emo::ji("office"),"", sep = " "),
          align = "center",
          minWidth = 80,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party  = colDef(
          name = paste(emo::ji("abc"),"", sep = " "),
          align = "center",
          minWidth = 40,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        likes_lastweek = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        ),
        impact_lastweek = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
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
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastweek"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Politiker", columns = c("name", "profile_image_url", "followers_lastweek")),
        colGroup(name = "Parti", columns = c("affiliation", "party")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastweek", "impact_lastweek","commentsprtweet_lastweek","retweets_lastweek"))
      )
    )
  })

  # Last Week Tweets ----
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
        username = colDef(
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
        public_metrics_like_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 40
        ),
        impact = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 40
        ),
        sentiment_total = colDef(
          name = paste(emo::ji("yellow heart"),"Happiness (BETA)", sep = " "),
          align = "center",
          minWidth = 160,
          sortNALast = TRUE
        ),
        public_metrics_reply_count = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 40
        ),
        public_metrics_retweet_count = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 40
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["lastweek"]]$username[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ## Current Month Scoreboard ----
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
        affiliation  = colDef(
          name = paste(emo::ji("office"),"", sep = " "),
          align = "center",
          minWidth = 80,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party  = colDef(
          name = paste(emo::ji("abc"),"", sep = " "),
          align = "center",
          minWidth = 40,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        likes_curmonth = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        impact_curmonth = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
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
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curmonth"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Politiker", columns = c("name", "profile_image_url", "followers_curmonth")),
        colGroup(name = "Parti", columns = c("affiliation", "party")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curmonth", "impact_curmonth","commentsprtweet_curmonth","retweets_curmonth"))
      )
    )
  })

  # Current Month Tweets ----
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
        username = colDef(
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
        public_metrics_like_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 40
        ),
        impact = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 40
        ),
        sentiment_total = colDef(
          name = paste(emo::ji("yellow heart"),"Happiness (BETA)", sep = " "),
          align = "center",
          minWidth = 160,
          sortNALast = TRUE
        ),
        public_metrics_reply_count = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 40
        ),
        public_metrics_retweet_count = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 40
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["curmonth"]]$username[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ## Last Month Scoreboard ----
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
        affiliation  = colDef(
          name = paste(emo::ji("office"),"", sep = " "),
          align = "center",
          minWidth = 80,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party  = colDef(
          name = paste(emo::ji("abc"),"", sep = " "),
          align = "center",
          minWidth = 40,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        likes_lastmonth = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        ),
        impact_lastmonth = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
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
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastmonth"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Politiker", columns = c("name", "profile_image_url", "followers_lastmonth")),
        colGroup(name = "Parti", columns = c("affiliation", "party")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastmonth", "impact_lastmonth","commentsprtweet_lastmonth","retweets_lastmonth"))
      )
    )
  })

  # Last Month Tweets ----
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
        username = colDef(
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
        public_metrics_like_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 40
        ),
        impact = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 40
        ),
        sentiment_total = colDef(
          name = paste(emo::ji("yellow heart"),"Happiness (BETA)", sep = " "),
          align = "center",
          minWidth = 160,
          sortNALast = TRUE
        ),
        public_metrics_reply_count = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 40
        ),
        public_metrics_retweet_count = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 40
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["lastmonth"]]$username[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ## Current Year Scoreboard ----
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
        affiliation  = colDef(
          name = paste(emo::ji("office"),"", sep = " "),
          align = "center",
          minWidth = 80,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party  = colDef(
          name = paste(emo::ji("abc"),"", sep = " "),
          align = "center",
          minWidth = 40,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        likes_curyear = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        impact_curyear = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
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
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["curyear"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Politiker", columns = c("name", "profile_image_url", "followers_curyear")),
        colGroup(name = "Parti", columns = c("affiliation", "party")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_curyear", "impact_curyear","commentsprtweet_curyear","retweets_curyear"))
      )
    )
  })

  # Current Year Tweets ----
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
        username = colDef(
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
        public_metrics_like_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 40
        ),
        impact = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 40
        ),
        sentiment_total = colDef(
          name = paste(emo::ji("yellow heart"),"Happiness (BETA)", sep = " "),
          align = "center",
          minWidth = 160,
          sortNALast = TRUE
        ),
        public_metrics_reply_count = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 40
        ),
        public_metrics_retweet_count = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 40
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["curyear"]]$username[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })

  ## Last Year Scoreboard ----
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
        affiliation  = colDef(
          name = paste(emo::ji("office"),"", sep = " "),
          align = "center",
          minWidth = 80,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        party  = colDef(
          name = paste(emo::ji("abc"),"", sep = " "),
          align = "center",
          minWidth = 40,
          style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
        ),
        likes_lastyear = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        ),
        impact_lastyear = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
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
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 80,
          style = highlight_max(dat_tables()[["lastyear"]], font_color = "black", highlighter = "#ECECEC")
        )
      ),
      columnGroups = list(
        colGroup(name = "Politiker", columns = c("name", "profile_image_url", "followers_lastyear")),
        colGroup(name = "Parti", columns = c("affiliation", "party")),
        colGroup(name = "Scoreboard", columns = c("placering", "likes_lastyear", "impact_lastyear","commentsprtweet_lastyear","retweets_lastyear"))
      )
    )
  })

  # Tweets Last Year Table ----
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
        username = colDef(
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
        public_metrics_like_count = colDef(
          name = paste(emo::ji("heart"),"Likes", sep = " "),
          align = "center",
          minWidth = 40
        ),
        impact = colDef(
          name = paste(emo::ji("collision"),"Impact", sep = " "),
          align = "center",
          minWidth = 40
        ),
        sentiment_total = colDef(
          name = paste(emo::ji("yellow heart"),"Happiness (BETA)", sep = " "),
          align = "center",
          minWidth = 160,
          sortNALast = TRUE
        ),
        public_metrics_reply_count = colDef(
          name = paste(emo::ji("left_speech_bubble"),"Kommentarer", sep = " "),
          align = "center",
          minWidth = 40
        ),
        public_metrics_retweet_count = colDef(
          name = paste(emo::ji("recycling_symbol"),"Retweets", sep = " "),
          align = "center",
          minWidth = 40
        ),
        text = colDef(
          name = paste(emo::ji("bird"),"Tweet", sep = " "),
          align = "left",
          minWidth = 720
        ),
        id = colDef(
          name = paste(emo::ji("bird"),emo::ji("link"), sep = ""),
          align = "center",
          minWidth = 60,
          html = TRUE,
          cell = function(value, index) {
            url <- paste0("https://twitter.com/",dat_tweets()[["lastyear"]]$username[index],"/status/",value)
            shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
          }
        )
      )
    )
  })
})


# Run the application
shinyApp(ui = ui, server = server)
#auth0::shinyAppAuth0(ui, server)
