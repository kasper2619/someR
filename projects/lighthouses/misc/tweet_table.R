# libs
library(dplyr)
library(reactable)
library(reactablefmtr)
library(someR)

# get data
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

# select columns
dat %>% dplyr::filter(
  is.na(reply_to_status_id) == T,
  is_retweet == 0
) %>% dplyr::select(
  profile_image_url,
  name,
  screen_name,
  created_at,
  favorite_count,
  text,
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

### REACTABLE ###
reactable(
  theme = fivethirtyeight(),
  dat_tweets_curweek,
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
      minWidth = 180
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
      minWidth = 120
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
        url <- paste0("https://twitter.com/",dat_tweets_curweek$screen_name[index],"/status/",value)
        shiny::tags$a(href = url,style="text-decoration: none;", target = "_blank", emo::ji("link"))
      }
    )
  )
)
