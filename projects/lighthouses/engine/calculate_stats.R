# libs
library(DBI)
library(odbc)
library(RMySQL)

library(dplyr)
library(reshape2)
library(lubridate)

######################
### LOAD AND CLEAN ###
######################
# make con
con <- someR::con_sql()

# get data
res <- dbSendQuery(con, "SELECT * FROM twitter_lighthouses_tl_clean")
dat <- dbFetch(res, n = -1)
dbClearResult(res)

# prep and clean
dat %>% dplyr::select(
  -mentions_user_id,
  -mentions_screen_name,
  -quoted_user_id,
  -quoted_screen_name
) -> dat

# "NA" to NA
dat %>% dplyr::mutate(
  across(
    .cols = everything(),
    ~na_if(.,"NA")
  )
) -> dat

# make columns defining type of tweets
dat[["tweet_type"]] <- NA
dat %>% dplyr::mutate(
  tweet_type = ifelse(
    is.na(reply_to_status_id) == T,"tweet","comment"
  ),
  tweet_type = ifelse(
    is_quote == 1,"retweet",tweet_type
  ),
  tweet_type = ifelse(
    is_retweet == 1,"retweet",tweet_type
  ),
) -> dat

# arrange
dat %>% dplyr::arrange(
  user_id,
  desc(created_at)
) -> dat

# derive time variables
dat %>% dplyr::mutate(
  date = as.Date(created_at),
  year = lubridate::year(created_at),
  month = lubridate::month(created_at, label = T),
  week = lubridate::week(created_at),
  wday = lubridate::wday(created_at, label = T),
  hour = lubridate::hour(created_at)
) -> dat

#####################
### GENERAL STATS ###
#####################

# first we derive the dates/time variables we need
# the excersize is somewhat complex so we do it up front instead
# we use the rollback function from lubridate to get last month
curdate <- Sys.Date()
#curdate <- as.Date("2022-01-01")

# current
cw <-lubridate::week(curdate)
cm <- lubridate::month(curdate, label = T)
cy <- lubridate::year(curdate)

# last
lw <- lubridate::week(curdate-7)
lm <- lubridate::month(lubridate::rollback(curdate), label = T)
ly <- lubridate::year(curdate) - 1

# in the case of first week in year then define y which is they last year
y <- cy
if(cw == 1){
  y <- ly
}

# current week
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::filter(
  year == cy,
  week == cw
) %>% dplyr::summarise(
  activity_curweek = n(),
  likes_curweek = sum(favorite_count),
  likes_mean_curweek = mean(favorite_count),
  tweets_curweek = sum(tweet_type == "tweet"),
  comments_curweek = sum(tweet_type == "comment"),
  retweets_curweek = sum(tweet_type == "retweet"),
  commentsprtweet_curweek = round(comments_curweek/tweets_curweek,2)
) -> dat_curweek

# last week
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::filter(
  year == y,
  week == lw
) %>% dplyr::summarise(
  activity_lastweek = n(),
  likes_lastweek = sum(favorite_count),
  likes_mean_lastweek = mean(favorite_count),
  tweets_lastweek = sum(tweet_type == "tweet"),
  comments_lastweek = sum(tweet_type == "comment"),
  retweets_lastweek = sum(tweet_type == "retweet"),
  commentsprtweet_lastweek = round(comments_lastweek/tweets_lastweek,2)
) -> dat_lastweek

# current month
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::filter(
  year == cy,
  month == cm
) %>% dplyr::summarise(
  activity_curmonth = n(),
  likes_curmonth = sum(favorite_count),
  likes_mean_curmonth = mean(favorite_count),
  tweets_curmonth = sum(tweet_type == "tweet"),
  comments_curmonth = sum(tweet_type == "comment"),
  retweets_curmonth = sum(tweet_type == "retweet"),
  commentsprtweet_curmonth = round(comments_curmonth/tweets_curmonth,2)
) -> dat_curmonth

# last month
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::filter(
  year == y,
  month == lm
) %>% dplyr::summarise(
  activity_lastmonth = n(),
  likes_lastmonth = sum(favorite_count),
  likes_mean_lastmonth = mean(favorite_count),
  tweets_lastmonth = sum(tweet_type == "tweet"),
  comments_lastmonth = sum(tweet_type == "comment"),
  retweets_lastmonth = sum(tweet_type == "retweet"),
  commentsprtweet_lastmonth = round(comments_lastmonth/tweets_lastmonth,2)
) -> dat_lastmonth

# current year
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::filter(
  year == cy
) %>% dplyr::summarise(
  activity_curyear = n(),
  likes_curyear = sum(favorite_count),
  likes_mean_curyear = mean(favorite_count),
  tweets_curyear = sum(tweet_type == "tweet"),
  comments_curyear = sum(tweet_type == "comment"),
  retweets_curyear = sum(tweet_type == "retweet"),
  commentsprtweet_curyear = round(comments_curyear/tweets_curyear,2)
) -> dat_curyear

# last year
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::filter(
  year == ly
) %>% dplyr::summarise(
  activity_lastyear = n(),
  likes_lastyear = sum(favorite_count),
  likes_mean_lastyear = mean(favorite_count),
  tweets_lastyear = sum(tweet_type == "tweet"),
  comments_lastyear = sum(tweet_type == "comment"),
  retweets_lastyear = sum(tweet_type == "retweet"),
  commentsprtweet_lastyear = round(comments_lastyear/tweets_lastyear,2)
) -> dat_lastyear

### JOIN ###

# get master data
res <- dbSendQuery(con, "SELECT * FROM twitter_lighthouses_master")
dat_t <- dbFetch(res, n = -1)
dbClearResult(res)

# get latest
dat_t %>% dplyr::filter(
  timestamp == max(timestamp)
) %>% select(
  -timestamp
) -> dat_t

# reshape
dat_t <- reshape2::dcast(
  dat_t,
  user_id ~ variable
)

dat_t %>% dplyr::select(
  screen_name,
  name,
  profile_image_url
) -> dat_t

# join
dat_t <- dplyr::left_join(
  dat_t,
  dat_curweek,
  c("screen_name" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_curmonth,
  c("screen_name" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_curyear,
  c("screen_name" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_lastweek,
  c("screen_name" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_lastmonth,
  c("screen_name" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_lastyear,
  c("screen_name" = "screen_name")
)

# add followers and friends
dat %>% dplyr::select(
  screen_name,
  followers_count,
  timestamp
) %>% dplyr::group_by(
  screen_name
) %>% dplyr::slice(
  which.max(as.Date(timestamp))
) %>% dplyr::select(
  -timestamp
) -> dat_followers

dat_t <- dplyr::left_join(
  dat_t,
  dat_followers,
  by = c("screen_name" = "screen_name")
)

# long to wide
dat_out <- reshape2::melt(
  dat_t,
  id.vars = c(
    "screen_name", "name","profile_image_url"
  )
)

# write to db
dbSendQuery(con, "SET GLOBAL local_infile = true;")
dbWriteTable(
  con,
  "twitter_lighthouses_tl_stats",
  dat_out,
  overwrite = T,
  append = F,
  row.names = F
)

dbDisconnect(con)

