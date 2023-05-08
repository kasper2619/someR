# Libraries ----
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)
library(reshape2)
library(lubridate)

# Get Data ----

## Get Tweets ----
con <- someR::con_sql()
res <- dbSendQuery(
  con,
    "SELECT * FROM twitter_tweets_raw
      WHERE VARIABLE IN (
        'created_at',
        'in_reply_to_user_id',
        'public_metrics_like_count',
        'public_metrics_quote_count',
        'public_metrics_reply_count',
        'public_metrics_retweet_count',
        'referenced_tweets_id',
        'referenced_tweets_type',
        'text'
  )"
)
dat_tweets <- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)

# filter latest
dat_tweets %>% dplyr::group_by(
  author_id,
  id
) %>% dplyr::filter(
  timestamp == max(timestamp)
) -> dat_tweets

# reshape
dat_tweets <- reshape2::dcast(
  dat_tweets,
  formula = "id + author_id ~ variable",
)

## Get Users ----
con <- someR::con_sql()
res <- dbSendQuery(
  con,
    "SELECT * FROM twitter_users_raw
      WHERE VARIABLE IN (
        'id',
        'blok',
        'list',
        'party',
        'name',
        'profile_image_url',
        'username',
        'affiliation'
  )"
)
dat_users <- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)

# filter latest
dat_users %>% dplyr::group_by(
  id
) %>% dplyr::filter(
  timestamp == max(timestamp)
) -> dat_users

# reshape
dat_users <- reshape2::dcast(
  dat_users,
  formula = "id ~ variable",
  value.var = "value"
)

# Model ----

## Join Tweets and Users ----
dat <- dplyr::left_join(
  dat_tweets,dat_users,
  by = c("author_id" = "id")
)

# clean memory
rm(dat_tweets,dat_users)

## Rename and Select ----
dat %>% dplyr::select(
  id,
  created_at,
  author_id,
  username,
  name,
  list,
  profile_image_url,
  affiliation,
  party,
  blok,
  public_metrics_like_count,
  public_metrics_quote_count,
  public_metrics_retweet_count,
  public_metrics_reply_count,
  referenced_tweets_type,
  text
) -> dat

## "NA" to NA ----
dat %>% dplyr::mutate(
  across(
    .cols = everything(),
    ~na_if(.,"NA")
  )
) -> dat

## Calculate Sentiments----
dat[["sentiment_mean"]] <- sapply(
  dat[["text"]],
  output = "mean",
  FUN = Sentida::sentida
)
dat[["sentiment_mean"]] <- round(dat[["sentiment_mean"]],2)

dat[["sentiment_total"]] <- sapply(
  dat[["text"]],
  output = "total",
  FUN = Sentida::sentida

)
dat[["sentiment_total"]] <- round(dat[["sentiment_total"]],2)

# make columns defining type of tweets
dat %>% dplyr::rename(
  "tweet_type" = referenced_tweets_type
) -> dat
dat %>% dplyr::mutate(
  tweet_type = ifelse(is.na(tweet_type) == T,"tweet",tweet_type),
  tweet_type = ifelse(tweet_type == "replied_to","comment",tweet_type),
  tweet_type = ifelse(tweet_type == "retweeted","retweet",tweet_type),
  tweet_type = ifelse(tweet_type == "quoted","retweet",tweet_type),
  tweet_type = ifelse(tweet_type == "quoted, replied_to","retweet",tweet_type),
  tweet_type = ifelse(tweet_type == "","tweet",tweet_type),
) -> dat

# arrange
dat %>% dplyr::arrange(
  author_id,
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

## Follower Development ----
con <- someR::con_sql()
res <- dbSendQuery(
  con,
  "SELECT * FROM twitter_users_raw"
)
dat_users <- dbFetch(res, n = -1)
dbClearResult(res)

dat_users %>% dplyr::filter(
  variable == "public_metrics_followers_count"
) %>% dplyr::select(
  -variable
) %>% dplyr::mutate(
  timestamp = as.Date(timestamp)
) -> dat_followers

dat_followers %>% dplyr::group_by(
  id,
  timestamp
) %>% dplyr::summarise(
  followers_count = round(mean(as.numeric(value), na.rm = T),0)
) -> dat_followers

# merge followers into data
dat <- dplyr::left_join(
  dat,
  dat_followers,
  by = c(
    "author_id" = "id",
    "date" = "timestamp"
  )
)

# fill NA's with last value
dat %>% dplyr::arrange(
  author_id,
  desc(created_at)
) -> dat

dat %>% dplyr::group_by(
  author_id
) %>% tidyr::fill(
  followers_count,
  .direction  = "downup"
) -> dat

## Format Variables ----
dat %>% dplyr::mutate(
  public_metrics_like_count = as.numeric(public_metrics_like_count),
  public_metrics_quote_count = as.numeric(public_metrics_quote_count),
  public_metrics_retweet_count = as.numeric(public_metrics_retweet_count),
  public_metrics_reply_count = as.numeric(public_metrics_reply_count)
) -> dat

## Calculate Impact ----
dat %>% dplyr::mutate(
  impact = round(public_metrics_like_count / followers_count,2)
) -> dat

# convert date to character
dat[["created_at"]] <- as_datetime(dat[["created_at"]])

# Write to Tweet Table ----
dat_tweets <- reshape2::melt(
  dat,
  id.vars = c(
    "id","username","created_at","list"
  )
)

dbSendQuery(con, "SET GLOBAL local_infile = true;")
dbWriteTable(
  con,
  "twitter_tweets",
  dat_tweets,
  overwrite = T,
  append = F,
  row.names = F
)

dbDisconnect(con)

# Make Statistics for Scoreboard ----

# first we derive the dates/time variables we need
# the excersize is somewhat complex so we do it up front instead
# we use the rollback function from lubridate to get last month
curdate <- Sys.Date()
#curdate <- as.Date("2022-01-01")

# current
cd <- curdate
cw <- lubridate::week(curdate)
cm <- lubridate::month(curdate, label = T)
cy <- lubridate::year(curdate)

# last
ld <- curdate - 1
lw <- lubridate::week(curdate-7)
lm <- lubridate::month(lubridate::rollback(curdate), label = T)
ly <- lubridate::year(curdate) - 1

# in the case of first week in year then define y which is they last year
y <- cy
if(cw == 1){
  y <- ly
}

## Current Day ----
dat %>% dplyr::group_by(
  username
) %>% dplyr::filter(
  date == cd
) %>% dplyr::summarise(
  activity_curday = n(),
  likes_curday = sum(public_metrics_like_count),
  likes_mean_curday = mean(public_metrics_like_count),
  tweets_curday = sum(tweet_type == "tweet"),
  comments_curday = sum(tweet_type == "comment"),
  retweets_curday = sum(tweet_type == "retweet"),
  commentsprtweet_curday = round(comments_curday/tweets_curday,2),
  followers_curday = first(followers_count),
  impact_curday = round(mean(impact,na.rm = T),2)
) -> dat_curday

## Last Day ----
dat %>% dplyr::group_by(
  username
) %>% dplyr::filter(
  date == ld
) %>% dplyr::summarise(
  activity_lastday = n(),
  likes_lastday = sum(public_metrics_like_count),
  likes_mean_lastday = mean(public_metrics_like_count),
  tweets_lastday = sum(tweet_type == "tweet"),
  comments_lastday = sum(tweet_type == "comment"),
  retweets_lastday = sum(tweet_type == "retweet"),
  commentsprtweet_lastday = round(comments_lastday/tweets_lastday,2),
  followers_lastday = first(followers_count),
  impact_lastday = round(mean(impact,na.rm = T)*100,2)
) -> dat_lastday

## Current Week ----
dat %>% dplyr::group_by(
  username
) %>% dplyr::filter(
  year == cy,
  week == cw
) %>% dplyr::summarise(
  activity_curweek = n(),
  likes_curweek = sum(public_metrics_like_count),
  likes_mean_curweek = mean(public_metrics_like_count),
  tweets_curweek = sum(tweet_type == "tweet"),
  comments_curweek = sum(tweet_type == "comment"),
  retweets_curweek = sum(tweet_type == "retweet"),
  commentsprtweet_curweek = round(comments_curweek/tweets_curweek,2),
  followers_curweek = first(followers_count),
  impact_curweek = round(mean(impact,na.rm = T)*100,2)
) -> dat_curweek

## Last Week ----
dat %>% dplyr::group_by(
  username
) %>% dplyr::filter(
  year == y,
  week == lw
) %>% dplyr::summarise(
  activity_lastweek = n(),
  likes_lastweek = sum(public_metrics_like_count),
  likes_mean_lastweek = mean(public_metrics_like_count),
  tweets_lastweek = sum(tweet_type == "tweet"),
  comments_lastweek = sum(tweet_type == "comment"),
  retweets_lastweek = sum(tweet_type == "retweet"),
  commentsprtweet_lastweek = round(comments_lastweek/tweets_lastweek,2),
  followers_lastweek = first(followers_count),
  impact_lastweek = round(mean(impact,na.rm = T)*100,2)
) -> dat_lastweek

## Current Month ----
dat %>% dplyr::group_by(
  username
) %>% dplyr::filter(
  year == cy,
  month == cm
) %>% dplyr::summarise(
  activity_curmonth = n(),
  likes_curmonth = sum(public_metrics_like_count),
  likes_mean_curmonth = mean(public_metrics_like_count),
  tweets_curmonth = sum(tweet_type == "tweet"),
  comments_curmonth = sum(tweet_type == "comment"),
  retweets_curmonth = sum(tweet_type == "retweet"),
  commentsprtweet_curmonth = round(comments_curmonth/tweets_curmonth,2),
  followers_curmonth = first(followers_count),
  impact_curmonth = round(mean(impact,na.rm = T)*100,2)
) -> dat_curmonth

## Last Month ----
dat %>% dplyr::group_by(
  username
) %>% dplyr::filter(
  year == y,
  month == lm
) %>% dplyr::summarise(
  activity_lastmonth = n(),
  likes_lastmonth = sum(public_metrics_like_count),
  likes_mean_lastmonth = mean(public_metrics_like_count),
  tweets_lastmonth = sum(tweet_type == "tweet"),
  comments_lastmonth = sum(tweet_type == "comment"),
  retweets_lastmonth = sum(tweet_type == "retweet"),
  commentsprtweet_lastmonth = round(comments_lastmonth/tweets_lastmonth,2),
  followers_lastmonth = first(followers_count),
  impact_lastmonth = round(mean(impact,na.rm = T)*100,2)
) -> dat_lastmonth

## Current Year ----
dat %>% dplyr::group_by(
  username
) %>% dplyr::filter(
  year == cy
) %>% dplyr::summarise(
  activity_curyear = n(),
  likes_curyear = sum(public_metrics_like_count),
  likes_mean_curyear = mean(public_metrics_like_count),
  tweets_curyear = sum(tweet_type == "tweet"),
  comments_curyear = sum(tweet_type == "comment"),
  retweets_curyear = sum(tweet_type == "retweet"),
  commentsprtweet_curyear = round(comments_curyear/tweets_curyear,2),
  followers_curyear = first(followers_count),
  impact_curyear = round(mean(impact,na.rm = T)*100,2)
) -> dat_curyear

## Last Year ----
dat %>% dplyr::group_by(
  username
) %>% dplyr::filter(
  year == ly
) %>% dplyr::summarise(
  activity_lastyear = n(),
  likes_lastyear = sum(public_metrics_like_count),
  likes_mean_lastyear = mean(public_metrics_like_count),
  tweets_lastyear = sum(tweet_type == "tweet"),
  comments_lastyear = sum(tweet_type == "comment"),
  retweets_lastyear = sum(tweet_type == "retweet"),
  commentsprtweet_lastyear = round(comments_lastyear/tweets_lastyear,2),
  followers_lastyear = first(followers_count),
  impact_lastyear = round(mean(impact,na.rm = T)*100,2)
) -> dat_lastyear

## Join with User Info ----

# get master data
con <- someR::con_sql()
res <- dbSendQuery(con, "SELECT * FROM twitter_users_raw")
dat_t <- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)

# get latest
dat_t %>% dplyr::filter(
  timestamp == max(timestamp)
) %>% select(
  -timestamp
) -> dat_t

# reshape
dat_t <- reshape2::dcast(
  dat_t,
  id ~ variable
)

dat_t %>% dplyr::select(
  username,
  name,
  profile_image_url,
  list
) -> dat_t

# join
dat_t <- dplyr::left_join(
  dat_t,
  dat_curday,
  c("username" = "username")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_lastday,
  c("username" = "username")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_curweek,
  c("username" = "username")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_lastweek,
  c("username" = "username")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_curmonth,
  c("username" = "username")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_lastmonth,
  c("username" = "username")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_curyear,
  c("username" = "username")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_lastyear,
  c("username" = "username")
)

# long to wide
dat_out <- reshape2::melt(
  dat_t,
  id.vars = c(
    "username", "name","profile_image_url","list"
  )
)

# Write to Scoreboard Table ----
con <- someR::con_sql()
dbSendQuery(con, "SET GLOBAL local_infile = true;")
dbWriteTable(
  con,
  "twitter_scoreboard",
  dat_out,
  overwrite = T,
  append = F,
  row.names = F
)

dbDisconnect(con)

