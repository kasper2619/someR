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
res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_tl_clean")
dat <- dbFetch(res, n = -1)
dbClearResult(res)

# prep and clean
dat %>% dplyr::select(
  -timestamp,
  -friends_count,
  -mentions_user_id,
  -mentions_screen_name,
  -quoted_user_id,
  -quoted_screen_name,
  -followers_count
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
    is_quote == 1,"quote",tweet_type
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

### STATS NO LAST N TWEETS ###
#t10
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::top_n(
  10,
  created_at
) %>% dplyr::summarise(
  activity_t10 = n(),
  likes_t10 = sum(favorite_count),
  likes_mean_t10 = mean(favorite_count),
  tweets_t10 = sum(tweet_type == "tweet"),
  comments_t10 = sum(tweet_type == "comment"),
  retweets_t10 = sum(tweet_type == "retweet"),
  quotes_t10 = sum(tweet_type == "quote")
) -> dat_t10

#t50
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::top_n(
  50,
  created_at
) %>% dplyr::summarise(
  activity_t50 = n(),
  likes_t50 = sum(favorite_count),
  likes_mean_t50 = mean(favorite_count),
  tweets_t50 = sum(tweet_type == "tweet"),
  comments_t50 = sum(tweet_type == "comment"),
  retweets_t50 = sum(tweet_type == "retweet"),
  quotes_t50 = sum(tweet_type == "quote")
) -> dat_t50

#t250
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::top_n(
  250,
  created_at
) %>% dplyr::summarise(
  activity_t250 = n(),
  likes_t250 = sum(favorite_count),
  likes_mean_t250 = mean(favorite_count),
  tweets_t250 = sum(tweet_type == "tweet"),
  comments_t250 = sum(tweet_type == "comment"),
  retweets_t250 = sum(tweet_type == "retweet"),
  quotes_t250 = sum(tweet_type == "quote")
) -> dat_t250

#1000
dat %>% dplyr::group_by(
  screen_name
) %>% dplyr::top_n(
  1000,
  created_at
) %>% dplyr::summarise(
  activity_t1000 = n(),
  likes_t1000 = sum(favorite_count),
  likes_mean_t1000 = mean(favorite_count),
  tweets_t1000 = sum(tweet_type == "tweet"),
  comments_t1000 = sum(tweet_type == "comment"),
  retweets_t1000 = sum(tweet_type == "retweet"),
  quotes_t1000 = sum(tweet_type == "quote")
) -> dat_t1000

### STATS NO LAST N TWEETS GIVEN TIME ###
# current week
cw <- lubridate::week(Sys.Date())
cy <- lubridate::year(Sys.Date())
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
  quotes_curweek = sum(tweet_type == "quote")
) -> dat_curweek

# current month
cm <- lubridate::month(Sys.Date(), label = T)
cy <- lubridate::year(Sys.Date())
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
  quotes_curmonth = sum(tweet_type == "quote")
) -> dat_curmonth

# current year
cy <- lubridate::year(Sys.Date())
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
  quotes_curyear = sum(tweet_type == "quote")
) -> dat_curyear

### JOIN ###
dat_t <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/folketinget.xlsx"
)

# get twittern names
dat %>% dplyr::distinct(
  screen_name,name
) -> twitter_names

dat_t <- dplyr::left_join(
  dat_t,
  twitter_names[c("screen_name","name")],
  c("user" = "screen_name")
)

dat_t <- dplyr::left_join(
  dat_t,
  dat_t10,
  c("user" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_t50,
  c("user" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_t250,
  c("user" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_t1000,
  c("user" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_curweek,
  c("user" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_curmonth,
  c("user" = "screen_name")
)
dat_t <- dplyr::left_join(
  dat_t,
  dat_curyear,
  c("user" = "screen_name")
)

# long to wide
dat_out <- reshape2::melt(
  dat_t,
  id.vars = c(
    "user", "name", "party", "blok"
  )
)

# write to db
dbWriteTable(
  con,
  "twitter_folketing_tl_stats",
  dat_out,
  overwrite = T,
  append = F,
  row.names = F
)

dbDisconnect(con)
