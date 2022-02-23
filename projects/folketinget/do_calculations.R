# libs
library(DBI)
library(odbc)
library(dplyr)
library(lubridate)

######################
### LOAD AND CLEAN ###
######################
# make con
con <- someR::con_sql()

# get data
res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_tl_raw")
dat <- dbFetch(res)
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

# date conversion
dat[["created_at"]] <- as.POSIXct(dat[["created_at"]]) + 60*60

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
  wday = lubridate::wday(created_at, label = T),
  hour = lubridate::hour(created_at)
) -> dat

dat %>% dplyr::group_by(
  screen_name,wday
) %>% dplyr::summarise(
  mean = mean(favorite_count)
) -> qqq


######################

#######################
### CALCULATE STATS ###
#######################

### POLITICIAN STATS ###
dat %>% dplyr::group_by(
  user_id, screen_name, name
) %>% dplyr::summarise(
 tweets = count(tweet_type == "tweet")
) -> q





# convert to timerseries
