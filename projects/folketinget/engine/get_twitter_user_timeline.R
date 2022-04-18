# library
library(openxlsx)
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)
library(rtweet)

# get token
token <- someR::get_twitter_token()

# user list
dat_users <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/folketinget.xlsx"
)

# make connection do db
con <- someR::con_sql()

# time started
ts <- Sys.time() + (60*60*2)

# get data
for(i in dat_users[["user"]]){

  print(i)

  # get data
  dat_tl <- rtweet::get_timeline(
    i,
    n = 3200
  )

  # if no tweets in timeline
  if(nrow(dat_tl) == 0){
    next
  }

  # select data
  dat_tl %>% dplyr::select(
    user_id,
    screen_name,
    name,
    followers_count,
    status_id,
    created_at,
    hashtags,
    favorite_count,
    retweet_count,
    text,
    reply_to_status_id,
    reply_to_screen_name,
    mentions_user_id,
    mentions_screen_name,
    friends_count,
    quoted_user_id,
    quoted_screen_name,
    is_retweet,
    is_quote
  ) -> dat_tl

  # convert lists to strings
  class <- as.character(sapply(dat_tl, class))
  class <- names(dat_tl)[which(class == "list")]
  for(n in class){
    dat_tl[[n]] <- sapply(dat_tl[[n]], toString)
    print(n)
  }

  # convert dates to string
  dat_tl[["created_at"]] <- dat_tl[["created_at"]] + (60*60)
  dat_tl[["created_at"]] <- as.character(dat_tl[["created_at"]])

  # add timestamp
  dat_tl[["timestamp"]] <- ts

  # convert quotes and retweets
  dat_tl[["is_retweet"]] <- ifelse(dat_tl[["is_retweet"]] == T,1,0)
  dat_tl[["is_quote"]] <- ifelse(dat_tl[["is_quote"]] == T,1,0)

  # write to db
  dbSendQuery(con, "SET GLOBAL local_infile = true;")
  dbWriteTable(
    con,
    "twitter_folketing_tl_raw",
    dat_tl,
    overwrite = F,
    append = T,
    row.names = F
  )

  # look at rate limit
  rate_lim <- rtweet::rate_limit(token = token, query = "get_timeline", parse = TRUE)[["remaining"]]
  print(rate_lim)
  if(rate_lim < 100){
    print("Sleeping")
    Sys.sleep(900)
    print("Not sleeping anymore")
  }

}

# make distinct
res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_tl_raw")
dat <- dbFetch(res, n =-1)
dbClearResult(res)

dat %>% dplyr::distinct(
  user_id,
  status_id,
  .keep_all = T
) -> dat

# write to db
dbWriteTable(
  con,
  "twitter_folketing_tl_clean",
  dat,
  overwrite = T,
  append = F,
  row.names = F
)

dbDisconnect(con)
