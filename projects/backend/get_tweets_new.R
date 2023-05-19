# Load Libraries ----
library(httr)
library(jsonlite)
library(openxlsx)
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)

# Get Users ----
con <- someR::con_sql()
res <- dbSendQuery(
  con,
  "SELECT DISTINCT(id)
    FROM twitter_users_raw
    WHERE timestamp=(
      SELECT MAX(timestamp) from twitter_users_raw
    )"
)
dat_users <- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)

# remove NA's
# dat_users %>% dplyr::filter(
#   is.na(user) == F
# ) -> dat_users

i <- "1513224926391676931"

# Get Data ----
for(i in dat_users[["id"]]){

  print(i)

  ## Define Query ----
  params = list(
    `pagination_token` = NULL,
    `max_results` = 10,
    `tweet.fields` = "ALL",
    `user.fields` = "ALL",
    `expansions` = "ALL",
    `place.fields`= "ALL",
    `poll.fields` = "ALL"
  )

  params$tweet.fields <- "author_id,conversation_id,created_at,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text"
  params$user.fields <- "created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username"
  params$expansions <- "author_id,referenced_tweets.id,referenced_tweets.id.author_id,in_reply_to_user_id"
  params$place.fields <- "contained_within,country,country_code,full_name,id,name,place_type"
  params$poll.fields <- NULL

  # Query Data ----
  headers <- someR::twitter_bearer_token()

  response <- httr::GET(
    url = paste0('https://api.twitter.com/2/users/',i,'/tweets'),
    httr::add_headers(.headers=headers),
    query = params
  )

    # Check Rate Rimit 1 ----
    # get rate limits
    rate_lim <- as.numeric(response$headers$`x-rate-limit-remaining`)
    rate_lim_reset <- response$headers$`x-rate-limit-reset`

    # check if rate limit is OK. elsewise pause
    print(rate_lim)
    if(rate_lim < 750){
      print("Pausing")
      pause <- as.POSIXct(as.numeric(rate_lim_reset), origin="1970-01-01") - Sys.time()
      pause <- ceiling(60*as.numeric(pause))+60
      Sys.sleep(pause)
    }
    if(rate_lim < 5){
      print("Pausing")
      pause <- as.POSIXct(as.numeric(rate_lim_reset), origin="1970-01-01") - Sys.time()
      pause <- ceiling(60*as.numeric(pause))+60
      Sys.sleep(pause)
    }

  resp <- httr::content(
    response, as = "text"
  )
  dat <- jsonlite::fromJSON(
    resp,
    simplifyVector = T,
    simplifyDataFrame = T,
    flatten = T
  )
  if(is.null(dat[["meta"]][["result_count"]]) == T){
    next
  }
  if(dat[["meta"]][["result_count"]] == 0){
    next
  }

  # extract tweets
  dat_tweets <- dat[["data"]]

  # Paginate ----

  # first we get days date
  dd <- Sys.Date()
  last_tweet_day <- as.Date(dat_tweets[["created_at"]][length(dat_tweets[["created_at"]])])
  date_diff <- as.numeric(dd - last_tweet_day)

  counter <- 1
  repeat{

    print(counter)

    # stop if it last tweet is more than X days ago
    if(date_diff > 10){
      break
    }

    params$pagination_token <- dat$meta$next_token

    response <- httr::GET(
      url = paste0('https://api.twitter.com/2/users/',i,'/tweets'),
      httr::add_headers(.headers=headers),
      query = params
    )

      ## Check Rate Rimit 2 ----
      # get rate limits
      rate_lim <- as.numeric(response$headers$`x-rate-limit-remaining`)
      rate_lim_reset <- response$headers$`x-rate-limit-reset`

      # check if rate limit is OK. elsewise pause
      print(rate_lim)
      if(rate_lim < 5){
        print("Pausing")
        pause <- as.POSIXct(as.numeric(rate_lim_reset), origin="1970-01-01") - Sys.time()
        pause <- ceiling(60*as.numeric(pause))+60
        Sys.sleep(pause)
      }

    resp <- httr::content(
      response, as = "text"
    )
    dat <- jsonlite::fromJSON(
      resp,
      simplifyVector = T,
      simplifyDataFrame = T,
      flatten = T
    )

    dat_tweets_paginate <- dat[["data"]]

    ## Bind ----
    dat_tweets <- dplyr::bind_rows(
      dat_tweets, dat_tweets_paginate
    )

    counter <- counter + 1

    last_tweet_day <- as.Date(dat_tweets[["created_at"]][length(dat_tweets[["created_at"]])])
    date_diff <- as.numeric(dd - last_tweet_day)

  }

  # remove edit_history_tweets_id
  dat_tweets %>% dplyr::select(
    -edit_history_tweet_ids
  ) -> dat_tweets

  # unnest
  if("referenced_tweets" %in% names(dat_tweets)){
    dat_tweets %>% tidyr::unnest_wider(
      referenced_tweets,
        names_repair = tidyr::tidyr_legacy,
        names_sep = "_"
    ) -> dat_tweets

    # make list to characters
    class <- c("referenced_tweets_type","referenced_tweets_id")
    for(n in class){
      dat_tweets[[n]] <- sapply(dat_tweets[[n]], toString)
    }

  }

  # make timestamp
  dat_tweets[["timestamp"]] <- Sys.time()+60*60

  # remove dots from colnames
  names(dat_tweets) <- gsub("\\.","_",names(dat_tweets))

  # remove withhel
  if("withheld_copyright" %in% names(dat_tweets)){
    dat_tweets %>% dplyr::select(
      -withheld_copyright,
      -withheld_country_codes
    ) -> dat_tweets
  }

  # reshape
  dat_tweets <- reshape2::melt(
    dat_tweets,
    id.vars = c(
      "author_id",
      "id",
      "timestamp"
    )
  )

  # Write to Database ----
  con <- someR::con_sql()
  dbSendQuery(con, "SET GLOBAL local_infile = true;")
  dbWriteTable(
    con,
    "twitter_tweets_raw",
    dat_tweets,
    overwrite = F,
    append = T,
    row.names = F
  )
  dbDisconnect(con)

}
# Get Data Done ----

# Filter Latest ----
con <- someR::con_sql()
res <- dbSendQuery(con, "SELECT * FROM twitter_tweets_raw")
dat <- dbFetch(res, n =-1)
dbClearResult(res)
dbDisconnect(con)

dat %>% dplyr::group_by(
  author_id,
  id
) %>% dplyr::filter(
  timestamp == max(timestamp)
) -> dat

# Remove Deleted Users ----
dat <- dat[which(dat[["author_id"]] %in% dat_users[["id"]] == T),]

# Write Distinct to Database ----
con <- someR::con_sql()
dbSendQuery(con, "SET GLOBAL local_infile = true;")
dbWriteTable(
  con,
  "twitter_tweets_raw",
  dat,
  overwrite = T,
  append = F,
  row.names = F
)
dbDisconnect(con)

# See Data ----
# con <- someR::con_sql()
# res <- dbSendQuery(con, "SELECT * FROM twitter_tweets_raw")
# dat <- dbFetch(res, n = -1)
# dbClearResult(res)
# dbDisconnect(con)
#
# # reshape
# dat <- reshape2::dcast(
#   dat,
#   formula = "id ~ variable"
# )
