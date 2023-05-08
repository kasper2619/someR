get_user_timeline_xdays <- function(username = "kasper2619", xdays = 5, maxtweets = 500){

  # Load Libraries ----
  library(httr)
  library(jsonlite)
  library(dplyr)

  #i <- "1079806855"
  #username <- "kasper2619"

  # Get User Info ----
  params = list(
    `pagination_token` = NULL,
    `user.fields` = "ALL"
  )

  params$user.fields <- "id,location,name,profile_image_url,public_metrics,url,username"

  headers <- someR::twitter_bearer_token()
  user <- httr::GET(
    url = paste0("https://api.twitter.com/2/users/by/username/",username),
    httr::add_headers(.headers=headers),
    query = params
  )

  user <- httr::content(
    user, as = "text"
  )

  # convert payload
  user <- jsonlite::fromJSON(
    user,
    simplifyVector = T,
    simplifyDataFrame = T,
    flatten = T
  )

  user <- as.data.frame(user[["data"]])

  # rename
  names(user) <- gsub("public_metrics.","",names(user))

  # Get Tweets ----
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

  ## Query Data ----
  headers <- someR::twitter_bearer_token()

  userid <- user[["id"]][1]
  response <- httr::GET(
    url = paste0('https://api.twitter.com/2/users/',userid,'/tweets'),
    httr::add_headers(.headers=headers),
    query = params
  )

  # Check Rate Rimit 1
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
  if(dat[["meta"]][["result_count"]] == 0){
    next
  }

  # extract tweets
  dat_tweets <- dat[["data"]]

  ## Paginate ----

  # first we get days date
  dd <- Sys.Date()
  last_tweet_day <- as.Date(dat_tweets[["created_at"]][length(dat_tweets[["created_at"]])])
  date_diff <- as.numeric(dd - last_tweet_day)

  counter <- 1
  repeat{

    print(counter)

    # if more than X tweets then stop
    if(nrow(dat_tweets) > maxtweets){
      break
    }

    # stop if it last tweet is more than X days ago
    if(date_diff > xdays){
      break
    }

    # prepares next token
    params$pagination_token <- dat$meta$next_token

    # stops if next token is null
    if(is.null(dat$meta$next_token) == T){
      break
    }

    response <- httr::GET(
      url = paste0('https://api.twitter.com/2/users/',userid,'/tweets'),
      httr::add_headers(.headers=headers),
      query = params
    )

    ## Check Rate Rimit 2
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
  names(dat_tweets) <- gsub("public_metrics.","",names(dat_tweets))

  # remove withhel
  if("withheld_copyright" %in% names(dat_tweets)){
    dat_tweets %>% dplyr::select(
      -withheld_copyright,
      -withheld_country_codes
    ) -> dat_tweets
  }

  # Join With User Info ----
  dat_tweets <- dplyr::left_join(
    dat_tweets,
    user,
    by = c("author_id" = "id")
  )

  # clean
  dat_tweets %>% dplyr::mutate(
    referenced_tweets_type = ifelse(is.na(referenced_tweets_type) == T,"Tweet",referenced_tweets_type),
    referenced_tweets_type = ifelse(referenced_tweets_type == "NA","Tweet",referenced_tweets_type),
    referenced_tweets_type = ifelse(referenced_tweets_type == "","Tweet",referenced_tweets_type),
    referenced_tweets_type = ifelse(referenced_tweets_type == "replied_to","Reply",referenced_tweets_type),
    referenced_tweets_type = ifelse(referenced_tweets_type == "quoted","Quote",referenced_tweets_type),
    referenced_tweets_type = ifelse(referenced_tweets_type == "retweeted","Retweet",referenced_tweets_type),
    created_at = paste0(substr(created_at,1,10)," ",substr(created_at,12,19)),
    retweet_count = retweet_count + quote_count
  ) %>% dplyr::select(
    -quote_count
  ) -> dat_tweets

  rm(list=setdiff(ls(), "dat_tweets"))

  return(dat_tweets)

}

