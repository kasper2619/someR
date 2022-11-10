# libs
library(httr)
library(jsonlite)
library(openxlsx)
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)

# Get Users ----

dat_users <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/folketinget.xlsx"
)

# remove NA's
dat_users %>% dplyr::filter(
  is.na(user) == F
) -> dat_users

user_id <- "1225930531"

# Define Query ----
n <- 100

params = list(
  `pagination_token` = NULL,
  `max_results` = n,
  `tweet.fields` = "ALL",
  `user.fields` = "ALL",
  `expansions` = "ALL",
  `place.fields`= "ALL",
  `poll.fields` = "ALL"
)

params$tweet.fields <- "attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld"
params$user.fields <- "created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld"
params$expansions <- "author_id,referenced_tweets.id,referenced_tweets.id.author_id,entities.mentions.username,attachments.poll_ids,attachments.media_keys,in_reply_to_user_id,geo.place_id"
params$place.fields <- "contained_within,country,country_code,full_name,geo,id,name,place_type"
params$poll.fields <- NULL

# Get data ----
headers <- someR::twitter_bearer_token()

response <- httr::GET(
  url = paste0('https://api.twitter.com/2/users/',user_id,'/tweets'),
  httr::add_headers(.headers=headers),
  query = params
)
resp <- httr::content(
  response, as = "text"
)

# convert payload
dat <- jsonlite::fromJSON(
  resp,
  simplifyVector = T,
  simplifyDataFrame = T,
  flatten = T
)

params$pagination_token <- dat$meta$next_token
response <- httr::GET(
  url = paste0('https://api.twitter.com/2/users/',user_id,'/tweets'),
  httr::add_headers(.headers=headers),
  query = params
)



# Extract Tweets ----
dat_tweets <- dat[["data"]]

# unnest
dat_tweets %>% tidyr::unnest_wider(
  referenced_tweets,
    names_repair = tidyr::tidyr_legacy,
    names_sep = "_"
) -> dat_tweets

dat_tweets %>% tidyr::unnest_wider(
  entities.hashtags,
  names_repair = tidyr::tidyr_legacy,
  names_sep = "_"
) -> dat_tweets
dat_tweets %>% tidyr::hoist(
  entities.hashtags_tag
) -> dat_tweets

dat_tweets %>% tidyr::unnest_wider(
  entities.mentions,
  names_repair = tidyr::tidyr_legacy,
  names_sep = "_"
) -> dat_tweets
dat_tweets %>% tidyr::hoist(
  entities.mentions_username
) -> dat_tweets

# select
dat_tweets %>% dplyr::select(
  id,
  text,
  author_id,
  created_at,
  in_reply_to_user_id,
  referenced_tweets_id,
  referenced_tweets_type,
  entities.hashtags_tag,
  entities.mentions_username,
  public_metrics.like_count,
  public_metrics.reply_count,
  public_metrics.retweet_count,
  public_metrics.quote_count
) -> dat_tweets

class <- as.character(sapply(dat_tweets, class))
class <- names(dat_tweets)[which(class == "list")]
for(n in class){
  dat_tweets[[n]] <- sapply(dat_tweets[[n]], toString)
  print(n)
}

# rename
dat_tweets %>% dplyr::rename(
  "tweet_id" = id,
  "user_id" = author_id,
  "tweet_created_at" = created_at,
  "like_count" = public_metrics.like_count,
  "reply_count" = public_metrics.reply_count,
  "retweet_count" = public_metrics.retweet_count,
  "quote_count" = public_metrics.quote_count,
  "hashtags" = entities.hashtags_tag,
  "mentions_username" = entities.mentions_username
) -> dat_tweets

# Extract User Info ----
dat_user <- dat[["includes"]][["users"]]

dat_user %>% dplyr::select(
  id,
  username,
  name,
  created_at,
  description,
  location,
  profile_image_url,
  url,
  public_metrics.following_count,
  public_metrics.tweet_count,
  public_metrics.listed_count,
  public_metrics.followers_count
) -> dat_user

dat_user %>% dplyr::rename(
  "user_id" = id,
  "following_count" = public_metrics.following_count,
  "tweet_count" = public_metrics.tweet_count,
  "listed_count" = public_metrics.listed_count,
  "followers_count" = public_metrics.followers_count,
  "user_created_at" = created_at
) -> dat_user

# Join Tweets & User Data ----
dat_out <- dplyr::left_join(
  dat_tweets,
  dat_user,
  by = c(
    "user_id" = "user_id"
  )
)








con <- someR::con_sql()

# get data
res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_tl_clean LIMIT 1000")
qqq <- dbFetch(res, n = -1)
dbClearResult(res)







# check if data
out <- jsonlite::fromJSON(
  dat[1][,
  simplifyVector = T,
  simplifyDataFrame = T,
  flatten = T
)



# check if data
out <- jsonlite::fromJSON(
  dat[2],
  simplifyVector = T,
  simplifyDataFrame = T,
  flatten = T
)





json_data <- fromJSON(dat, flatten = TRUE) %>% as.data.frame
View(json_data)

final <-
  sprintf(
    "Handle: %s\nBio: %s\nPinned Tweet: %s",
    dat$data.username,
    dat$data.description,
    dat$includes.tweets.text
  )

names(dat) <- sub(".*\\.", "", names(dat))







handle <- readline('$USERNAME')

url_handle <-
  sprintf('https://api.twitter.com/2/users/by?usernames=%s', handle)

response <-
  httr::GET(url = url_handle,
            httr::add_headers(.headers = headers),
            query = params)
obj <- httr::content(response, as = "text")
print(obj)

json_data <- fromJSON(obj, flatten = TRUE) %>% as.data.frame
View(json_data)

final <-
  sprintf(
    "Handle: %s\nBio: %s\nPinned Tweet: %s",
    json_data$data.username,
    json_data$data.description,
    json_data$includes.tweets.text
  )





# URLS
#url_handle <- "https://api.twitter.com/2/users/2712091824/tweets?exclude=replies,retweets&tweet.fields=id,created_at,text,author_id,in_reply_to_user_id,referenced_tweets,attachments,withheld,geo,entities,public_metrics,possibly_sensitive,source,lang,context_annotations,conversation_id,reply_settings&user.fields=id,created_at,name,username,protected,verified,withheld,profile_image_url,location,url,description,entities,pinned_tweet_id,public_metrics"
#url_handle <- "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=RasmusJarlov&count=200&trim_user=true"

# get headers
headers <- someR::twitter_bearer_token()







i <- 2712091824

# define url
since_id <- ""
user_id <- "1225930531"

if(since_id == ""){
  url_handle <- paste0(
    "https://api.twitter.com/2/users/",user_id,"/tweets?max_results=100&start_time=2015-01-01T00:00:00.000Z&tweet.fields=attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld"
  )
}
if(since_id != ""){
  url_handle <- paste0(
    "https://api.twitter.com/2/users/",user_id,"/tweets?since_id=",since_id,"&max_results=100&start_time=2015-01-01T00:00:00.000Z&tweet.fields=attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld"
  )
}

# https://developer.twitter.com/apitools/api?endpoint=%2F2%2Fusers%2F%7Bid%7D%2Ftweets&method=get

# get resonse
response <- httr::GET(
  url = url_handle,
  httr::add_headers(.headers = headers)
)
response

# get data
dat <- httr::content(
  response, as = "text"
)

# check if data
dat <- jsonlite::fromJSON(
  dat,
  simplifyVector = T,
  flatten = TRUE
) %>% as.data.frame
names(dat) <- sub(".*\\.", "", names(dat))

library(RTwitterV2)




# get headers
headers <- someR::twitter_bearer_token()

# get list of users
dat_users <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/folketinget.xlsx"
)

# remove NA's
dat_users %>% dplyr::filter(
  is.na(user) == F
) -> dat_users

# loop to get user info
rm(out)
rate_lim <- 1000
for(i in dat_users[["user"]]){

  # define url
  url_handle <- paste0(
    "https://api.twitter.com/2/users/by/username/",i,"?user.fields=id,created_at,name,username,protected,verified,withheld,profile_image_url,location,url,description,public_metrics"
  )

  # check if rate limnit is OK. elsewise pause
  if(rate_lim < 5){
    print("Pausing")
    pause <- as.POSIXct(as.numeric(rate_lim_reset), origin="1970-01-01") - Sys.time()
    pause <- ceiling(60*as.numeric(pause))+60
    Sys.sleep(pause)
  }

  # get resonse
  response <- httr::GET(
    url = url_handle,
    httr::add_headers(.headers = headers)
  )

  # get rate limits
  rate_lim <- as.numeric(response$headers$`x-rate-limit-remaining`)
  rate_lim_reset <- response$headers$`x-rate-limit-reset`

  # get data
  dat <- httr::content(
    response, as = "text"
  )

  # check if data
  dat <- jsonlite::fromJSON(
    dat,
    simplifyVector = F,
    flatten = TRUE
  ) %>% as.data.frame
  names(dat) <- sub(".*\\.", "", names(dat))

  # check for error
  if(is.null(dat[["title"]]) == F){
    next
  }

  # add timestamp
  dat[["timestamp"]] <- Sys.time()+(60*60*2)

  # add extra information from user list
  dat[["party"]] <- dat_users[which(dat_users$user==i),]$party
  dat[["blok"]] <- dat_users[which(dat_users$user==i),]$blok

  # reshape
  dat <- reshape2::melt(
    dat,
    id.vars=c("id","username","timestamp")
  )

  if(exists("out") == T){
    out <- rbind(out,dat)
  }
  if(exists("out") == F){
    out <- dat
  }

  print(i)
  print(rate_lim)

}

# make connection do db
con <- someR::con_sql()

# write to db
dbSendQuery(con, "SET GLOBAL local_infile = true;")
dbWriteTable(
  con,
  "twitter_folketing_master_new",
  out,
  overwrite = F,
  append = T,
  row.names = F
)

dbDisconnect(con)



