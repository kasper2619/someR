# libs
library(httr)
library(jsonlite)
library(openxlsx)
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)

# define params
user_id <- "2712091824"

n <- 3200

params = list(
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

headers <- someR::twitter_bearer_token()

params$max_results <- 100

response <- httr::GET(
  url = paste0('https://api.twitter.com/2/users/',user_id,'/tweets'),
  httr::add_headers(.headers=headers),
  query = params
)

# get data
dat <- httr::content(
  response, as = "text"
)

# check if data
dat <- jsonlite::fromJSON(
  dat[1],
  simplifyVector = T,
  simplifyDataFrame = T,
  flatten = T
)

dat <- dat$data

dat <- as.data.frame(dat)

names(dat) <- sub(".*\\.", "", names(dat))



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



