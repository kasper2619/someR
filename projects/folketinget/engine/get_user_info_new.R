# libs
library(httr)
library(jsonlite)
library(openxlsx)
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)

# URLS
#url_handle <- "https://api.twitter.com/2/users/2712091824/tweets?exclude=replies,retweets&tweet.fields=id,created_at,text,author_id,in_reply_to_user_id,referenced_tweets,attachments,withheld,geo,entities,public_metrics,possibly_sensitive,source,lang,context_annotations,conversation_id,reply_settings&user.fields=id,created_at,name,username,protected,verified,withheld,profile_image_url,location,url,description,entities,pinned_tweet_id,public_metrics"
#url_handle <- "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=RasmusJarlov&count=200&trim_user=true"

# get headers
headers <- someR::twitter_bearer_token()

# get list of users
dat_users <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/folketinget.xlsx"
)

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



