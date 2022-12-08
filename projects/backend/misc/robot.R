# libs
library(httr)
library(jsonlite)
library(openxlsx)
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)

# Start Loop ----
headers <- someR::twitter_bearer_token()

# get token
token <- someR::get_twitter_token()

# get data
dat_tl <- rtweet::get_timeline(
  "kasper2619",
  n = 3200
)


?rtweet::flatten

rtweet::tweet_shot("1600584610664288256")


qqq <- rtweet::lookup_tweets(
  dat_tl$status_id
)

ta <- trends <- rtweet::trends_available()


trends <- rtweet::get_trends("23424796")



search <- rtweet::search_tweets2("kasper2619",n = 2000, lang = "da")
