library(rtweet)

auth <- someR::get_twitter_token()

user <- rtweet::lookup_users("kasper2619", token = auth)

dat <- rtweet::get_timeline("kasper2619", token = auth)
