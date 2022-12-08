###
library(httr)
library(jsonlite)
library(dplyr)

# get bearer token
bearer_token <- "AAAAAAAAAAAAAAAAAAAAAG3%2BVAEAAAAAtW3aCy%2FMau9cdt4tsEphm%2BporEY%3DIEDUipqxGGSoiKrqj8m2z3ZVeCxXEsfF6M2DBpjmuxTyl7MP41"

# define headers
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

# define data points to retrieve
params <- list(
  `media.fields` = 'public_metrics',
  `place.fields` = 'country'
)

#handle <- readline('$USERNAME')
#url_handle <- sprintf('https://api.twitter.com/2/users/by?usernames=%s', handle)

url_handle <- "https://api.twitter.com/2/users/2712091824/tweets?exclude=replies,retweets&tweet.fields=id,created_at,text,author_id,in_reply_to_user_id,referenced_tweets,attachments,withheld,geo,entities,public_metrics,possibly_sensitive,source,lang,context_annotations,conversation_id,reply_settings&user.fields=id,created_at,name,username,protected,verified,withheld,profile_image_url,location,url,description,entities,pinned_tweet_id,public_metrics"
url_handle <- "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=RasmusJarlov&count=200&trim_user=true"

url_handle <- "https://api.twitter.com/2/users/by/username/kasper2619?expansions=pinned_tweet_id&user.fields=id,created_at,name,username,protected,verified,withheld,profile_image_url,location,url,description,entities,pinned_tweet_id,public_metrics"

response <- httr::GET(
  url = url_handle,
  httr::add_headers(.headers = headers)
)

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

cat(final)


