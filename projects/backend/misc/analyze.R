# Get Data ----

## Get Tweets ----
con <- someR::con_sql()
res <- dbSendQuery(con, "SELECT * FROM twitter_tweets_raw")
dat_tweets <- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)

# filter latest
dat_tweets %>% dplyr::group_by(
  author_id,
  id
) %>% dplyr::filter(
  timestamp == max(timestamp)
) -> dat_tweets

# reshape
dat_tweets <- reshape2::dcast(
  dat_tweets,
  formula = "id + author_id ~ variable",
)

## Get Users ----
con <- someR::con_sql()
res <- dbSendQuery(con, "SELECT * FROM twitter_users_raw")
dat_users <- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)

# filter latest
dat_users %>% dplyr::group_by(
  id
) %>% dplyr::filter(
  timestamp == max(timestamp)
) -> dat_users

# reshape
dat_users <- reshape2::dcast(
  dat_users,
  formula = "id ~ variable",
  value.var = "value"
)

# Join Tweets and Users ----
dat_tweets <- dplyr::left_join(
  dat_tweets,dat_users,
  by = c("author_id" = "id")
)

# sentiment analysis
devtools::install_github("Guscode/Sentida")

library(Sentida)
dat_tweets[["sentiment"]] <- sapply(dat_tweets$text, output = "mean", FUN = Sentida::sentida)
class(dat_tweets$sentiment)

dat_tweets %>% dplyr::group_by(
  list
) %>% dplyr::summarise(
  sentiment = mean(sentiment, na.rm = T),
  n = n()
) -> qqq


Sentida::sentida("Det er helt vildt, at analytiker hos TV2 kan blive forarget over en afstemning på Twitter.
Der blæser nye vinde i medieverdenen. Main stream medier og forsmåede journalisters indflydelse bliver mindre med lynets hast. Brugerne har talt om man kan lide det eller ej.", output = "mean")

Sentida::sentida("Tja. Det er din sag", output = "mean")

Sentida::sentida("Jeg har ingen udfordringer som mand. Så læg bare denne mændenes kampdag til de andre 364 dage, hvor kvinder hele tiden sætter fokus på dem selv og deres problemer som skyldes mænd.")


ds

dat[["sentiment"]] <- sapply(dat$text,apply(x,1,product  ))


Sentida::sentida(dat$text, output = "mean")


Sentida::sentida(dat$text[1:100])

dat$text
ggplot(dat, aes(x = sentiment_mean)) +
  geom_histogram(fill = "lightblue", colour = "black") +
  facet_grid(referenced_tweets_type ~ .)

