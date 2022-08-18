#' Make Connection to SQL DB
#'
#' @return A connection to local db
#' @export
con_sql <- function(){

  library(DBI)
  library(odbc)
  library(RMySQL)

  drv <- RMySQL::MySQL()
  con <- RMySQL::dbConnect(
    drv = drv,
    username = "kasper",
    password = "Obutdob23",
    dbname = "someR_db"
  )

  return(con)
}

#' Make connection to Twitter (Deprecated)
#'
#' @return A connection to local db
#' @export
get_twitter_token <- function(){

  # libs
  library(rtweet)

  # credentials
  api_key <- "henmkG6ZyFyZBR92sLLgmwjB6"
  api_secret_key <- "DmSI6MaJealML7qL6bvfk1tuEZDdJ2uBOeijbDViOfUkvR2G6f"
  at <- "1330014024-FAKErD5Sa9Kc0r8ph7Eg9M3Xt9SBjlNLHfVQ3al"
  at_secret <- "nb8zjSNSFPedVHsa7mfzhtHlAFnyilOmnc1BFCIO2XVdW"

  ## authenticate via web browser
  token <- create_token(
    app = "TweetaLyze_001",
    consumer_key = api_key,
    consumer_secret = api_secret_key,
    access_token = at,
    access_secret = at_secret
  )

  tk <- get_token()

  return(tk)
}

#' Twitter Bearer Token
#'
#' @return A connection to local db
#' @export
twitter_bearer_token <- function(){

  # bearer token
  bearer_token <- "AAAAAAAAAAAAAAAAAAAAAG3%2BVAEAAAAAomkXdiUabO1ZMZSXNpjC1kxdCjs%3DX8x4d4eIUfENGYhm75g0p8274VEpIAxrGgKuWqvFzQnNXy3w1v"

  # define headers
  headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

  return(headers)

}
