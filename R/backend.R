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

#' Make Connection to SQL DB
#'
#' @return A connection to local db
#' @export
get_twitter_token <- function(){

  # libs
  library(rtweet )

  # credentials
  api_key <- "FtRtGtENd6Zxc9SDbXbG6z7wp"
  api_secret_key <- "OirXTuYuAsCXAbtzYNJ1NZG6KW8jdVek6iebaMqH9fZgsiOLuT"
  at <- "1330014024-v3ArKU7ANGVTQjD1zSiZXkUcsuPydYITIRpYO9A"
  at_secret <- "qBJBdROsGjz5LDAtv8CTrTYg4kCO5VfcAXS3eDPLxo9Po"

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
