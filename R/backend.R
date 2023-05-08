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
  library(dplyr)

  # credentials
  # fast4ward
  # api_key <- "Dnbyy8xPUa87il0Wzbtxj7A0h"
  # api_secret_key <- "mfl3I1LY0hwZcQKY1MDxgOJexuqjUupVwGDXQU2htsSxozAd4W"
  # at <- "1515745785782095873-gJtSvTaXKirNUmGqtCj9VbowOw3zgO"
  # at_secret <- "Z8A286Uhel356OB4d6VSLcGHSJ3cJtX5b6o8fe356c0jv"

  # kasper2619
  api_key <- "sAWbvZZOPlwVGkmIoBx2zOCMG"
  api_secret_key <- "wg2QfKEyTmlfKvIzc2nqQ86eWF8qhnSrddLIM8YIE3x8Wxji8R"
  at <- "1330014024-ZoSQUyGq7UKYABwSUB5e1ql00JlGTV0cnpo3Q0i"
  at_secret <- "hehFtak0VGwWJkcyDxHMlOB6SEh2z0chKar693k9nMJva"

  ## authenticate via web browser
  token <- create_token(
    app = "TweetaLyze_003",
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
  # kasper2619
  bearer_token <- "AAAAAAAAAAAAAAAAAAAAAN8hnAEAAAAAHdj97It27ooI20CeoRa3xf4eZug%3DdAHjUVDAa1wKOqMWDx7r7383IwKVFAOnXIaPfcRfWPrLBQqdxO"

  # fast4ward
  #bearer_token <- "AAAAAAAAAAAAAAAAAAAAAGC6kAEAAAAAQ96yCpDWYDDkrEKm2%2BId1HKdFV8%3Dl4dfDjmFOC8UYQ7OBZDiWIrbfSTlj9MlYF9OGvWYngaCGBF1IV"

  # define headers
  headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

  return(headers)

}
