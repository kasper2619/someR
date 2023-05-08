# libs
library(httr)
library(jsonlite)
library(openxlsx)
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)

# Get Users ----
# join twittertinget and lighthouselisten together

# # twittertinget
# dat_twittertinget <- openxlsx::read.xlsx(
#   "/home/kasper/someR/data/twittertinget.xlsx"
# )
# dat_twittertinget <- reshape2::melt(
#   dat_twittertinget,
#   id.vars = c("user"),
#   measure.vars = c("party","blok","list","country","affiliation")
# )
#
# # lighthouselisten
# dat_lighthouselisten <- openxlsx::read.xlsx(
#   "/home/kasper/someR/data/lighthouselisten.xlsx"
# )
# dat_lighthouselisten <- reshape2::melt(
#   dat_lighthouselisten,
#   id.vars = c("user"),
#   measure.vars = c("list","country","affiliation")
# )
#
# # bind
# dat_users <- rbind(
#   dat_lighthouselisten,
#   dat_twittertinget
# )
#
# # reshape
# dat_users <- reshape2::dcast(
#   dat_users,
#   "user ~ variable"
# )
#
# # remove NA's
# dat_users %>% dplyr::filter(
#   is.na(user) == F
# ) -> dat_users

dat_users<- openxlsx::read.xlsx(
  "/home/kasper/someR/projects/backend/data/users.xlsx"
)
dat_users %>% dplyr::distinct(
  user,
  .keep_all = T
) -> dat_users

dat_users %>% dplyr::select(
  -badge
) -> dat_users

# Start Loop ----
headers <- someR::twitter_bearer_token()

rm(out)
for(user in unique(dat_users[["user"]])){

  ## Define Query ----
  params = list(
    `pagination_token` = NULL,
    `user.fields` = "ALL"
  )

  params$user.fields <- "created_at,id,location,name,profile_image_url,public_metrics,url,username"

  # Get Data ----
  response <- httr::GET(
    url = paste0("https://api.twitter.com/2/users/by/username/",user),
    httr::add_headers(.headers=headers),
    query = params
  )

  # Check Rate Rimit ----
  # get rate limits
  rate_lim <- as.numeric(response$headers$`x-rate-limit-remaining`)
  rate_lim_reset <- response$headers$`x-rate-limit-reset`

  # check if rate limit is OK. elsewise pause
  if(rate_lim < 5){
    print("Pausing")
    pause <- as.POSIXct(as.numeric(rate_lim_reset), origin="1970-01-01") - Sys.time()
    pause <- ceiling(60*as.numeric(pause))+60
    Sys.sleep(pause)
  }

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

  dat <- as.data.frame(dat[["data"]])

  # Append Data ----
  if(exists("out") == T){
    out <- dplyr::bind_rows(dat,out)
  }
  if(exists("out") == F){
    out <- dat
  }

  print(user)

}
# Stop Loop ----

# remove dots from colnames
names(out) <- gsub("\\.","_",names(out))

# add additional information from sheet
out <- dplyr::left_join(
  out,dat_users,
  by = c("username" = "user")
)

# add extraction date
out[["timestamp"]] <- Sys.time()+60*60

# reshape
out <- reshape2::melt(
  out,
  id.vars = c(
    "id",
    "timestamp"
  )
)

# Write Raw Data to Database ----
con <- someR::con_sql()
dbSendQuery(con, "SET GLOBAL local_infile = true;")
dbWriteTable(
  con,
  "twitter_users_raw",
  out,
  overwrite = F,
  append = T,
  row.names = F
)

dbDisconnect(con)

# # make con
# con <- someR::con_sql()
# res <- dbSendQuery(con, "SELECT * FROM twitter_users_raw")
# dat <- dbFetch(res, n = -1)
# dbClearResult(res)
