# library
library(openxlsx)
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)
library(rtweet)

# get token
token <- someR::get_twitter_token()

# user list
dat_users <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/folketinget.xlsx"
)

# remove NA's
dat_users %>% dplyr::filter(
  is.na(user) == F
) -> dat_users

# make connection do db
con <- someR::con_sql()

# time started
ts <- Sys.time() + (60*60*2)

dat_user_info <- rtweet::lookup_users(
  dat_users[["user"]]
)

dat_user_info %>% dplyr::select(
  user_id,
  screen_name,
  name,
  followers_count,
  friends_count,
  profile_image_url
) -> dat_user_info
dat_user_info[["timestamp"]] <- Sys.time()+(60*60*2)

# merge with original sheet data
dat_user_info <- dplyr::inner_join(
  dat_user_info,
  dat_users,
  by = c("screen_name" = "user")
)

# reshape
dat_user_info <- reshape2::melt(
  dat_user_info,
  c("user_id","timestamp")
)

# write to db
dbSendQuery(con, "SET GLOBAL local_infile = true;")
dbWriteTable(
  con,
  "twitter_folketing_master",
  dat_user_info,
  overwrite = F,
  append = T,
  row.names = F
)

dbDisconnect(con)
