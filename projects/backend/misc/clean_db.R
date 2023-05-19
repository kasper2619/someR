# Libraries ----
library(DBI)
library(odbc)
library(RMySQL)
library(dplyr)
library(reshape2)
library(lubridate)

## Clean Users ----
con <- someR::con_sql()
res <- dbSendQuery(
  con,
  "SELECT * FROM twitter_users_raw"
)
dat_users <- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)

dat_users %>% dplyr::filter(
  id != "1346870269311381508"
) -> dat_users

con <- someR::con_sql()
dbSendQuery(con, "SET GLOBAL local_infile = true;")
dbWriteTable(
  con,
  "twitter_users_raw",
  dat_users,
  overwrite = T,
  append = F,
  row.names = F
)

dbDisconnect(con)
