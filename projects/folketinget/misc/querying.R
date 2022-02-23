# libs
library(DBI)
library(odbc)
library(dplyr)

# make con
con <- someR::con_sql()

# distinct table
tab <- data.frame(
  one = c("a","b","a","b"),
  two = c("a","a","b","b")
  #three = c(1,2,3,4)
)

# write table to db
dbWriteTable(con, "tab", tab, overwrite = T)

RMySQL::dbWriteTable(con,"tab",tab)


# list tables
dbListTables(con)

# remove indistinct
res <- dbSendQuery(con, "SELECT DISTINCT one,two FROM tab")

# execute diretly on DB
dbSendStatement(con, "SELECT DISTINCT one FROM tab")
dbSendStatement(con, "DELETE FROM tab WHERE one DISTINCT")

dbHasCompleted(rs)
dbGetRowsAffected(rs)
dbClearResult(rs)

# You can fetch all results:
res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_tl_raw")
dat <- dbFetch(res, n=-1)
dbClearResult(res)

# count
count <- as.data.frame(table(dat_dist$status_id))
count <- as.data.frame(table(dat$screen_name))

# make distinct
dat %>% dplyr::distinct(
  user_id,
  status_id,
  .keep_all = T
) -> dat_dist

dat %>% dplyr::filter(
  as.Date(timestamp) == "2022-02-17"
) -> dat_ts
