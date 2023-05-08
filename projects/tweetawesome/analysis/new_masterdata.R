# make connection do db
con <- someR::con_sql()

# get data
res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_master_new")
dat <- dbFetch(res, n = -1)
dbClearResult(res)


dbWriteTable(
  con,
  "twitter_folketing_master_new",
  out,
  overwrite = F,
  append = T,
  row.names = F
)
