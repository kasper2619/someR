# twittertinget
dat_twittertinget <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/twittertinget.xlsx"
)
dat_twittertinget <- reshape2::melt(
  dat_twittertinget,
  id.vars = c("user"),
  measure.vars = c("party","blok","list","country","affiliation")
)

# lighthouselisten
dat_lighthouselisten <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/lighthouselisten.xlsx"
)
dat_lighthouselisten <- reshape2::melt(
  dat_lighthouselisten,
  id.vars = c("user"),
  measure.vars = c("list","country","affiliation")
)

# bind
dat_users <- rbind(
  dat_lighthouselisten,
  dat_twittertinget
)

# reshape
dat_users <- reshape2::dcast(
  dat_users,
  "user ~ variable"
)

# remove NA's
dat_users %>% dplyr::filter(
  is.na(user) == F
) -> dat_users

# write to db
