# libs
library(dplyr)
library(reactable)
library(reactablefmtr)
library(someR)

# get data
con <- someR::con_sql()
res <- dbSendQuery(con, "SELECT * FROM twitter_folketing_tl_stats")
dat <- dbFetch(res, n = -1)
dbClearResult(res)

# need to join this in for later
dat %>% dplyr::select(
  user,name,party
) %>% distinct(
  user,
  .keep_all = T
) -> dat_master

# make table
dat %>% dplyr::filter(
  variable %in% c(
    "activity_curweek",
    "tweets_curweek",
    "comments_curweek",
    "likes_curweek",
    "likes_mean_curweek",
    "activity_curmonth",
    "tweets_curmonth",
    "comments_curmonth",
    "likes_curmonth",
    "likes_mean_curmonth",
    "activity_curyear",
    "tweets_curyear",
    "comments_curyear",
    "likes_curyear",
    "likes_mean_curyear"
  )
) -> table_currentx

table_currentx <- reshape2::dcast(
  table_currentx,
  "user ~ variable",
  value.var = "value",
  fun.aggregate = sum
)

### CURRENT WEEK ###
table_currentx %>% dplyr::select(
  user,
  activity_curweek,
  tweets_curweek,
  comments_curweek,
  likes_curweek,
  likes_mean_curweek
) %>% dplyr::arrange(
  desc(likes_curweek)
) -> table_curweek
table_curweek[["likes_mean_curweek"]] <- round(table_curweek[["likes_mean_curweek"]],2)
table_curweek[["placering"]] <- 1:nrow(table_curweek)

table_curweek <- dplyr::left_join(
  table_curweek,dat_master, by = c("user"="user")
)
table_curweek %>% dplyr::select(
  name,
  party,
  placering,
  likes_curweek,
  likes_mean_curweek,
  activity_curweek,
  tweets_curweek,
  comments_curweek
) -> table_curweek


### GOES INTO APP ###
# define places
# first <- table_curweek[["name"]][1]
# second <- table_curweek[["name"]][2]
# third <- table_curweek[["name"]][3]
#
# # vizualise
# reactable(
#   theme = nytimes(),
#   table_curweek,
#   resizable = TRUE,
#   sortable = TRUE,
#   filterable = TRUE,
#   searchable = TRUE,
#   pagination = TRUE,
#   fullWidth = TRUE,
#   defaultColDef = colDef(
#     #header = function(value) gsub("_", "_", value, fixed = TRUE),
#     #cell = function(value) format(value, nsmall = 1),
#     align = "center",
#
#     minWidth = 120
#   ),
#   columns = list(
#     name = colDef(
#       name = "Politiker",
#       align = "left",
#       minWidth = 180
#     ),
#     party = colDef(
#       name = "Parti",
#       align = "center",
#       minWidth = 180
#     ),
#     placering = colDef(
#       name = "Placering",
#       align = "center",
#       minWidth = 180
#     ),
#     likes_curweek = colDef(
#       name = "Likes I Alt",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curweek, font_color = "black", highlighter = "#FFD700")
#     ),
#     likes_mean_curweek = colDef(
#       name = "Likes Gns.",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curweek, font_color = "black", highlighter = "#FFD700")
#     ),
#     activity_curweek = colDef(
#       name = "Aktivitet",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curweek, font_color = "black", highlighter = "#FFD700")
#     ),
#     tweets_curweek = colDef(
#       name = "Tweets",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curweek, font_color = "black", highlighter = "#FFD700")
#     ),
#     comments_curweek = colDef(
#       name = "Kommentarer",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curweek, font_color = "black", highlighter = "#FFD700")
#     )
#   )
# )
### GOES INTO APP ###

### CURRENT MONTH ###
table_currentx %>% dplyr::select(
  user,
  activity_curmonth,
  tweets_curmonth,
  comments_curmonth,
  likes_curmonth,
  likes_mean_curmonth
) %>% dplyr::arrange(
  desc(likes_curmonth)
) -> table_curmonth
table_curmonth[["likes_mean_curmonth"]] <- round(table_curmonth[["likes_mean_curmonth"]],2)
table_curmonth[["placering"]] <- 1:nrow(table_curmonth)

table_curmonth <- dplyr::left_join(
  table_curmonth,dat_master, by = c("user"="user")
)
table_curmonth %>% dplyr::select(
  name,
  party,
  placering,
  likes_curmonth,
  likes_mean_curmonth,
  activity_curmonth,
  tweets_curmonth,
  comments_curmonth
) -> table_curmonth

### GOES INTO APP ###
# # define places
# first <- table_curmonth[["name"]][1]
# second <- table_curmonth[["name"]][2]
# third <- table_curmonth[["name"]][3]
#
# # vizualise
# reactable(
#   theme = nytimes(),
#   table_curmonth,
#   resizable = TRUE,
#   sortable = TRUE,
#   filterable = TRUE,
#   searchable = TRUE,
#   pagination = TRUE,
#   fullWidth = TRUE,
#   defaultColDef = colDef(
#     #header = function(value) gsub("_", "_", value, fixed = TRUE),
#     #cell = function(value) format(value, nsmall = 1),
#     align = "center",
#
#     minWidth = 120
#   ),
#   columns = list(
#     name = colDef(
#       name = "Politiker",
#       align = "left",
#       minWidth = 180
#     ),
#     party = colDef(
#       name = "Parti",
#       align = "center",
#       minWidth = 180
#     ),
#     placering = colDef(
#       name = "Placering",
#       align = "center",
#       minWidth = 180
#     ),
#     activity_curmonth = colDef(
#       name = "Aktivitet",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curmonth, font_color = "black", highlighter = "#FFD700")
#     ),
#     tweets_curmonth = colDef(
#       name = "Tweets",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curmonth, font_color = "black", highlighter = "#FFD700")
#     ),
#     comments_curmonth = colDef(
#       name = "Kommentarer",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curmonth, font_color = "black", highlighter = "#FFD700")
#     ),
#     likes_curmonth = colDef(
#       name = "Likes I Alt",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curmonth, font_color = "black", highlighter = "#FFD700")
#     ),
#     likes_mean_curmonth = colDef(
#       name = "Likes Gns.",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curmonth, font_color = "black", highlighter = "#FFD700")
#     )
#   )
# )
### GOES INTO APP ###

### CURRENT YEAR ###
table_currentx %>% dplyr::select(
  user,
  activity_curyear,
  tweets_curyear,
  comments_curyear,
  likes_curyear,
  likes_mean_curyear
) %>% dplyr::arrange(
  desc(likes_curyear)
) -> table_curyear
table_curyear[["likes_mean_curyear"]] <- round(table_curyear[["likes_mean_curyear"]],2)
table_curyear[["placering"]] <- 1:nrow(table_curyear)

table_curyear <- dplyr::left_join(
  table_curyear,dat_master, by = c("user"="user")
)
table_curyear %>% dplyr::select(
  name,
  party,
  placering,
  likes_curyear,
  likes_mean_curyear,
  activity_curyear,
  tweets_curyear,
  comments_curyear
) -> table_curyear

### GOES INTO APP ###
# define places
# first <- table_curyear[["name"]][1]
# second <- table_curyear[["name"]][2]
# third <- table_curyear[["name"]][3]
#
# # vizualise
# reactable(
#   theme = nytimes(),
#   table_curyear,
#   resizable = TRUE,
#   sortable = TRUE,
#   filterable = TRUE,
#   searchable = TRUE,
#   pagination = TRUE,
#   fullWidth = TRUE,
#   defaultColDef = colDef(
#     #header = function(value) gsub("_", "_", value, fixed = TRUE),
#     #cell = function(value) format(value, nsmall = 1),
#     align = "center",
#
#     minWidth = 120
#   ),
#   columns = list(
#     name = colDef(
#       name = "Politiker",
#       align = "left",
#       minWidth = 180
#     ),
#     party = colDef(
#       name = "Parti",
#       align = "center",
#       minWidth = 180
#     ),
#     placering = colDef(
#       name = "Placering",
#       align = "center",
#       minWidth = 180
#     ),
#     likes_curyear = colDef(
#       name = "Likes I Alt",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curyear, font_color = "black", highlighter = "#FFD700")
#     ),
#     likes_mean_curyear = colDef(
#       name = "Likes Gns.",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curyear, font_color = "black", highlighter = "#FFD700")
#     ),
#     activity_curyear = colDef(
#       name = "Aktivitet",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curyear, font_color = "black", highlighter = "#FFD700")
#     ),
#     tweets_curyear = colDef(
#       name = "Tweets",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curyear, font_color = "black", highlighter = "#FFD700")
#     ),
#     comments_curyear = colDef(
#       name = "Kommentarer",
#       align = "center",
#       minWidth = 120,
#       style = highlight_max(table_curyear, font_color = "black", highlighter = "#FFD700")
#     )
#   )
# )
### GOES INTO APP ###
