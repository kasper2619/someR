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
  "name ~ variable",
  value.var = "value",
  fun.aggregate = sum
)

### CURRENT WEEK ###
table_currentx %>% dplyr::select(
  name,
  activity_curweek,
  tweets_curweek,
  comments_curweek,
  likes_curweek,
  likes_mean_curweek
) %>% dplyr::arrange(
  desc(likes_curweek)
) -> table_curweek
table_curweek[["likes_mean_curweek"]] <- round(table_curweek[["likes_mean_curweek"]],0)

# define places
first <- table_curweek[["name"]][1]
second <- table_curweek[["name"]][2]
third <- table_curweek[["name"]][3]

# vizualise
reactable(
  theme = nytimes(),
  table_curweek,
  resizable = TRUE,
  sortable = TRUE,
  filterable = TRUE,
  searchable = TRUE,
  pagination = TRUE,
  fullWidth = TRUE,
  defaultColDef = colDef(
    #header = function(value) gsub("_", "_", value, fixed = TRUE),
    #cell = function(value) format(value, nsmall = 1),
    align = "center",

    minWidth = 120
  ),
  columns = list(
    name = colDef(
      name = "Politiker",
      align = "left",
      minWidth = 180
    ),
    activity_curweek = colDef(
      name = "Aktivitet",
      align = "center",
      minWidth = 120
    ),
    tweets_curweek = colDef(
      name = "Tweets",
      align = "center",
      minWidth = 120
    ),
    comments_curweek = colDef(
      name = "Kommentarer",
      align = "center",
      minWidth = 120
    ),
    likes_curweek = colDef(
      name = "Likes I Alt",
      align = "center",
      minWidth = 120,
      style = highlight_max(table_curweek, font_color = "black", highlighter = "#FFD700")
    ),
    likes_mean_curweek = colDef(
      name = "Likes Gns.",
      align = "center",
      minWidth = 120
    )
  )
)





    Mean_Post_Likes_pr_Follower = colDef(
      name = "MPLFollow",
      style = function(value) {
        if (value == first) {
          color <- "#FFD700"
        } else if (value == second) {
          color <- "#C0C0C0"
        } else if (value == third) {
          color <- "#C9AE5D"
        } else {
          color <- "#777"
        }
        list(color = color, fontWeight = "bold")
      }
    )
  )
)





table_currentx %>% dplyr::select(
  name,
  activity_curweek,
  tweets_curweek,
  comments_curweek,
  likes_curweek,
  likes_mean_curweek,

  activity_curmonth,
  tweets_curmonth,
  comments_curmonth,
  likes_curmonth,
  likes_mean_curmonth,

  activity_curyear,
  tweets_curyear,
  comments_curyear,
  likes_curyear,
  likes_mean_curyear
) -> table_currentx

# vizualise
reactable(
  table_currentx,
  resizable = TRUE,
  sortable = TRUE,
  filterable = TRUE,
  searchable = TRUE,
  pagination = TRUE,
  fullWidth = TRUE,
  defaultColDef = colDef(
    header = function(value) gsub("_", " ", value, fixed = TRUE),
    cell = function(value) format(value, nsmall = 1),
    align = "center",
    minWidth = 120
  )
)






# vizualise
reactable(
  table_currentx,
  resizable = TRUE,
  sortable = TRUE,
  filterable = TRUE,
  searchable = TRUE,
  pagination = TRUE,
  fullWidth = TRUE,
  # defaultColDef = colDef(
  #   header = function(value) gsub("_", " ", value, fixed = TRUE),
  #   cell = function(value) format(value, nsmall = 1),
  #   align = "center",
  #   minWidth = 120
  # ),
  columns = list(
    Politician = colDef(
      align = "left",
      minWidth = 180
    ),
    Twitter_Name = colDef(
      align = "left",
      minWidth = 180
    ),
    Party = colDef(
      minWidth = 90
    ),
    Mean_Post_Likes_pr_Follower = colDef(
      name = "MPLFollow",
      style = function(value) {
        if (value == first) {
          color <- "#FFD700"
        } else if (value == second) {
          color <- "#C0C0C0"
        } else if (value == third) {
          color <- "#C9AE5D"
        } else {
          color <- "#777"
        }
        list(color = color, fontWeight = "bold")
      }
    )
  )
)


