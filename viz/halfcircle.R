### MEDLEMMER ###
con <- someR::con_sql()
res <- dbSendQuery(
  con,
  "SELECT
        username,
        name,
        profile_image_url,
        affiliation,
        party,
        variable,
        value
      FROM twitter_scoreboard
      WHERE list IN ('twittertinget')"
)
dat <- dbFetch(res, n = -1)
dbClearResult(res)
DBI::dbDisconnect(con)

dat <- reshape2::dcast(
  dat,
  "username + name + profile_image_url + affiliation + party ~ variable",
  value.var = "value"
)

dat %>% dplyr::select(
  username,
  name,
  profile_image_url,
  affiliation,
  party
) -> dat_viz

# reorder
dat_viz[["party"]] <- as.factor(dat_viz[["party"]])
dat_viz[["party"]] <- factor(dat_viz[["party"]], levels = c("Ø","Å","F","A","GL","B","M","C","FØ","V","I","Æ","O","D","FRI"))

dat_viz %>% dplyr::group_by(
  party
) %>% dplyr::summarise(
  count = n()
) -> dat_viz

dat_viz %>% dplyr::mutate(
  color = "#202123",
  color = ifelse(party == "A","#F40526",color),
  color = ifelse(party == "Æ","#4E77A3",color),
  color = ifelse(party == "B","#E82E8A",color),
  color = ifelse(party == "C","#165738",color),
  color = ifelse(party == "D","#134851",color),
  color = ifelse(party == "F","#EE9C9F",color),
  color = ifelse(party == "FRI","#BDBDBD",color),
  color = ifelse(party == "FØ","#BDBDBD",color),
  color = ifelse(party == "GL","#BDBDBD",color),
  color = ifelse(party == "I","#EEA925",color),
  color = ifelse(party == "M","#842990",color),
  color = ifelse(party == "O","#235CA9",color),
  color = ifelse(party == "Ø","#D0004E",color),
  color = ifelse(party == "V","#19438E",color),
  color = ifelse(party == "Å","#37BD00",color)
) -> dat_viz

hchart(
  dat_viz,
  "item",
  hcaes(
    name = party,
    y = count,
    #label = abbrv,
    color = color
  ),
  name = "",
  showInLegend = TRUE,
  size = "100%",
  center = list("50%", "75%"),
  startAngle = -100,
  endAngle  = 100
) %>%
  hc_title(text = "Twittertinget") %>%
  hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>')

### LIKES ###
con <- someR::con_sql()
res <- dbSendQuery(
  con,
  "SELECT
        username,
        name,
        profile_image_url,
        affiliation,
        party,
        variable,
        value
      FROM twitter_scoreboard
      WHERE list IN ('twittertinget')"
)
dat <- dbFetch(res, n = -1)
dbClearResult(res)
DBI::dbDisconnect(con)

dat <- reshape2::dcast(
  dat,
  "username + name + profile_image_url + affiliation + party ~ variable",
  value.var = "value"
)

dat %>% dplyr::select(
  username,
  name,
  profile_image_url,
  affiliation,
  party,
  likes_curweek
) -> dat_viz

# reorder
dat_viz[["party"]] <- as.factor(dat_viz[["party"]])
dat_viz[["party"]] <- factor(dat_viz[["party"]], levels = c("Ø","Å","F","A","GL","B","M","C","FØ","V","I","Æ","O","D","FRI"))

dat_viz %>% dplyr::group_by(
  party
) %>% dplyr::summarise(
  count = round((sum(likes_curweek,na.rm=T)/sum(dat_viz[["likes_curweek"]],na.rm=T)*100),0)
) -> dat_viz

dat_viz %>% dplyr::mutate(
  color = "#202123",
  color = ifelse(party == "A","#F40526",color),
  color = ifelse(party == "Æ","#4E77A3",color),
  color = ifelse(party == "B","#E82E8A",color),
  color = ifelse(party == "C","#165738",color),
  color = ifelse(party == "D","#134851",color),
  color = ifelse(party == "F","#EE9C9F",color),
  color = ifelse(party == "FRI","#BDBDBD",color),
  color = ifelse(party == "FØ","#BDBDBD",color),
  color = ifelse(party == "GL","#BDBDBD",color),
  color = ifelse(party == "I","#EEA925",color),
  color = ifelse(party == "M","#842990",color),
  color = ifelse(party == "O","#235CA9",color),
  color = ifelse(party == "Ø","#D0004E",color),
  color = ifelse(party == "V","#19438E",color),
  color = ifelse(party == "Å","#37BD00",color)
) -> dat_viz

hchart(
  dat_viz,
  "item",
  hcaes(
    name = party,
    y = count,
    #label = abbrv,
    color = color
  ),
  name = "Folketingsmedlemmer",
  showInLegend = TRUE,
  size = "100%",
  center = list("50%", "75%"),
  startAngle = -100,
  endAngle  = 100
) %>%
  hc_title(text = "Folketinget på Twitter fordelt på Likes (%)") %>%
  hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>')

### MEDLEMMER ###
con <- someR::con_sql()
res <- dbSendQuery(
  con,
  "SELECT
        username,
        name,
        profile_image_url,
        affiliation,
        party,
        variable,
        value
      FROM twitter_scoreboard
      WHERE list IN ('twittertinget')"
)
dat <- dbFetch(res, n = -1)
dbClearResult(res)
DBI::dbDisconnect(con)

dat <- reshape2::dcast(
  dat,
  "username + name + profile_image_url + affiliation + party ~ variable",
  value.var = "value"
)

dat %>% dplyr::select(
  username,
  name,
  profile_image_url,
  affiliation,
  party
) -> dat_viz

# reorder
dat_viz[["affiliation"]] <- as.factor(dat_viz[["affiliation"]])
dat_viz[["affiliation"]] <- factor(dat_viz[["affiliation"]], levels = c(
  "Enhedslisten","Alternativet","Socialistisk Folkeparti",
  "Socialdemokratiet","Grønland","Radikale Venstre",
  "Moderaterne","Konservative","Færøerne",
  "Venstre","Liberal Alliance","Danmarksdemokraterne",
  "Dansk Folkeparti","Nye Borgerlige","Frigænger")
)

dat_viz %>% dplyr::group_by(
  affiliation,party
) %>% dplyr::summarise(
  count = n()
) -> dat_viz

dat_viz %>% dplyr::mutate(
  color = "#202123",
  color = ifelse(party == "A","#F40526",color),
  color = ifelse(party == "Æ","#4E77A3",color),
  color = ifelse(party == "B","#E82E8A",color),
  color = ifelse(party == "C","#165738",color),
  color = ifelse(party == "D","#134851",color),
  color = ifelse(party == "F","#EE9C9F",color),
  color = ifelse(party == "FRI","#BDBDBD",color),
  color = ifelse(party == "FØ","#BDBDBD",color),
  color = ifelse(party == "GL","#BDBDBD",color),
  color = ifelse(party == "I","#EEA925",color),
  color = ifelse(party == "M","#842990",color),
  color = ifelse(party == "O","#235CA9",color),
  color = ifelse(party == "Ø","#D0004E",color),
  color = ifelse(party == "V","#19438E",color),
  color = ifelse(party == "Å","#37BD00",color)
) -> dat_viz

hchart(
  dat_viz,
  "item",
  hcaes(
    name = affiliation,
    y = count,
    #label = abbrv,
    color = color
  ),
  name = "",
  showInLegend = TRUE,
  size = "100%",
  center = list("50%", "75%"),
  startAngle = -100,
  endAngle  = 100
) %>%
  hc_title(text = "Twittertinget") %>%
  hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>')
