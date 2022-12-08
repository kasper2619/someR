dat <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/folketinget.xlsx"
)

dat %>% dplyr::filter(
  user != "stinuslindgreen" | is.na(user) == T
) -> dat

dat <- rbind(dat, data.frame(
  "user" = "uffeelbaek",
  "party" = "Å",
  "blok" = "red",
  "navn" = "Uffe Elbæk",
  "list" = NA
))

openxlsx::write.xlsx(dat,"/home/kasper/someR/data/folketinget.xlsx", rownames = F)

dat %>% dplyr::filter(
  user != "SikandaSIDDIQUE"
) -> dat
