dat <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/folketinget.xlsx"
)

#dat %>% dplyr::filter(
#  screen_name != "SandieWesth"
#) -> dat

dat <- rbind(dat, data.frame(
  "user" = "SikandaSIDDIQUE",
  "party" = "FG",
  "blok" = "blue"
))

openxlsx::write.xlsx(dat,"/home/kasper/someR/data/folketinget.xlsx", rownames = F)

dat %>% dplyr::filter(
  user != "SikandaSIDDIQUE"
) -> dat
