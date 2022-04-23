dat <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/lighthouses.xlsx"
)

#dat %>% dplyr::filter(
#  screen_name != "SandieWesth"
#) -> dat

dat <- rbind(dat, data.frame("screen_name" = "QvortrupHenrik"))

openxlsx::write.xlsx(dat,"/home/kasper/someR/data/lighthouses.xlsx", rownames = F)
