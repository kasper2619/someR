dat <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/lighthouses.xlsx"
)

dat %>% dplyr::distinct(
  screen_name
) -> dat

# dat %>% dplyr::filter(
#  screen_name != "km_frydensbjerg"
# ) -> dat

dat <- rbind(
  dat, data.frame("screen_name" = "JanHoby")
)

openxlsx::write.xlsx(dat,"/home/kasper/someR/data/lighthouses.xlsx", rownames = F)
