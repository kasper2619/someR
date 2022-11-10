dat <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/lighthouses.xlsx"
)

dat %>% dplyr::distinct(
  screen_name
) -> dat

# dat %>% dplyr::filter(
#  screen_name != "AnneSAndersen"
# ) -> dat

dat <- rbind(
  dat, data.frame("screen_name" = "KHegaard")
)

openxlsx::write.xlsx(dat,"/home/kasper/someR/data/lighthouses.xlsx", rownames = F)
