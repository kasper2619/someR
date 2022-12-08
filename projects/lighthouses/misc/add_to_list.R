dat <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/lighthouses.xlsx"
)

dat %>% dplyr::distinct(
  screen_name
) -> dat

dat %>% dplyr::filter(
 screen_name != "NikolajBjorg"
) -> dat

dat <- rbind(
  dat, data.frame("screen_name" = "NikolajBjork")
)

openxlsx::write.xlsx(dat,"/home/kasper/someR/data/lighthouses.xlsx", rownames = F)
