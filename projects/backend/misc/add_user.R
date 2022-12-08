# Script that add users to lists ----
dat <- openxlsx::read.xlsx(
  "/home/kasper/someR/data/lighthouselisten.xlsx"
)

add <- data.frame(
  user = "karmel80",
  list = "lighthouselisten",
  country = "Denmark",
  affiliation = "Politiker"
)

dat <- rbind(
  dat,
  add
)

dat %>% dplyr::filter(
  user != "Nikolajbjork"
) -> dat

openxlsx::write.xlsx(
  dat,
  "/home/kasper/someR/data/lighthouselisten.xlsx",
  rownames = F
)
