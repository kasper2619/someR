# # Libraries ----
# library(rhandsontable)
#
# # Load Data ----
# dat <- openxlsx::read.xlsx(
#   "/home/kasper/someR/data/lighthouselisten.xlsx"
# )
#
# dat %>% dplyr::filter(
#   user != "BihlWinum"
# ) -> dat
#
# openxlsx::write.xlsx(
#   dat,
#   "/home/kasper/someR/data/lighthouselisten.xlsx",
#   rownames = F
# )
#
# status <- c("keep","delete", "added")
# dat[["status"]] <- factor(
#   NA,
#   levels = status,
#   ordered = TRUE
# )
#
# dat %>% dplyr::arrange(
#   party
# ) -> dat
#
# rhandsontable(dat, width = 600, height = 300)
#
# openxlsx::write.xlsx(
#   dat,
#   "/home/kasper/someR/data/twittertinget.xlsx",
#   rownames = F
# )
