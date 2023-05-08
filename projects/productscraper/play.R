# Libraries ----
library(dplyr)
library(tidyr)
library(rvest)
library(RSelenium)
library(xml2)

#
dat <- rvest::read_html("https://danbohesselager.dk/moebler/sofaer")
dat

dat_prod <- dat %>% rvest::html_elements(
  ".catalog-product"
)

rvest::html_children(dat_prod)

dat_prod_title <- rvest::html_nodes(dat_prod, ".catalog-product-title")
dat_prod_price <- rvest::html_nodes(dat_prod, ".catalog-product-price")
dat_prod_info <- rvest::html_nodes(dat_prod, ".catalog-product-info")
dat_prod_image <- rvest::html_nodes(dat_prod, ".catalog-product-img")

name <- xml_text(xml_find_all(dat_prod, xpath = "//name"))
price <- xml_text(xml_find_all(dat_prod, xpath = "//pris"))



bind_rows(lapply(xml_attrs(dat_prod_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))




as.data.frame(dat_prod_title)



rvest::html_attrs(dat_prod)


rvest::html_table(dat_prod)

html_children(x = dat_prod, name = "brandImage")


x <- dat_prod %>% html_attrs("")

html_nodes(dat_prod, "catalog-product-title")

dat_price <- dat %>% rvest::html_elements(
  ".catalog-product-price"
)

as.character(dat_prod[2])



xml_address = "http://www.fehd.gov.hk/english/licensing/license/text/LP_Restaurants_EN.XML"

restaurant_license_xml = as_list(read_xml(xml_address))
