## ----setup_lib_std, include=FALSE----------------------------------------
# ____ Library_Tidyverse_Setup
library(dplyr)
library(tibble)

library(rvest)
library(jsonlite)

## ----setup_special, include=FALSE----------------------------------------
library(httr)

## ----import_oil_price----------------------------------------------------
api_url <- "https://api.stlouisfed.org/fred/"
req_type <- "series/observations"
req_qry <- "?series_id="
api_qry <- "&api_key="
api_key_fred <- getOption("api_key_fred")
file_type <- "&file_type="

fred_qry <- paste0(api_url, req_type,
                   req_qry, "DCOILWTICO",
                   api_qry, api_key_fred,
                   file_type, "json")

wti <- fred_qry %>%
  read_html() %>%
  html_text() %>%
  fromJSON() %>%
  .[["observations"]] %>%
  as_tibble() %>%
  select(-(1:2))

wti <- wti %>%
  mutate(value = as.double(value),
         date  = as.Date(date)) %>%
  rename(price = value)

## ----save_data-----------------------------------------------------------
save(wti, file = "~/R/oil/data/price/wti.RData")

## ----cleanup, include=FALSE----------------------------------------------
rm(list = ls(pattern = "api_key_"))

