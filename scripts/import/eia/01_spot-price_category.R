library(rvest)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

eia_api_url <- "http://api.eia.gov"
spot_price_cat_id <- 241335

# source("scripts/import/eia/00_master-functions.R")
source("scripts/import/functions.R")

# SCRAPE SPOT PRICE CATEGORY ----------------------------------------------

response_json <- spot_price_cat_id %>%
 set_cat_url() %>%
 read_html() %>%
 get_cat_json()

response_parent <- response_json$category$parent_category_id
response_name <- response_json$category$name  # table name
response_data <- response_json$category$childseries  # write to data base

response_data_daily <- filter(response_data, f == "D")

# PUT IN CODE TO COMPARE LAST 'updated' FIELD IN DATABASE TO CURRENT VALUE

# ...

# IF CURRENT VALUE IS GREATER THAN DATABASE VALUE

series_json <- response_data_daily %>%
 pull(series_id) %>%
 map(set_series_url) %>%
 map(read_html) %>%
 map(get_series_json)

series_meta <- series_json %>%
 map_df(get_series_meta)

series_data <- series_json %>%
 map(get_series_data) %>%
 set_names(series_meta$series_id) %>%
 bind_rows(.id = "series_id")
