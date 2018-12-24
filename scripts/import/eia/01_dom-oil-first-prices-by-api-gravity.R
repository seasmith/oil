library(rvest)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

id <- 293653
freq_order <- c("D", "W", "M", "Q", "A")

source("scripts/import/eia/00_master-functions.R")

# SCRAPE SPOT PRICE CATEGORY ----------------------------------------------

response_json <- id %>%
 set_cat_url() %>%
 read_html() %>%
 get_cat_json()

response_parent <- response_json$category$parent_category_id
response_name <- response_json$category$name  # table name
response_data <- response_json$category$childseries  # write to data base

hi_freq <- response_data$f %>%
 unique() %>%
 factor(freq_order, ordered = TRUE) %>%
 min()

response_data_hi_freq <- filter(response_data, f == !!hi_freq)

# PUT IN CODE TO COMPARE LAST 'updated' FIELD IN DATABASE TO CURRENT VALUE

# ...

# IF CURRENT VALUE IS GREATER THAN DATABASE VALUE

series_json <- response_data_hi_freq %>%
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
