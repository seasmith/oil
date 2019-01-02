# DEPENDENCIES ------------------------------------------------------------

library(jsonlite)
library(blg)
library(rvest)
library(tidyverse)
library(httr)

`%not_in%` <- compose(`!`, `%in%`)
qry_cat  <- 2134979



# UPDATE SCHEDULE ---------------------------------------------------------

load("~/R/oil/data/prod/update_schedule.RData")

# When did this execution happen?
update_schedule <- update_schedule %>%
 mutate(executed = if_else(category == qry_cat, Sys.time(), executed))




# SETUP-SERIES-QUERY ------------------------------------------------------

# Load meta data
load("~/R/oil/data/prod/wocp_meta.RData")

# Extract series to 
meta_series <- wocp_meta %>%
 pull("series_id") %>%
 set_names(nm = wocp_meta %>% pull("country"))

api_key_eia  <- getOption("api_key_eia")
api_url <- "http://api.eia.gov/"
req_srs <- "/series/"
qry_srs <- "&series_id="

url <- parse_url(api_url)

# url <- parse_url(api_url)
reqs <- meta_series %>%
 map(function(x, u) {
  map(x, ~{
   u$path  <- file_path(req_srs)
   u$query <- list(api_key = api_key_eia,
                   series_id = .x)
   build_url(u)
  })
 }, u = url)




# MAKE-SERIES-QUERY -------------------------------------------------------

wocp <- map(reqs,
                ~map(.x, ~fromJSON(html_text(read_html(.x))))
)




# SAVE META DATA ----------------------------------------------------------

# Extract
wocp_meta2 <- wocp %>%
 modify_depth(2, purrr::pluck, "series") %>%
 modify_depth(2, function(x) x[, names(x) %not_in% "data"]) %>%
 map(bind_rows) %>%
 map(as_tibble) %>%
 bind_rows()

# Combine
wocp_meta <- wocp_meta %>%
 inner_join(wocp_meta2 %>%
             select(-units, -f, -updated), by = "series_id")

# Save
save(wocp_meta, file = "~/R/oil/data/prod/wocp_meta.RData")




# TIDY-DATA ---------------------------------------------------------------

# Extract data
wocp <- wocp %>%
 modify_depth(2, purrr::pluck, "series") %>%
 modify_depth(2, function(x) x[, names(x) %in% "data"]) %>%
 modify_depth(3, as_tibble) %>%
 modify_depth(2, bind_rows) %>%
 modify_depth(1, bind_rows, .id = "location")

# Coerce columns to correct data types
wocp <- wocp %>%
 map(~mutate(.x, V1 = as.Date(paste0(V1, 01), format = "%Y%m%d"),
             V2 = as.integer(V2))) %>%
 bind_rows(.id = "country")

# Remove 'location' column and rename others
wocp <- wocp %>%
 select(-location) %>%
 rename(date = V1, prod = V2)




# SAVE-DATA ---------------------------------------------------------------

save(wocp, file = "~/R/oil/data/prod/wocp.RData")




# SCORCHED EARTH ----------------------------------------------------------

rm(list = ls())
