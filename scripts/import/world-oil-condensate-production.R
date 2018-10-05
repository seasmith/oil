# DEPENDENCIES ------------------------------------------------------------

library(jsonlite)
library(blg)
library(rvest)
library(tidyverse)
library(httr)

`%not_in%` <- compose(`!`, `%in%`)




# SETUP-CATEGORY-QUERY ----------------------------------------------------

api_url <- "http://api.eia.gov/"
req_cat <- "category/"
qry_cat  <- 2134979
api_key_eia  <- getOption("api_key_eia")

url <- parse_url(api_url)
url$path  <- file_path(url$path, req_cat)




# MAKE-CATEGORY-REQUEST ---------------------------------------------------

meta <- qry_cat %>%
 map(function(x, u) {
  u$query <- list(api_key = api_key_eia, category_id = x)
  u <- build_url(u)
 }, u = url) %>%
 map(read_html) %>%
 map(html_text) %>%
 map(fromJSON)

meta <- meta %>%
 map(purrr::pluck, "category") 

meta_meta <- meta %>%
 map(~{.x[names(.x) %not_in% "childseries"]})

meta <- meta %>%
 map(purrr::pluck, "childseries") %>%
 map(as_tibble) %>%
 map(~filter(.x, f == "M" & units == "Thousand Barrels Per Day"))

meta <- meta[[1]] %>%
 separate(name, c("type", "country"), "\\, ", extra = "merge") %>%
 mutate(country = str_remove(country, "\\, Monthly"))

rm_countries <- paste("^Africa$|OPEC|Antarctica|Asia & Oceania",
                       "Central & South America|EU-15|EU-27|Eurasia|Europe",
                      "Hawaiian|IEA|Middle East|OECD|North America",
                      "Persian Gulf|World" 
                      ,sep = "|")

meta <- meta %>%
 filter(!grepl(rm_countries, country))

meta_series <- meta %>%
 pull("series_id") %>%
 set_names(nm = meta %>% pull("country"))




# SETUP-SERIES-QUERY ------------------------------------------------------

req_srs <- "/series/"
qry_srs <- "&series_id="

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




# TIDY-DATA ---------------------------------------------------------------

# Extract meta
wocp_meta <- wocp %>%
 modify_depth(2, purrr::pluck, "series") %>%
 modify_depth(2, function(x) x[, names(x) %not_in% "data"]) %>%
 map(bind_rows) %>%
 map(as_tibble)

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
