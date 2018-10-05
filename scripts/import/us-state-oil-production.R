

# DEPENDENCIES ------------------------------------------------------------

# library(pals)
library(jsonlite)
library(blg)
library(rvest)
library(tidyverse)
library(httr)

`%not_in%` <- compose(`!`, `%in%`)




# SETUP-CATEGORY-QUERY ----------------------------------------------------

# List of categories
cats <- list(
 crude_oil_production = 296686,  # All states, monthly bpd
 production_by_api    = 2477496  # All states, monthly bpd
)

# Possible categories
# 236392  # Supply and disposition summary

# Possible series
# PET.WCRFPUS2.W  # Weekly US Field Production
# PET.WCRNTUS2.W  # Weekly US Net Oil Imports
# PET.WTTNTUS2.W  # Weekly US Net Oil and Petroleum Products Imports
# PET.ESM_EPC0_RAIL_NUS-NUS_MBBL.M  # Monthly US Oil Rail Shipments

api_url <- "http://api.eia.gov/"
req_cat <- "category/"
api_key_eia  <- getOption("api_key_eia")

url <- parse_url(api_url)
url$path  <- file_path(url$path, req_cat)
# url$query <- list(api_key = api_key_eia,
#                   category_id = qry_id)
# url <- build_url(url)



# MAKE-CATEGORY-REQUEST ---------------------------------------------------

meta <- cats %>%
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
 map(~filter(.x, f == "M" & units == "Thousand Barrels per Day"))

meta_series <- meta %>%
 map(pull, "series_id") %>%
 map2(meta %>% map(pull, "name"), set_names)



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

prod_api <- map(reqs,
                ~map(.x, ~fromJSON(html_text(read_html(.x))))
)


prod_api_meta <- prod_api %>%
 modify_depth(2, purrr::pluck, "series") %>%
 modify_depth(2, function(x) x[, names(x) %not_in% "data"]) %>%
 map(bind_rows) %>%
 map(as_tibble)

prod_api <- prod_api %>%
 modify_depth(2, purrr::pluck, "series") %>%
 modify_depth(2, function(x) x[, names(x) %in% "data"]) %>%
 modify_depth(3, as_tibble) %>%
 modify_depth(2, bind_rows) %>%
 modify_depth(1, bind_rows, .id = "location")



# TIDY-DATA ---------------------------------------------------------------

# Separate 'location' column into two parts: 'location' and 'type'
prod_api$production_by_api <- prod_api$production_by_api %>%
 separate(location, c("location", "type"), " Crude Oil and Lease Condensate Production for ")

# Eliminate uneeded meta information in 'location'
prod_api$crude_oil_production <- prod_api$crude_oil_production %>%
 mutate(location = str_replace(location, " Crude Oil Production, Monthly", ""),
        location = str_replace(location, " Field Production of Crude Oil, Monthly", ""))
prod_api$production_by_api <- prod_api$production_by_api %>%
 mutate(type = str_replace(type, ", Monthly", ""))

# Correct misworded 'type'
prod_api$production_by_api <- prod_api$production_by_api %>%
 mutate(type = if_else(grepl("ULL", type), "50.1 or Higher  Degrees API Gravity", type))

# Rename and convert 'V1'
prod_api <- prod_api %>%
 map(~{
  .x %>%
   rename(date = V1, prod = V2) %>%
   mutate(date = as.Date(paste0(date, "01"), "%Y%m%d"),
          prod = as.integer(prod))
 })



# SAVE-DATA ---------------------------------------------------------------

save(prod_api, file = "~/R/oil/data/prod/prod_api.RData")
