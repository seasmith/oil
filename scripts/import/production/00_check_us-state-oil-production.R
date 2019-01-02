# DEPENDENCIES ------------------------------------------------------------

library(jsonlite)
library(blg)
library(rvest)
library(tidyverse)
library(httr)



# SETUP-CATEGORY-QUERY ----------------------------------------------------

# List of categories
cats <- list(
 crude_oil_production = 296686,  # All states, monthly bpd
 production_by_api    = 2477496  # All states, monthly bpd
)

api_url <- "http://api.eia.gov/"
req_cat <- "category/"
api_key_eia  <- getOption("api_key_eia")

url <- parse_url(api_url)
url$path  <- file_path(url$path, req_cat)

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

meta_extra <- meta %>%
 map(~{.x[names(.x) %not_in% "childseries"]})

meta <- meta %>%
 map(purrr::pluck, "childseries") %>%
 map(as_tibble) %>%
 map(~filter(.x, f == "M" & units == "Thousand Barrels per Day"))

meta_series <- meta %>%
 map(pull, "series_id") %>%
 map2(meta %>% map(pull, "name"), set_names)


# SAVE META DATA ----------------------------------------------------------

load("~/R/oil/data/prod/prod_api_meta.RData")

meta <- meta %>%
 map(~mutate(.x, 
             updated = as.POSIXct(updated, format = "%d-%b-%y %I.%M.%S %p")))

save(prod_api_meta, file = "~/R/oil/data/prod/prod_api_meta.RData")


# UPDATE SCHEDULE ---------------------------------------------------------

load("~/R/oil/data/prod/update_schedule.RData")

last_updated <- meta %>%
 map_df(~distinct(.x, updated)) %>%
 pull(updated) %>%
 max()

update_schedule <- update_schedule %>%
 mutate(executed = if_else(category == "prod_api", Sys.time(), executed),
        updated =  if_else(category == "prod_api", updated, updated))

save(update_schedule, file = "~/R/oil/data/prod/update_schedule.RData")

