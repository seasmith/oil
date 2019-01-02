# DEPENDENCIES ------------------------------------------------------------

library(jsonlite)
library(blg)
library(rvest)
library(tidyverse)
library(httr)

not_one_of <- compose(`-`, one_of)
`%not_in%` <- compose(`!`, `%in%`)
qry_cat <- 296686  # All states, monthly bpd




# UPDATE SCHEDULE ---------------------------------------------------------

load("~/R/oil/data/prod/update_schedule.RData")

# When did this execution happen?
update_schedule <- update_schedule %>%
 mutate(executed = if_else(category == qry_cat, Sys.time(), executed))

save(update_schedule, file = "~/R/oil/data/prod/update_schedule.RData")


# SETUP-SERIES-QUERY ------------------------------------------------------

# Load meta data
load("~/R/oil/data/prod/cat_296686_meta.RData")

# Extract series to 
meta_series <- cat_296686_meta %>%
 pull("series_id") %>%
 set_names(nm = cat_296686_meta %>% pull("state"))

api_key_eia  <- getOption("api_key_eia")
api_url <- "http://api.eia.gov/"
req_srs <- "/series/"
qry_srs <- "&series_id="

url <- parse_url(api_url)

# url <- parse_url(api_url)
reqs <- meta_series %>%
 map(function(.x, u) {
  # map(x, ~{
   u$path  <- file_path(req_srs)
   u$query <- list(api_key = api_key_eia,
                   series_id = .x)
   build_url(u)
  # })
 }, u = url)




# MAKE-SERIES-QUERY -------------------------------------------------------

res <- map(reqs, ~fromJSON(html_text(read_html(.x))))

# res2 <- map(reqs, GET)


# SAVE META DATA ----------------------------------------------------------

res_series <- res %>%
 map(purrr::pluck, "series")

# Extract
cat_296686_meta2 <- res_series %>%
 map(~.x[, names(.x) %not_in% "data"]) %>%
 bind_rows() %>%
 as_tibble()

# Combine
exclude <- intersect(names(cat_296686_meta), names(cat_296686_meta2))
exclude <- exclude[exclude != "series_id"]

cat_296686_meta <- cat_296686_meta %>%
 inner_join(cat_296686_meta2 %>%
             select(not_one_of(exclude)), by = "series_id")

# Save
save(cat_296686_meta, file = "~/R/oil/data/prod/cat_296686_meta.RData")




# TIDY-DATA ---------------------------------------------------------------

# Extract data
res_data <- res_series %>%
 map(~.x[, names(.x) %in% "data"]) %>%
 modify_depth(2, as_tibble) %>%
 map(bind_rows) %>%
 bind_rows(.id = "location")

# Coerce columns to correct data types
res_data <- res_data %>%
 mutate(V1 = as.Date(paste0(V1, 01), format = "%Y%m%d"),
        V2 = as.integer(V2))

# Remove 'location' column and rename others
res_data <- res_data %>%
 rename(date = V1, prod = V2)



# SAVE-DATA ---------------------------------------------------------------

# Rename
us_crude_prod <- res_data

# Save
save(us_crude_prod, file = "~/R/oil/data/prod/us_crude_prod.RData")




# SCORCHED EARTH ----------------------------------------------------------

rm(list = ls())
