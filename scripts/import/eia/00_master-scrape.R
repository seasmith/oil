library(purrr)
library(rvest)
library(httr)
library(jsonlite)

# This is the top category id in the EIA API for EIA Data Sets. There are other
# sections of the API, such as maps, but this is the section holding all the
# table data.
top_parent_id <- 371

source("scripts/import/eia/00_master-functions.R")
