# Crude Oil Production

# DEPENDENCIES ------------------------------------------------------------

library(jsonlite)
library(blg)
library(rvest)
library(tidyverse)
library(httr)
source("scripts/import/functions.R")

# SETUP-CATEGORY-QUERY ----------------------------------------------------

api_url <- "http://api.eia.gov/"
req_cat <- "category/"
qry_cat <- 296686  # All states, monthly bpd
api_key_eia  <- getOption("api_key_eia")

url <- parse_url(api_url)
url$path  <- file_path(url$path, req_cat)

# MAKE-CATEGORY-REQUEST ---------------------------------------------------

# Build url; read html; convert to JSON
url$query <- list(api_key = api_key_eia, category_id = qry_cat)
url <- build_url(url)

res <- GET(url)

# Content
res_content <- res %>%
 content(as = "text") %>%
 fromJSON()

# Request (content)
res_request <- get_content_request(res_content)
res_cat   <- get_content_cat(res_content)
res_child <- as_tibble(get_content_child(res_cat))


# Extract only by desired frequency and units
res_child <- res_child %>%
filter(f == "M" & str_detect(units, "[T|t]housand [B|b]arrels [P|p]er [D|d]ay"))

res_child <- res_child %>%
 separate(name, c("state", "freq"), "\\, ", extra = "merge") %>%
 mutate(freq = str_remove(freq, "\\, Monthly"))



# SAVE META DATA ----------------------------------------------------------

load("~/R/oil/data/prod/cat_296686_meta.RData")

res_child <- res_child %>%
 mutate(updated = as.POSIXct(updated, format = "%d-%b-%y %I.%M.%S %p"))

cat_296686_meta <- res_child

# Find updated rows and update the 'updated' column
updated_rows <- cat_296686_meta$updated != res_child$updated
cat_296686_meta$updated[updated_rows] <- res_child$updated[updated_rows]

# Save meta
save(cat_296686_meta, file = "~/R/oil/data/prod/cat_296686_meta.RData")


# UPDATE SCHEDULE ---------------------------------------------------------

load("~/R/oil/data/prod/update_schedule.RData")

last_updated <- cat_296686_meta %>%
 pull(updated) %>%
 max()

update_schedule <- update_schedule %>%
 mutate(checked = if_else(category == qry_cat, Sys.time(), checked),
        updated =  if_else(category == qry_cat, last_updated, updated))

save(update_schedule, file = "~/R/oil/data/prod/update_schedule.RData")





# SCORCHED EARTH ----------------------------------------------------------

rm(list = ls(pattern = "[^updated_rows]"))


# RETURN STATUS -----------------------------------------------------------

# Was there an update?

date_updated <- any(sum(updated_rows))
