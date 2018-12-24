# DEPENDENCIES ------------------------------------------------------------

library(jsonlite)
library(blg)
library(rvest)
library(tidyverse)
library(httr)



# SETUP-CATEGORY-QUERY ----------------------------------------------------

api_url <- "http://api.eia.gov/"
req_cat <- "category/"
qry_cat  <- 2134979
api_key_eia  <- getOption("api_key_eia")

url <- parse_url(api_url)
url$path  <- file_path(url$path, req_cat)




# MAKE-CATEGORY-REQUEST ---------------------------------------------------

# Build url; read html; convert to JSON
meta <- qry_cat %>%
 map(function(x, u) {
  u$query <- list(api_key = api_key_eia, category_id = x)
  u <- build_url(u)
 }, u = url) %>%
 map(read_html) %>%
 map(html_text) %>%
 map(fromJSON)

# Extract meta info
meta <- meta %>%
 map(purrr::pluck, "category") 

# Extract only 'childseries' info
meta <- meta %>%
 map(purrr::pluck, "childseries") %>%
 map(as_tibble) %>%
 map(~filter(.x, f == "M" & units == "Thousand Barrels Per Day"))

meta <- meta[[1]] %>%
 separate(name, c("type", "country"), "\\, ", extra = "merge") %>%
 mutate(country = str_remove(country, "\\, Monthly"))

# Countries to remove
rm_countries <- paste("^Africa$|OPEC|Antarctica|Asia & Oceania",
                      "Central & South America|EU-15|EU-27|Eurasia|Europe",
                      "Hawaiian|IEA|Middle East|OECD|North America",
                      "Persian Gulf|World" 
                      ,sep = "|")

# Remove countries
meta <- meta %>%
 filter(!grepl(rm_countries, country))


# SAVE META DATA ----------------------------------------------------------

load("~/R/oil/data/prod/wocp_meta.RData")

meta <- meta %>%
 mutate(updated = as.POSIXct(updated, format = "%d-%b-%y %I.%M.%S %p"))

# Find updated rows and update the 'updated' column
updated_rows <- wocp_meta$updated != meta$updated
wocp_meta$updated[updated_rows] <- meta$updated[updated_rows]

# Save meta
save(wocp_meta, file = "~/R/oil/data/prod/wocp_meta.RData")


# UPDATE SCHEDULE ---------------------------------------------------------

load("~/R/oil/data/prod/update_schedule.RData")

last_updated <- meta %>%
 pull(updated) %>%
 max()

update_schedule <- update_schedule %>%
 mutate(checked = if_else(category == "wocp", Sys.time(), checked),
        updated =  if_else(category == "wocp", last_updated, updated))

save(update_schedule, file = "~/R/oil/data/prod/update_schedule.RData")





# SCORCHED EARTH ----------------------------------------------------------

rm(list = ls(pattern = "[^updated_rows]"))


# RETURN STATUS -----------------------------------------------------------

# Was there an update?

date_updated <- any(sum(updated_rows))
