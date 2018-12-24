library(dplyr)
library(purrr)

this_dir <- file.path("data/eia/dom-oil-first-prices-by-api-gravity")
load(file.path(this_dir, "series_data.RData"))
load(file.path(this_dir, "series_meta.RData"))

# Join for meta information to split by
series_data <- series_data %>%
 left_join(series_meta %>% select(series_id, name, unitsshort), by = "series_id")

# Rename
series_data <- series_data %>% 
 rename(date = V1, price = V2)

# Coerce to appropriate units
freq <- unique(series_meta$f)

series_data <- series_data %>%
 mutate(date = case_when(
  grepl("D|W", freq) ~ as.Date(date, "%Y%m%d"),
  grepl("M|Q", freq) ~ as.Date(paste0(date, "01"), "%Y%m%d"),
  grepl("A", freq)   ~ as.Date(paste0(date, "0101"), "%Y%m%d"),
  TRUE ~ lubridate::as_date(date)
 ),
 price = as.double(price))

# Rearrange
series_data <- series_data %>%
 select(date, price, name, unitsshort, series_id)

# Short-names for files and R objects
f_name_list <- list(PET.F000000013.M = "us_oil_api_20_less", 
                    PET.F000000023.M = "us_oil_api_20_25", 
                    PET.F000000033.M = "us_oil_api_25_30", 
                    PET.F000000043.M = "us_oil_api_30_35",
                    PET.F000000053.M = "us_oil_api_35_40",
                    PET.F000000063.M = "us_oil_api_40_more")

series_data_split <- series_data %>%
 split(.$series_id)

series_data_split %>%
 map2(names(.), ~{
  readr::write_csv(x = .x, path = paste0(this_dir, "/", f_name_list[[.y]], ".csv"))
 })
