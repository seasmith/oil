library(dplyr)
library(purrr)

this_dir <- file.path("data/eia/costs-of-imported-oil-by-api-gravity")
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
f_name_list <- list(PET.I000000208.M = "imported_oil_api_20_less", 
                    PET.I000000258.M = "imported_oil_api_20_25", 
                    PET.I000000308.M = "imported_oil_api_25_30", 
                    PET.I000000358.M = "imported_oil_api_30_35",
                    PET.I000000408.M = "imported_oil_api_35_40",
                    PET.I000000458.M = "imported_oil_api_40_45",
                    PET.I000000998.M = "imported_oil_api_45_more")

series_data_split <- series_data %>%
 split(.$series_id)

series_data_split %>%
 map2(names(.), ~{
  readr::write_csv(x = .x, path = paste0(this_dir, "/", f_name_list[[.y]], ".csv"))
 })
