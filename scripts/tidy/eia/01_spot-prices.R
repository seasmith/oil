load("data/eia/spot-prices/series_data.RData")
load("data/eia/spot-prices/series_meta.RData")

# Join for meta information to split by
series_data <- series_data %>%
 left_join(series_meta %>% select(series_id, name, unitsshort), by = "series_id")

# Rename
series_data <- series_data %>% 
 rename(date = V1, price = V2)

# Coerce to appropriate units
series_data <- series_data %>%
 mutate(date = as.Date(date, "%Y%m%d"),
        price = as.double(price))

# Rearrange
series_data <- series_data %>%
 select(date, price, name, unitsshort, series_id)

# Short-names for files and R objects
f_name_list <- list(
 PET.EER_EPD2DC_PF4_Y05LA_DPG.D = "la_diesel",
 PET.EER_EPD2DXL0_PF4_RGC_DPG.D = "us_gom_diesel",
 PET.EER_EPD2DXL0_PF4_Y35NY_DPG.D = "ny_diesel",
 PET.EER_EPD2F_PF4_Y35NY_DPG.D = "ny_heating",
 PET.EER_EPJK_PF4_RGC_DPG.D = "us_gom_kerosene",
 PET.EER_EPLLPA_PF4_Y44MB_DPG.D = "mt_bel_propane",
 PET.EER_EPMRR_PF4_Y05LA_DPG.D = "la_gasoline",
 PET.EER_EPMRU_PF4_RGC_DPG.D = "us_gom_gasoline",
 PET.EER_EPMRU_PF4_Y35NY_DPG.D = "ny_gasoline",
 PET.RBRTE.D = "brent_oil",
 PET.RWTC.D = "cushing_wti_oil"
)

series_data_split <- series_data %>%
 split(.$series_id)

series_data_split %>%
 map2(names(.), ~{
  readr::write_csv(x = .x, path = paste0("data/eia/spot-prices/", f_name_list[[.y]], ".csv"))
 })
