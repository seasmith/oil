library(dplyr)
library(purrr)

load("data/eia/nymex-futures-prices/series_data.RData")
load("data/eia/nymex-futures-prices/series_meta.RData")

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
f_name_list <- list(PET.EER_EPD2F_PE1_Y35NY_DPG.D = "ny_heating_1", 
                    PET.EER_EPD2F_PE2_Y35NY_DPG.D = "ny_heating_2", 
                    PET.EER_EPD2F_PE3_Y35NY_DPG.D = "ny_heating_3",
                    PET.EER_EPD2F_PE4_Y35NY_DPG.D = "ny_heating_4", 
                    PET.EER_EPLLPA_PE1_Y44MB_DPG.D = "mt_bel_propane_1",
                    PET.EER_EPLLPA_PE2_Y44MB_DPG.D = "mt_bel_propane_2", 
                    PET.EER_EPLLPA_PE3_Y44MB_DPG.D = "mt_bel_propane_3",
                    PET.EER_EPLLPA_PE4_Y44MB_DPG.D = "mt_bel_propane_4", 
                    PET.EER_EPMRR_PE1_Y35NY_DPG.D = "ny_gasoline_ref_1", 
                    PET.EER_EPMRR_PE2_Y35NY_DPG.D = "ny_gasoline_ref_2", 
                    PET.EER_EPMRR_PE3_Y35NY_DPG.D = "ny_gasoline_ref_3",
                    PET.EER_EPMRR_PE4_Y35NY_DPG.D = "ny_gasoline_ref_4", 
                    PET.EER_EPMR_PE1_Y35NY_DPG.D = "ny_gasoline_reg_1",
                    PET.EER_EPMR_PE2_Y35NY_DPG.D = "ny_gasoline_reg_2", 
                    PET.EER_EPMR_PE3_Y35NY_DPG.D = "ny_gasoline_reg_3", 
                    PET.EER_EPMR_PE4_Y35NY_DPG.D = "ny_gasoline_reg_4", 
                    PET.RCLC1.D = "cushing_oil_1",
                    PET.RCLC2.D = "cushing_oil_2",
                    PET.RCLC3.D = "cushing_oil_3",
                    PET.RCLC4.D = "cushing_oil_4")

series_data_split <- series_data %>%
 split(.$series_id)

series_data_split %>%
 map2(names(.), ~{
  readr::write_csv(x = .x, path = paste0("data/eia/nymex-futures-prices/", f_name_list[[.y]], ".csv"))
 })
