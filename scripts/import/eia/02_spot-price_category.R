source("scripts/import/eia/01_spot-price_category.R")

save(series_meta, file = "data/eia/spot-prices/series_meta.RData")
save(series_data, file = "data/eia/spot-prices/series_data.RData")
