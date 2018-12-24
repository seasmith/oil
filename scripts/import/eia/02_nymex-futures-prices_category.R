source("scripts/import/eia/01_nymex-futures-prices_category.R")

dir.create("data/eia/nymex-futures-prices")

save(series_meta, file = "data/eia/nymex-futures-prices/series_meta.RData")
save(series_data, file = "data/eia/nymex-futures-prices/series_data.RData")
