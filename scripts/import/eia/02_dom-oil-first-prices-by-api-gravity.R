source("scripts/import/eia/01_dom-oil-first-prices-by-api-gravity.R")

base_dir <- "data/eia/"
new_dir <- file.path(base_dir, "dom-oil-first-prices-by-api-gravity")

dir.create(new_dir)

save(series_meta, file = file.path(new_dir, "series_meta.RData"))
save(series_data, file = file.path(new_dir, "series_data.RData"))
