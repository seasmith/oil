new_dir <- "costs-of-imported-oil-by-api-gravity"

source(paste0("scripts/import/eia/", "01_", new_dir, ".R"))

base_dir <- "data/eia/"
new_dir <- file.path(base_dir, new_dir)

dir.create(new_dir)

save(series_meta, file = file.path(new_dir, "series_meta.RData"))
save(series_data, file = file.path(new_dir, "series_data.RData"))
