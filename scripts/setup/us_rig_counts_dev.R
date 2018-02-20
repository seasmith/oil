library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(raster)
library(nord)

load("data/maps/states_48.RData")
states_48 <- st_transform(states_48, crs = st_crs(states_48))
states_48_single <- st_union(states_48)

ge <- list.files("~/R/misc/maps/raster", recursive = TRUE, full.names = TRUE) %>%
  grep("GRAY_LR_SR_W\\.tif", ., value = TRUE) %>%
  raster()

# Set CRS equal
this_crs <- st_crs(states_48_single)
projection(ge) <- this_crs$proj4string
states_48_single_sp <- as(states_48_single, "Spatial")
# states_48_sp <- as(st_transform(states_48, crs = this_crs), "Spatial")

us_48_gray_raster <- crop(ge, states_48_single_sp)
us_48_gray_raster_spdf <- as(us_48_gray_raster, "SpatialPixelsDataFrame")
us_48_only <- over(us_48_gray_raster_spdf, states_48_single_sp, fn = NULL)
us_48_only_gray_raster_spdf <- us_48_gray_raster_spdf[!is.na(us_48_only), ]


us_48_gray_raster <- as.data.frame(us_48_gray_raster_spdf)
us_48_only_gray_raster <- as.data.frame(us_48_only_gray_raster_spdf)
names(us_48_gray_raster)[1L] <- "ele"
names(us_48_only_gray_raster)[1L] <- "ele"

us_48_no_ocean_gray_raster <- filter(us_48_gray_raster, ele != 106)
us_48_only_no_ocean_gray_raster <- filter(us_48_only_gray_raster, ele != 106)

ref <- "data/maps"

save(us_48_gray_raster, file = file.path(ref, "/us_48_gray_raster.RData"))
save(us_48_gray_raster, file = "~/R/misc/maps/raster/us_48_gray_raster.RData")

save(us_48_no_ocean_gray_raster, file = file.path(ref, "us_48_no_ocean_gray_raster.RData"))
save(us_48_no_ocean_gray_raster, file = "~/R/misc/maps/raster/us_48_no_ocean_gray_raster.RData")

save(us_48_only_gray_raster, file = file.path(ref, "us_48_only_gray_raster.RData"))
save(us_48_only_gray_raster, file = "~/R/misc/maps/raster/us_48_only_gray_raster.RData")

save(us_48_only_no_ocean_gray_raster, file = file.path(ref, "us_48_only_no_ocean_gray_raster.RData"))
save(us_48_only_no_ocean_gray_raster, file = "~/R/misc/maps/raster/us_48_only_no_ocean_gray_raster.RData")

rm(us_48_gray_raster_spdf)
rm(ge)
