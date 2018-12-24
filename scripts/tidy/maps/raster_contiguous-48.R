library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(raster)

load("data/maps/states_sim.RData")
states_sim <- st_union(states_sim %>%
                        filter(! state %in% c("ALASKA", "HAWAII")))

ge <- list.files("~/R/misc/maps/raster", recursive = TRUE, full.names = TRUE) %>%
 grep("GRAY_LR_SR_W\\.tif", ., value = TRUE) %>%
 raster()

# Set CRS equal
this_crs <- st_crs(states_sim)
projection(ge) <- this_crs$proj4string
states_sim_sp  <- as(states_sim, "Spatial")

us_48_raster <- crop(ge, states_sim_sp)
us_48_raster <- as(us_48_raster, "SpatialPixelsDataFrame")
us_48_only   <- over(us_48_raster, states_sim_sp, fn = NULL)
us_48_raster <- us_48_raster[!is.na(us_48_only), ]


us_48_raster <- as.data.frame(us_48_raster)
names(us_48_raster)[1L] <- "ele"

if (! dir.exists("data/maps/rasters")) dir.create("data/maps/rasters")

save(us_48_raster, file = "data/maps/rasters/us_48_raster.RData")
save(us_48_raster, file = "~/R/misc/maps/raster/us_48_raster.RData")

rm(list = ls())
