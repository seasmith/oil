
# PURPOSE -----------------------------------------------------------------

# Create rasters that match the shifted state and county maps.



# PACKAGES ----------------------------------------------------------------

library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(raster)
library(maptools)
library(rmapshaper)


# 50 STATE RASTER CROP ----------------------------------------------------

# Load simplified 50 states map, offshore map, raster, and Alaska offshore
load("data/maps/us/states_sim.RData")
load("data/maps/rasters/us_48_raster.RData")
ak_off <- st_read("data/maps/offshore/ak_protrac/AK_PROTLMT.shp") %>%
 dplyr::select(name = PROT_NAME) %>%
 ms_simplify()

# Extract ALASKA and HAWAII
ak <- filter(states_sim, state == "ALASKA")
hi <- filter(states_sim, state == "HAWAII")

# Load gray-filled raster
ge <- list.files("~/R/misc/maps/raster", recursive = TRUE, full.names = TRUE) %>%
 grep("GRAY_LR_SR_W\\.tif", ., value = TRUE) %>%
 raster()

# Set raster CRS equal to simplified 50 states map CRS
projection(ge) <- st_crs(states_sim)$proj4string

# Convert sf to sp (need an sp object to crop the raster)
ak_sp <- as(ak, "Spatial")
hi_sp <- as(hi, "Spatial")


# Crop raster and convert to pixels (returns a raster bounding box(?))
ak_sp <- crop(ge, ak_sp)
hi_sp <- crop(ge, hi_sp)

ak_sp <- as(ak_sp, "SpatialPixelsDataFrame")
hi_sp <- as(hi_sp, "SpatialPixelsDataFrame")

# Convert to sf
ak_sf <- st_as_sf(ak_sp)
hi_sf <- st_as_sf(hi_sp)

# Get only land-based points
ak_sf <- st_intersection(st_transform(ak_sf, st_crs(2163)),
                         st_transform(ak,    st_crs(2163)))
hi_sf <- st_intersection(st_transform(hi_sf, st_crs(2163)),
                         st_transform(hi,    st_crs(2163)))


# SHIFT ALASKA AND HAWAII -------------------------------------------------

####
# Set up shifting functions
# Handler
fixup <- function(pgon, fix_params){
 
 orig_proj4 <- proj4string(pgon)
 pgon = fix1(pgon , fix_params)
 proj4string(pgon) <- orig_proj4
 
 return(pgon)
 
}

# Workhorse
fix1 <- function(object,params){
 
 r = params[1]
 scale = params[2]
 shift = params[3:4]
 
 object = elide(object,rotate=r)
 size = max(apply(bbox(object),1,diff))/scale
 object = elide(object,scale=size)
 object = elide(object,shift=shift)
 
 return(object)
 
}
####

####
# offshore needs to be spatial points (sp cannot handle
# geometry collections).

# Need an equal-area projection to properly move the states
# ALASKA
# Clip entire offshore Alaska and state, move, and
# then re-clip after the move.
ak_sp <- ak_sf %>%
 rbind(
  ak_off %>%
   st_transform(st_crs(2163)) %>%
   mutate(GRAY_LR_SR_W = NA_real_) %>%
   rename(state = name) %>%
   st_cast("POINT")
 ) %>%
 as("Spatial") %>%
 fixup(c(-35,2.2,-2200000,-2900000)) %>%
 spTransform(CRS(st_crs(102003)$proj4string))

# HAWAII
hi_sp <- hi_sf %>%
 as("Spatial") %>%
 fixup(c(-35, 1, 5500000, -1400000)) %>%
 spTransform(CRS(st_crs(102003)$proj4string))
####


####
# Conver to data frames and keep only needed three columns
ak_df <- as.data.frame(ak_sp)
ak_df <- ak_df[!is.na(ak_df$GRAY_LR_SR_W), ]
hi_df <- as.data.frame(hi_sp)

names(ak_df)[1L] <- "ele"
names(hi_df)[1L] <- "ele"

ak_df <- ak_df[, c("ele", "x", "y")]
hi_df <- hi_df[, c("ele", "x", "y")]
####


####
# Add ALASKA and HAWAII back to other 48 states
# Remove ALASKA and HAWAII from 50 state raster

us48 <- SpatialPixelsDataFrame(us_48_raster[, c("x", "y")],
                               us_48_raster[, c("ele"), drop = FALSE],
                               proj4string = CRS(st_crs(4326)$proj4string))
us48 <- spTransform(us48, CRSobj = CRS(st_crs(102003)$proj4string))
us48 <- as.data.frame(us48)

us_50_raster <- us48 %>%
 rbind(ak_df, hi_df)
####


# Remove ele == 106
us_50_raster <- filter(us_50_raster, ele != 106)

load("data/maps/us/states_sim_shift.RData")
load("data/maps/us/counties_sim_shift.RData")

us_50_raster <- st_intersection(
 st_as_sf(us_50_raster, coords = c("x", "y"), crs = st_crs(102003)),
 st_transform(st_union(states_sim_shift), st_crs(102003))
)

us_50_raster <- us_50_raster %>%
 st_coordinates() %>%
 {cbind(as.data.frame(us_50_raster)$ele, .)} %>%
 as.data.frame()

names(us_50_raster) <- c("ele", "x", "y")

# WRITE TO FILE -----------------------------------------------------------

save(us_50_raster, file = "data/maps/rasters/us_50_raster.RData")



# PLOT --------------------------------------------------------------------


us_50_raster %>%
 ggplot() +
 geom_point(aes(x, y, color = ele), size = 0.01) +
 geom_sf(data = states_sim_shift, fill = "#00000000", color = "black") +
 # geom_sf(data = offshore) +
 coord_sf(crs = st_crs(102003), datum = NA) +
 scale_color_gradient(low = "gray10", high = "gray90", guide = FALSE) +
 theme_void() +
 theme(plot.background = element_rect(fill = "gray10"))
