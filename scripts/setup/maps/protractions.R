
# PURPOSE -----------------------------------------------------------------

# Create offshore maps that resolve name conflicts and
# look good.

# Alaska offshore protractions must be shifted with
# the state of Alaska.



# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(sp)
library(maptools)
library(rmapshaper)



# NOTES -------------------------------------------------------------------

# Alaska:
#  * 'NORTH SLOPE' is not searchable
# Conflicts resolved later in script.

# California:
#  * 'ORANGE' and 'SANTA BARBARA' are not searchable
# Conflicts resolved later in script.

# Florida:
#  * All searchable in BOEM data

# Alabama:
#  * All searchable in BOEM data

# Texas:
#  * All searchable
#  * Union the following BOEM polygons:

gom_union <- c("High Island", "Galveston", "Mustang Island", "Brazos")

#  * Dissolving these will help make map more aesthetic:

gom_union <- c(gom_union, "South Padre", "North Padre", "Sabine Pass Area",
               "Chandeleur Area")

# Louisiana:
#  * All searchable
#  * Union the following BOEM polygons:

gom_union <- c(gom_union, "Main Pass", "Viosca Knoll", "South Timbalier",
               "Ship Shoal", "South Marsh Island", "South Pass", "Eugene Island",
               "West Cameron", "East Cameron", "West Delta", "Vermilion",
               "Grand Isle", "Ewing Bank")


#  * Too small (merge with):
#    * 'Breton Sound' ('Main Pass')
#    * 'Bay Marchand' ('South Timbalier')
#    * 'South Pelto' ('South Timbalier')
# Conflicts resolved later in script

# LOAD --------------------------------------------------------------------

load("data/maps/us/counties_sim.RData")
states_sim <- counties_sim %>%
 group_by(state) %>%
 summarize() %>%
 ungroup()

# Pacific
pac_orig <- st_read("data/maps/offshore/pac_protrac/PC_PROTLMT.shp")
pac <- pac_orig %>%
 select(name = PROT_NAME) %>%
 ms_simplify()
  

# Gulf of Mexico
gom_orig <- st_read("data/maps/offshore/gom_protrac/protrac.shp")
gom <- gom_orig %>%
 st_transform(st_crs(pac)) %>%
 select(name = PROT_NAME) %>%
 ms_simplify()


# Alaska
# ak_orig <- st_read("data/maps/offshore/ak_protrac_clipped/AK_PROTCLP.shp")
ak_orig <- st_read("data/maps/offshore/ak_protrac/AK_PROTLMT.shp")
ak <- ak_orig %>%
 select(name = PROT_NAME) %>%
 ms_simplify()



# DISSOLVE ALASKA OFFSHORE PROTRACTIONS -----------------------------------

# Clip Alaska offshore to Alaska
ak <- ak %>%
 rbind(states_sim %>% filter(state == "ALASKA") %>% rename(name = state)) %>%
 split(.$name == "ALASKA") %>%
 {st_difference(.[["FALSE"]], st_union(.[["TRUE"]]))}

ak <- ak %>%
 st_centroid() %>%
 st_coordinates() %>%
 cbind(ak, .)

north_slope <- ak %>%
 filter(Y > 68) %>%
 st_union() %>%
 st_sf() %>%
 mutate(name = "NORTH SLOPE",
        region = "ak")

kenai <- ak %>%
 filter(Y < 68) %>%
 filter(X > -158) %>%
 st_union() %>%
 st_sf() %>%
 mutate(name = "KENAI",
        region = "ak")

other_ak <- ak %>%
 filter(Y < 68) %>%
 filter(X < -158) %>%
 st_union() %>%
 st_sf() %>%
 mutate(name = "OTHER ALASKA",
        region = "ak")

ak <- list(north_slope, kenai, other_ak) %>%
 reduce(rbind)

# SHIFT ALASKA AND HAWAII (STATE LEVEL) -----------------------------------

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



# SHIFT ALASKA AND HAWAII (COUNTY LEVEL) ----------------------------------

# COUNTIES
ak <- counties_sim %>%
 filter(state == "ALASKA") %>%
 mutate(type = "county") %>%
 rename(name = state) %>%
 list(
  ak %>%
   rename(county = region) %>%
   mutate(type = "protraction")
 ) %>%
 reduce(rbind) %>%
 as("Spatial") %>%
 spTransform(CRS("+init=epsg:2163")) %>%
 fixup(c(-35,2.2,-2200000,-2900000)) %>%
 spTransform(CRS("+init=epsg:4269")) %>%
 st_as_sf()

hi <- counties_sim %>%
 filter(state == "HAWAII") %>%
 as("Spatial") %>%
 spTransform(CRS("+init=epsg:2163")) %>%
 fixup(c(-35,1,5500000,-1400000)) %>%
 spTransform(CRS("+init=epsg:4269")) %>%
 st_as_sf()

# Capture the new CRS featuring towgs84=0,0,0,0,0,0,0 
map_crs <- st_crs(ak)

# Select Alaskan counties (removing offshore protractions)
ak_counties <- ak %>%
 as.data.frame() %>%
 select(-type) %>%
 inner_join(counties_sim %>%
             as.data.frame() %>%
             select(-geometry), by = c("name" = "state", "county")) %>%
 st_as_sf(crs = map_crs)

# Remove counties from ak
ak <- ak %>%
 filter(type != "county") %>%
 select(name, region = county, geometry)

# Merge Alaska (02) and Hawaii (15) into county map
counties_sim_shift <- counties_sim %>%
 filter(!(state == "ALASKA" | state == "HAWAII")) %>%
 st_transform(st_crs(map_crs)) %>%
 list(
  ak_counties %>% rename(state = name),
  hi
  ) %>%
 reduce(rbind)

# SHIFT ALASKA AND HAWAII (STATE LEVEL) -----------------------------------

states_sim_shift <- counties_sim_shift %>%
 group_by(state) %>%
 summarize()

# PRE OFFSHORE ------------------------------------------------------------

# Clip Alaska offshore to Alaska
# ak <- ak %>%
#  split(.$type == "county") %>%
#  {st_difference(.[["FALSE"]], st_union(.[["TRUE"]]))} %>%
#  select(name)

# Clip Pacific offshore to US west coast

pac <- pac %>%
 st_set_crs(map_crs) %>%
 list(
  states_sim_shift %>%
   filter(state %in% c("CALIFORNIA", "OREGON", "WASHINGTON")) %>%
   st_union()
  ) %>% 
  {st_difference(.[[1]], .[[2]])}

# Set 'region' identifier
pac$region <- "pac"
# ak$region  <- "ak"
gom$region <- "gom"


# Dissolve ambiguous offshore protraction names
# Set CRS to be the same across all maps.
gom <- st_transform(gom, st_crs(ak))

gom_dissolve <- data.frame()

for (i in seq_along(gom_union)) {
 
 if (i == 1) {
  gom_dissolve <- gom %>%
   filter(str_detect(name, gom_union[i])) %>%
   mutate(simp_name = gom_union[i]) %>%
   st_union() %>%
   st_sf() %>%
   mutate(name = gom_union[i])
 } else {
  gom_dissolve <- gom %>%
   filter(str_detect(name, gom_union[i])) %>%
   mutate(simp_name = gom_union[i]) %>%
   st_union() %>%
   st_sf() %>%
   mutate(name = gom_union[i]) %>%
   list(gom_dissolve, .) %>%
   reduce(rbind)
 }
 
}

gom_dissolve <- mutate(gom_dissolve, region = "gom")

gom <- gom %>%
 filter(!grepl(paste0(gom_union, collapse = "|"), name)) %>%
 list(gom_dissolve) %>%
 reduce(rbind)



# COMBINE MAPS ------------------------------------------------------------

# Pacific needs to have 'towgs84' param to be compatible with other CRS's proj4string
pac <- st_transform(pac, st_crs(ak))

# Bind offshore blocks into one map
offshore <- pac %>%
 list(ak, gom) %>%
 reduce(rbind)



# RESOLVE NAME CONFLICTS --------------------------------------------------

load('data/rig_counts/rc_master.RData')

# COUNTIES

# Alaska:
#  * Sagavanirktok == North Slope
#  * Tyonek == Kenai
#  * Umiat == North Slope

rc_master <- rc_master %>%
 filter(Location == "Land") %>%
 mutate(County =
         case_when(
          County == "SAGAVANIRKTOK" ~ "NORTH SLOPE",
          County == "Umiat"         ~ "NORTH SLOPE",
          County == "TYONEK"        ~ "KENAI PENINSULA",
          TRUE                      ~ County
         )) %>%
 bind_rows(rc_master %>% filter(Location != "Land"))


# Should return 4:
#  * 'Broomfield' in rc_master
#  * 'DE WITT' in counties_sim
#  * 'LAPORTE' in counties_sim
#  * Two cases of 'LASALLE' in counties_sim (Illinois and Louisiana)
rc_master %>%
 filter(Location == "Land") %>%
 filter(Country == "UNITED STATES") %>%
 group_by(`State/Province`, County) %>%
 summarize(n = n()) %>%
 ungroup() %>%
 anti_join(counties_sim_shift, by = c("County" = "county", "State/Province" = "state"))


# ONSHORE CONFLICTS
# 'Broomfield' is not all caps (others could be in future)
rc_master <- rc_master %>%
 mutate_at(vars(County), str_to_upper)

# 'DEWITT' vs 'DE WITT' (Texas)
counties_sim_shift <- counties_sim_shift %>%
 mutate(county = case_when(
  county == "DEWITT" ~ "DE WITT",
  county == "LASALLE" & state == "ILLINOIS"  ~ "LA SALLE",
  county == "LASALLE" & state == "LOUISIANA" ~ "LA SALLE",
  county == "LAPORTE"     ~ "LA PORTE",
  TRUE ~ county
 ))

# Should return zero rows
rc_master %>%
 filter(Location == "Land") %>%
 filter(Country == "UNITED STATES") %>%
 group_by(`State/Province`, County) %>%
 summarize(n = n()) %>%
 ungroup() %>%
 anti_join(counties_sim_shift, by = c("County" = "county", "State/Province" = "state"))

# RESOLVE OFFSHORE NAMES --------------------------------------------------

rc_master <- rc_master %>%
 filter(Location == "Offshore") %>%
 mutate(County = case_when(
  County == "NORTH SLOPE OFFSHORE" ~ "NORTH SLOPE",
  County == "KENAI PENINSULA" ~ "KENAI",
  County == "ORANGE" ~ "SANTA MARIA",
  County == "SANTA BARBARA" ~ "SANTA MARIA",
  County == "BAY MARCHAND" ~ "BAY MARCHAND AREA",
  County == "BRETON SOUND" ~ "BRETON SOUND AREA",
  County == "EAST CAMERON SOUTH" ~ "EAST CAMERON",
  County == "EUGENE ISLAND SOUTH" ~ "EUGENE ISLAND",
  County == "MAIN PASS SOUTH AND EAST" ~ "MAIN PASS",
  County == "SHIP SHOAL SOUTH" ~ "SHIP SHOAL",
  County == "SOUTH MARSH ISLAND NORTH" ~ "SOUTH MARSH ISLAND",
  County == "SOUTH MARSH ISLAND SOUTH" ~ "SOUTH MARSH ISLAND",
  County == "SOUTH PELTO" ~ "SOUTH PELTO AREA",
  County == "VERMILION OFFSHORE" ~ "VERMILION",
  County == "VERMILION SOUTH" ~ "VERMILION",
  County == "WEST CAMERON SOUTH" ~ "WEST CAMERON",
  County == "WEST CAMERON WEST" ~ "WEST CAMERON",
  County == "WEST DELTA SOUTH" ~ "WEST DELTA",
  County == "HIGH ISLAND SOUTH" ~ "HIGH ISLAND",
  County == "MATAGORDA ISLAND" ~ "MATAGORDA ISLAND AREA",
  TRUE ~ County
 )) %>%
 bind_rows(
  rc_master %>%
   filter(Location != "Offshore")
 )


offshore <- offshore %>%
 mutate_at(vars(region), as.character) %>%
 mutate_at(vars(name), str_to_upper)

# Should return 0 rows
rc_master %>%
 filter(Location == "Offshore") %>%
 filter(Country == "UNITED STATES") %>%
 group_by(`State/Province`, County) %>%
 summarize(n = n()) %>%
 ungroup() %>%
 anti_join(offshore, by = c("County" = "name"))



# CAST STATES AND COUNTIES TO MULTILINESTRINGS ----------------------------

states_lines <- states_sim_shift %>%
 st_cast("MULTILINESTRING") %>%
 mutate(RigCount = NA)



# WRITE TO FILE -----------------------------------------------------------

save(offshore, file = "data/maps/offshore/offshore.RData")
save(states_lines, file = "data/maps/us/states_lines.RData")
save(counties_sim_shift, file = "data/maps/us/counties_sim_shift.RData")
save(states_sim_shift, file = "data/maps/us/states_sim_shift.RData")
