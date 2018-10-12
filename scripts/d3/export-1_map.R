library(tidyverse)
library(sf)
library(geojsonio)

load("data/maps/offshore/offshore.RData")
load("data/maps/us/counties_map.RData")
load("data/rig_counts/rc_master.RData")


# PREP RIG COUNT NAMES ----------------------------------------------------

rc_master <- rc_master %>%
 filter(Country == "UNITED STATES")

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

# ONSHORE CONFLICTS
# 'Broomfield' is not all caps (others could be in future)
rc_master <- rc_master %>%
 mutate_at(vars(County), str_to_upper)


# RESOLVE OFFSHORE NAMES
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

# RESOLVE INLAND WATERS NAMES
rc_master <- rc_master %>%
 filter(Location == "Inland Waters") %>%
 mutate(Location = case_when(
  County == "EUGENE ISLAND"       ~ "Offshore",
  County == "VERMILION OFFSHORE"  ~ "Offshore",
  County == "BRAZOS OFFSHORE"     ~ "Offshore",
  County == "GRAND ISLE OFFSHORE" ~ "Offshore",
  County == "GALVESTON OFFSHORE"  ~ "Offshore",
  TRUE ~ Location
 )) %>%
 mutate(County = case_when(
  County == "VERMILION OFFSHORE"  ~ "VERMILION",
  County == "BRAZOS OFFSHORE"     ~ "BRAZOS",
  County == "GRAND ISLE OFFSHORE" ~ "GRAND ISLE",
  County == "GALVESTON OFFSHORE"  ~ "GALVESTON",
  TRUE ~ County
 )) %>%
 bind_rows(
  rc_master %>%
   filter(Location != "Inland Waters")
 )


# JOIN OFFSHORE AND RIG COUNT NAMES ---------------------------------------

us_off <- offshore %>%
 left_join(rc_master %>%
            filter(Country == "UNITED STATES" & Location == "Offshore") %>%
            distinct(County),
           by = c("name" = "County"))

old_crs <- st_crs(us_off)
us_off  <- st_transform(us_off, st_crs(102003))
us_off  <- st_difference(us_off, 
                         counties_map %>%
                          st_transform(st_crs(102003)) %>%
                          st_union() %>%
                          st_buffer(10000))
us_off <- st_transform(us_off, old_crs)
us_off <- select(us_off, name, state = region)
us_off <- mutate_at(us_off, vars(state), str_to_upper)
us_on  <- select(counties_map, name = NAME, state = STATE_NAME)

# Combine
us_oil_map <- us_off %>%
 rbind(us_on)

# New column for matching in JA
us_oil_map <- unite(us_oil_map, location, name, state, remove = FALSE)
us_oil_map <- unite(us_oil_map, display_name, name, state, sep = ", ", remove = FALSE)
us_oil_map <- mutate(us_oil_map, location = str_replace(location, " ", "-"))

# Change display_name to proper case
us_oil_map <- us_oil_map %>%
 mutate_at(vars(display_name), str_to_title) %>%
 mutate_at(vars(display_name), function(x) case_when(
  grepl("Gom$", x) ~ str_replace(x, "Gom$", "GOM"),
  grepl("Pac$", x) ~ str_replace(x, "Pac$", "PAC"),
  grepl("Ak", x)   ~ str_replace(x, "Ak$", "OAK"),
  TRUE             ~ x
 ))

# Transform
# us_oil_map <- st_transform(us_oil_map, st_crs(102003))

# Create new directory (if needed) and save
if (! dir.exists("data/maps/d3")) dir.create("data/maps/d3")
geojson_write(us_oil_map, file = "data/maps/d3/counties.json")
geojson_write(us_oil_map %>% group_by(state) %>% summarize(), file = "data/maps/d3/states.json")
save(us_off, file = "data/maps/d3/us_off.RData")
save(us_on, file = "data/maps/d3/us_on.RData")
# geojson_write(us_oil_map, file = "data/maps/d3/us_oil_map2.json")


