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

us_off <- select(us_off, name, state = region)
us_off <- mutate_at(us_off, vars(state), str_to_upper)
us_on  <- select(counties_map, name = NAME, state = STATE_NAME)

# Combine
us_oil_map <- us_off %>%
 rbind(us_on)

# New column for matching in JA
us_oil_map <- unite(us_oil_map, location, name, state, remove = FALSE)
us_oil_map <- unite(us_oil_map, display_name, name, state, sep = ", ")
us_oil_map <- mutate(us_oil_map, location = str_replace(location, " ", "-"))

# Transform
# us_oil_map <- st_transform(us_oil_map, st_crs(102003))

# Create new directory (if needed) and save
if (! dir.exists("data/maps/d3")) dir.create("data/maps/d3")
topojson_write(us_oil_map, object_name = "counties", group = "location", file = "data/maps/d3/us_oil_map.json")
geojson_write(us_oil_map, file = "data/maps/d3/us_oil_map2.json")


# DATA FOR D3 -------------------------------------------------------------

current_week <- max(rc_master$PublishDate, na.rm = TRUE)

# Offshore counts
off_data <- rc_master %>%
 filter(PublishDate == max(PublishDate) &
         Location == "Offshore") %>%
 group_by(County, `State/Province`) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 ungroup() %>%
 select(name = County, RigCount)

off_data <- off_data %>%
 inner_join(us_off %>% st_set_geometry(NULL), by = "name")

# Onshore counts
on_data <- rc_master %>%
 filter(PublishDate == max(PublishDate) &
         Location != "Offshore") %>%
 group_by(County, `State/Province`) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 ungroup() %>%
 rename(name = County, state = `State/Province`)

# Combine
rig_counts <- bind_rows(off_data, on_data)

# New column for matching in JA
rig_counts <- unite(rig_counts, location, name, state, remove = FALSE)
rig_counts <- unite(rig_counts, display_name, name, state, sep = ", ")

# Rename for JS
rig_counts <- rename(rig_counts, n = RigCount)

# Fill spaces with '-'
rig_counts <- rig_counts %>% mutate(location = str_replace(location, " ", "-"))

# Create new directory (if needed) and save
if (! dir.exists("data/rig_counts/d3")) dir.create("data/rig_counts/d3")
write_csv(rig_counts, "data/rig_counts/d3/rig_counts.csv")
