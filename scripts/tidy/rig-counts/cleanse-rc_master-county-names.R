library(dplyr)
library(stringr)

load("~/R/oil/data/rig_counts/rc_master.RData")

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
