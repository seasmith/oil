
# PURPOSE -----------------------------------------------------------------

# There are two main kinds o maps used in the rig count
# updates: counties and states maps.

# This file creats the base maps to build upon.

# These maps cannot have Alaska and Hawaii 'shifted' to sit
# below Arizone-Texas (cutting down on the bounding box).

# This is due to the fact that shifting the Alaskan offshore
# areas, independent of the state/counties of Alaska, causes
# misalignment between the two.

# This procedure is instead performed in 'protractions.R'.



# PACKAGES ----------------------------------------------------------------

library(sf)
library(sp)
library(raster)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(rmapshaper)



# IMPORT ------------------------------------------------------------------

# Use only county (to simplify this script)
# Need to shift Alaska to a more manageable location
counties_raw <- st_read("~/R/mymaps/data/cb_2017_us_county_20m.shp") %>%
 filter(STATEFP != "72")  # Remove Puerto Rico

# Baker Hughes' county names are all caps
counties_raw <- counties_raw %>% mutate_at(vars(NAME), str_to_upper)

# Add full state name
if (!file.exists("data/maps/state_fips.txt")) download.file("https://www2.census.gov/geo/docs/reference/state.txt", "data/maps/state_fips.txt")
fips <- read_delim("data/maps/state_fips.txt", delim = "|")

counties_raw <- counties_raw %>%
 mutate_at(vars(STATEFP), as.character) %>%
 inner_join(fips %>%
             select(STATE, STATE_NAME) %>%
             mutate_at(vars(STATE_NAME), str_to_upper), by = c("STATEFP" = "STATE"))


# Select and rename only the needed columns
counties_raw <- counties_raw %>%
 select(state = STATE_NAME, county = NAME)

# Raw states map
states_raw <- counties_raw %>%
 group_by(state) %>%
 summarize()



# SIMPLIFY ----------------------------------------------------------------

counties_sim <- counties_raw %>%
 ms_simplify()

states_sim <- counties_sim %>%
 group_by(state) %>%
 summarize()



# SAVE --------------------------------------------------------------------

save(counties_raw, file = "data/maps/us/counties_raw.RData")
save(counties_sim, file = "data/maps/us/counties_sim.RData")
save(states_raw, file = "data/maps/us/states_raw.RData")
save(states_sim, file = "data/maps/us/states_sim.RData")
