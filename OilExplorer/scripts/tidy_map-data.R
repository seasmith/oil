# This is a once-off script for cleaning up the map datasets.
# This will prevent having to perform these steps upon starting
# each shiny instance.

library(sf)
packrat::extlib("rmapshaper")
packrat::extlib("geojsonio")

load("data/counties_2014_adjusted_simp.RData")
load("data/states_map_adjusted_simp.RData")
load("data/sf_plays.RData")

# Counties
counties_crs <- sf::st_crs(counties_2014_adjusted_simp)

counties_2014_adjusted_simp <- counties_2014_adjusted_simp %>%
 filter(!(STATE == "PR" | STATE == "VI")) %>%
 filter(!is.na(NAME)) %>%
 left_join(blg::state_lookup, c("STATE" = "state_abb")) %>%
 mutate_at(vars(state_name, NAME), toupper) %>%
 st_sf()

st_crs(counties_2014_adjusted_simp) <- counties_crs

# States
states_crs <- sf::st_crs(states_map_adjusted_simp)

states_map_adjusted_simp <- states_map_adjusted_simp %>%
 filter(!(STATE_ABBR %in% c("PR", "VI")) & TYPE == "Land") %>%
 select(-(STATE_FIPS:PRIM_MILES)) %>%
 st_sf()

st_crs(states_map_adjusted_simp) <- states_crs

# Basins
sf_plays <- sf_plays %>% ms_simplify(keep_shapes = TRUE)

save(counties_2014_adjusted_simp, file = "~/R/oil/OilExplorer/data/counties_2014_adjusted_simp.RData")
save(states_map_adjusted_simp, file = "~/R/oil/OilExplorer/data/states_map_adjusted_simp.RData")
save(sf_plays, file = "~/R/oil/OilExplorer/data/sf_plays.RData")
