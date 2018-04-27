load("data/maps/states_map_adjusted_simp.RData")

new_crs <- st_crs(states_map_adjusted_simp)

gom <- st_read("data/maps/gom_protrac/protrac.shp") %>% st_transform(new_crs)
pac <- st_read("data/maps/pac_protrac_clipped/PC_PROT_CLIP.shp") %>% st_transform(new_crs)
pac_clean <- st_read("data/maps/pac_protrac_clipped/PC_PROT_CLIP.shp")
ak <- st_read("data/maps/ak_protrac_clipped/AK_PROTCLP.shp") %>% st_transform(new_crs)

# US states map must overlay pac protractions, or the protractions must be 'clipped'

states_map_adjusted_simp %>%
 ggplot() +
 geom_sf(data = pac) +
 geom_sf() +
 geom_sf(data = gom) #+
 # geom_sf(data = ak)
