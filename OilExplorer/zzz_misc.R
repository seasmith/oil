library(purrr)

# Heat map ----------------------------------------------------------------

# Rig Count ~ County
rc_master %>%
 filter(between(PublishDate, max(PublishDate), max(PublishDate))) %>%
 group_by(County, `State/Province`, PublishDate) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 summarize(RigCount = mean(RigCount,  na.rm = TRUE)) %>%
 ungroup() %>%
 right_join(counties_2014_adjusted_simp, c("County" = "NAME", "State/Province" = "state_name")) %>% # previously an inner_join()
 ggplot() +
 geom_sf(aes(fill = RigCount), color = "black") +
 coord_sf(datum = NA) +
 scale_fill_viridis_c()

# Rig Count ~ Basin
c(isBasin, isNotBasin) %<-% {
 rc_master %>%
  filter(between(PublishDate, max(PublishDate), max(PublishDate))) %>%
  split(.$Basin == "Other")
}

isBasin <- isBasin %>%
 group_by(Basin, PublishDate) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 summarize(RigCount = mean(RigCount,  na.rm = TRUE)) %>%
 ungroup()

isNotBasin <- isNotBasin %>%
 group_by(`State/Province`, PublishDate) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 summarize(RigCount = mean(RigCount,  na.rm = TRUE)) %>%
 ungroup()

isBasin <- isBasin %>%
 right_join(sf_plays, c("Basin" = "Shale_play"))

isNotBasin <- isNotBasin %>%
 mutate(`State/Province` = str_to_title(`State/Province`)) %>%
 right_join(states_map_adjusted_simp, by = c("State/Province" = "NAME"))

basinData <- isNotBasin %>%
 rename(Basin = `State/Province`) %>%
 select(Basin, RigCount, geometry) %>%
 rbind({
  isBasin %>%
   select(Basin, RigCount, geometry)
 }) %>%
 st_sf()


 ggplot(basinData) +
 geom_sf(aes(fill = RigCount), color = "gray65") +
 coord_sf(datum = NA) +
 scale_fill_viridis_c()

# Rig Count Change
rigCountChangeCounty <- rc_master %>%
 filter(PublishDate == max(PublishDate) | PublishDate == as.Date("2018-03-29")) %>%
 group_by(County, `State/Province`, PublishDate) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 arrange(desc(PublishDate)) %>%
 mutate(PublishDate = if_else(PublishDate == min(PublishDate), "Start", "End")) %>%
 spread("PublishDate", "RigCount") %>%
 mutate_at(vars(Start, End), function(x) if_else(is.na(x), 0L, x)) %>%
 mutate(RigCountChange = End - Start) %>%
 ungroup()

c(neg_end, pos_end) %<-% range(rigCountChangeCounty$RigCountChange)

breaks <- function(x, n) {
 x <- abs(x)
 div <- x %/% n
 qot <- x %% n
 if (div == 0 & qot == 0) {
  0
 } else {
  if (qot == 0) div else div + 1
 }
}

pos_breaks <- breaks(pos_end, 5)
neg_breaks <- breaks(neg_end, 5)
breaks <- seq(-neg_breaks * 5, pos_breaks * 5, by = 5)

rigCountChangeCounty <- rigCountChangeCounty %>%
 mutate(RigCountChangeBreak = cut(RigCountChange, breaks = breaks))


colorRampPalette(c("steelblue", "white"))(pos_breaks + 1)[-(pos_breaks + 1)]
colorRampPalette(c("brown", "white"))(neg_breaks + 1)[-(neg_breaks + 1)]

pos_cols <- colorRampPalette(c("steelblue", "white"))( pos_end + 1 )[-(pos_end + 1)]
neg_cols <- colorRampPalette(c("brown", "white"))( neg_end + 1 )[-(neg_end + 1)]

pos_cols <- if (length(pos_cols) == 0) NULL else pos_cols
neg_cols <- if (length(neg_cols) == 0) NULL else neg_cols

rigCountChangeCounty %>%
 right_join(counties_2014_adjusted_simp, c("County" = "NAME", "State/Province" = "state_name")) %>% # previously an inner_join()
 ggplot() +
 geom_sf(aes(fill = factor(RigCountChange, levels = rev(seq(-neg_end, pos_end)))),
         color = "black") +
 coord_sf(datum = NA) +
 scale_fill_manual("Change", values = c(pos_cols, "white", rev(neg_cols)),
                   drop = FALSE, na.value = "gray10")
 

# Percent change from previous period

percentChange <- rc_master %>%
 filter(PublishDate == max(PublishDate) | PublishDate == as.Date("2014-04-04")) %>%
 group_by(County, `State/Province`, PublishDate) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 arrange(desc(PublishDate)) %>%
 mutate(PublishDate = if_else(PublishDate == min(PublishDate), "Start", "End")) %>%
 spread("PublishDate", "RigCount") %>%
 mutate_at(vars(Start, End), function(x) if_else(is.na(x), 0L, x)) %>%
 mutate(RigCountChange = (End - Start) / Start) %>%
 ungroup()

percentChange %>%
 right_join(counties_2014_adjusted_simp, c("County" = "NAME", "State/Province" = "state_name")) %>%
 ggplot() +
 geom_sf(aes(fill = RigCountChange)) +
 scale_fill_gradient2()


# Where oil, gas, (misc,) or both, are located ----------------------------

rig_type_each <- rc_master %>%
 filter(Location == "Land") %>%
 filter(PublishDate == max(PublishDate)) %>%
 group_by(`State/Province`, County, DrillFor) %>%
 summarize(n = n()) %>%
 ungroup()

rig_type_both <- rc_master %>%
 filter(Location == "Land") %>%
 filter(PublishDate == max(PublishDate)) %>%
 group_by(`State/Province`, County, DrillFor) %>%
 summarize(n = n()) %>%
 ungroup() %>%
 group_by(`State/Province`, County) %>%
 filter(n() > 1 & row_number() == 1) %>%
 ungroup() %>%
 mutate(DrillFor = "Both")

rig_type_each %>%
 anti_join(rig_type_both, c("State/Province", "County")) %>%
 bind_rows(rig_type_both) %>%
 left_join(counties_2014_adjusted_simp, c("County" = "NAME", "State/Province" = "state_name")) %>%
 st_as_sf() %>%
 ggplot() +
 geom_sf(data = counties_2014_adjusted_simp,
         color = "gray40", fill = "gray40") +
 geom_sf(aes(fill = factor(DrillFor, levels = c("Oil", "Gas", "Both"))), color = "black", size = 0.5) +
 coord_sf(datum = NA) +
 labs(title = paste0("US counties where an oil or gas rig is located"),
      subtitle = paste0("For the week of ", max(rc_master$PublishDate))) +
 scale_fill_brewer("Rig type:", palette = "Set1") +
 theme(axis.title = element_blank(),
       axis.text = element_blank(),
       legend.position = "top", legend.direction = "horizontal")


# Change in rig count for each type ---------------------------------------

init_calc <- rc_master %>%
 filter(PublishDate == max(PublishDate) | PublishDate == sort(unique(PublishDate), TRUE)[2]) %>%
 group_by(`State/Province`, `County`, `DrillFor`, PublishDate) %>%
 summarize(n = n()) %>%
 ungroup()

## Which locations are missing for one particular date
#  but present in the other
miss_loc <- init_calc %>%
 group_by(`State/Province`, County, DrillFor) %>%
 filter(n() != 2) %>%
 ungroup()

## Add missing, coerce missing `n` to 0, and summarize
summ_loc <- init_calc %>%
 split(.$PublishDate) %>%
 imap(~{
  y <- filter(miss_loc, PublishDate != .y)
  y <- mutate(y, n = 0)
  bind_rows(.x, y)
 }) %>%
 imap(~mutate(.x, PublishDate = as.Date(.y))) %>%
 bind_rows() %>%
 group_by(County, DrillFor, `State/Province`) %>%
 arrange(PublishDate) %>%
 mutate(change = n - lag(n, 1L)) %>%
 ungroup()

## Find if any are still unresolved
unresolved <- summ_loc %>%
 group_by(`State/Province`, County, DrillFor) %>%
 filter(n() != 2)

summ_loc %>%
 filter(PublishDate == max(PublishDate)) %>%
 inner_join(counties_2014_adjusted_simp, c("State/Province" = "state_name", "County" = "NAME")) %>%
 mutate(DrillFor = factor(DrillFor, levels = c("Oil", "Gas"))) %>%
 st_as_sf() %>%
 ggplot() +
 geom_sf(data = states_map_adjusted_simp, fill = "gray80", color = "gray90") +
 geom_sf(aes(fill = factor(sign(change)), color = factor(sign(change)))) +
 coord_sf(datum = NA) +
 scale_fill_manual(NULL, values = c("#a52a2a", "gray50", "#4682b4"), labels = c("Decrease", "No Change", "Increase")) +
 scale_color_manual(NULL, values = c("#a52a2a", "gray50", "#4682b4"), labels = c("Decrease", "No Change", "Increase")) +
 facet_wrap(~DrillFor, ncol = 2) +
 labs(title = paste0("Weekly change in US county oil and gas rig count"),
      subtitle = paste0(pretty_date(max(rc_master$PublishDate))),
      caption = paste0("Source: Baker Hughes")) +
 theme(strip.text = element_text(size = 14, margin = margin(0, 0, 0, 0, "pt")),
       legend.title = element_text(size = 12),
       legend.text = element_text(size = 11),
       legend.position = c(0.5, 1.1),
       legend.direction = "horizontal",
       panel.spacing = unit(0, "lines"),
       plot.caption = element_text(size = 10, color = "gray50", hjust = 0.9, vjust = 3),
       plot.margin = unit(c(0,0,0,0.1), "lines"))


# EIA Regions -------------------------------------------------------------

non_basin_counties <- counties_2014_adjusted_simp %>%
 mutate_at(vars(NAME), toupper) %>%
 mutate_at(vars(STATE_FIPS), as.integer) %>%
 anti_join(xl_regions, c("STATE" = "state", "NAME" = "county"))

basin_counties <- counties_2014_adjusted_simp %>%
 mutate_at(vars(NAME), toupper) %>%
 mutate_at(vars(STATE_FIPS), as.integer) %>%
 inner_join(xl_regions, c("STATE" = "state", "NAME" = "county")) %>%
 mutate(region = str_replace(region, " Region$", ""))

basin_counties_dissolved <- basin_counties %>%
 split(.$region) %>%
 map(st_union) %>%
 map(~st_sf(geometry = .)) %>%
 imap(~data.frame(.x, region = .y)) %>%
 reduce(rbind) %>%
 st_sf()

basin_counties_dissolved2 <- basin_counties_dissolved %>%
 st_cast("LINESTRING")

rc_presence_region <- rc_master %>%
 filter(Country == "UNITED STATES") %>%
 filter(PublishDate == max(PublishDate)) %>%
 distinct(County, `State/Province`) %>%
 left_join(basin_counties, c("County" = "NAME", "State/Province" = "state_name")) %>%
 ggplot() +
 geom_sf(data = filter(counties_2014_adjusted_simp, !grepl("AK|HI", STATE)), fill = "gray20", color = "gray10") +
 geom_sf(aes(fill = region), basin_counties, alpha = 0.3, color = "gray10") +
 geom_sf(aes(fill = region), color = "gray10") +
 geom_sf(aes(color = region), basin_counties_dissolved2) +
 scale_fill_brewer("Region", palette = "Accent") +
 scale_color_brewer("Region", palette = "Accent") +
 coord_sf(datum = NA) +
 guides(color = FALSE)

region_counties <- rc_presence_region +
 labs(title = "Rig Presence By Region And County",
      subtitle = "January 12, 2018",
      caption = "Source: EIA Drilling Productivity Report") +
 theme(legend.position = c(.99, 0.35),
       plot.caption = element_text(size = 10, color = "gray50",
                                   margin = margin(-0, 0, 0, 0, "lines")), plot.margin = unit(c(0.2,0,0,-2), "lines"))

region_counties



# BASIN MAP ---------------------------------------------------------------

stateMapLayout <- ggplot() +
 geom_sf(data = states_map_adjusted_simp, fill = "#00000000",color = nord_palettes$polarnight[4L]) +
 scale_color_gradient(low = nord_palettes$polarnight[1L],
                      high = nord_palettes$polarnight[4L]) +
 scale_x_continuous(expand = expand_scale()) +
 scale_y_continuous(expand = expand_scale()) +
 # guides(color = FALSE, alpha = FALSE, fill = FALSE) +
 theme_nord(16) +
 theme(axis.title = element_blank()) +
 theme(legend.position = c(0.85, 0.08),
       legend.justification = c("left", "bottom"),
       legend.box.just = "right",
       legend.text = element_text(color = "gray80"),
       plot.background = element_rect(fill = nord_palettes$polarnight[1L],
                                      color = nord_palettes$polarnight[1L]))

countyMapLayout <- ggplot() +
 geom_sf(data = counties_2014_adjusted_simp, fill = "#00000000",color = nord_palettes$polarnight[4L]) +
 scale_color_gradient(low = nord_palettes$polarnight[1L],
                      high = nord_palettes$polarnight[4L]) +
 scale_x_continuous(expand = expand_scale()) +
 scale_y_continuous(expand = expand_scale()) +
 # guides(color = FALSE, alpha = FALSE, fill = FALSE) +
 theme_nord(16) +
 theme(axis.title = element_blank()) +
 theme(legend.position = c(0.85, 0.08),
       legend.justification = c("left", "bottom"),
       legend.box.just = "right",
       legend.text = element_text(color = "gray80"),
       plot.background = element_rect(fill = nord_palettes$polarnight[1L],
                                      color = nord_palettes$polarnight[1L]))

totalsByPlay <- rc_master %>%
 filter(between(PublishDate, max(PublishDate), max(PublishDate))) %>%
 group_by(Basin, PublishDate) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 summarize(RigCount = mean(RigCount,  na.rm = TRUE)) %>%
 ungroup() %>%
 right_join(sf_plays, by = c("Basin" = "Shale_play")) %>%
 sf::st_sf()

stateMapLayout +
 geom_sf(aes(fill = RigCount), totalsByPlay, color = "black") +
 coord_sf(datum = NA) +
 scale_fill_viridis_c()

# countyMapLayout +
#  geom_sf(aes(fill = RigCount), totalsByPlay, color = "black") +
#  coord_sf(datum = NA) +
#  scale_fill_viridis_c()
