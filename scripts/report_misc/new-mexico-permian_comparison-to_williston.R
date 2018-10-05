
# DEPENDENCIES ------------------------------------------------------------
library(tidyverse)
library(sf)
library(magick)
library(extrafont); loadfonts("win", quiet = TRUE)

pretty_date <- function(n) format(n, "%B %d, %Y")



# LOAD DATA ---------------------------------------------------------------
load("data/rig_counts/rc_master.RData")
load("data/maps/us/counties_map.RData")


# TIDY --------------------------------------------------------------------
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



# PLOTS -------------------------------------------------------------------
# ALL BASINS --------------------------------------------------------------
rc_master %>%
 group_by(PublishDate, Basin) %>%
 summarize(n = sum(RigCount)) %>%
 ggplot() +
 geom_line(aes(PublishDate, n, color = Basin)) +
 scale_x_date(NULL) +
 scale_y_continuous(NULL) +
 labs(title = "The Resurgent Permian",
      subtitle = "Rig Count",
      caption = "Source: Baker Hughes") +
 theme_classic() +
 theme(text = element_text(color = "white", family = "Lato"), 
       axis.line = element_line(color = "gray80"), 
       axis.text = element_text(color = "gray80"),
       axis.ticks = element_line(color = "gray80"),
       panel.background = element_rect(fill = "gray20", color = "gray20"),
       plot.caption = element_text(color = "gray50", size = 10),
       plot.background = element_rect(fill = "gray20", color = "gray20"),
       legend.background = element_rect(fill = "gray20"))


# PERMIAN VS ALL OTHER MAJOR BASINS ---------------------------------------
rc_master %>%
 filter(Basin != "Other") %>%
 group_by(PublishDate, Is_Permian = Basin == "Permian") %>%
 summarize(n = sum(RigCount)) %>%
 ggplot() +
 geom_line(aes(PublishDate, n, color = Is_Permian)) +
 scale_x_date(NULL) +
 scale_y_continuous(NULL) +
 labs(title = "The Resurgent Permian",
      subtitle = "Rig Count",
      caption = "Source: Baker Hughes") +
 theme_classic() +
 theme(text = element_text(color = "white", family = "Lato"), 
       axis.line = element_line(color = "gray80"), 
       axis.text = element_text(color = "gray80"),
       axis.ticks = element_line(color = "gray80"),
       panel.background = element_rect(fill = "gray20", color = "gray20"),
       plot.caption = element_text(color = "gray50", size = 10),
       plot.background = element_rect(fill = "gray20", color = "gray20"),
       legend.background = element_rect(fill = "gray20"))


split_in_3 <- rc_master %>%
 filter() %>%
 group_by(PublishDate, Basin) %>%
 summarize(n = sum(RigCount)) %>%
 ungroup() %>%
 {
  list(Permian = .[.$Basin == "Permian", ],
       Other   = .[.$Basin == "Other", ],
       Majors  = .[.$Basin != "Permian" & .$Basin != "Other", ])
 }

ggplot(split_in_3$Permian, aes(PublishDate, n)) +
 geom_line(color = pals::brewer.greens(7)[7]) +
 geom_line(data = split_in_3$Other, color = pals::brewer.greens(7)[3]) +
 geom_line(aes(group = Basin), data = split_in_3$Majors, color = pals::brewer.greens(7)[1]) +
 theme_classic() +
 theme(text = element_text(color = "white", family = "Lato"), 
       axis.line = element_line(color = "gray80"), 
       axis.text = element_text(color = "gray80"),
       axis.ticks = element_line(color = "gray80"),
       panel.background = element_rect(fill = "gray20", color = "gray20"),
       plot.caption = element_text(color = "gray50", size = 10),
       plot.background = element_rect(fill = "gray20", color = "gray20"),
       legend.background = element_rect(fill = "gray20"))



# PERMIAN: TEXAS VS NEW MEXICO --------------------------------------------
## Texas vs New Mexico
(tex_nm <- rc_master %>%
  filter(Basin == "Permian") %>%
  group_by(PublishDate, `State/Province`) %>%
  summarize(n = sum(RigCount)) %>%
  ggplot() +
  geom_line(aes(PublishDate, n, color = `State/Province`)) +
  scale_x_date(NULL) +
  scale_y_continuous(NULL) +
  scale_color_manual(values = c("yellow", blg::clrs$orange), guide = FALSE) +
  labs(title = "The Resurgent Permian",
       subtitle = "Rig Count",
       caption = "Source: Baker Hughes") +
  theme_classic() +
  theme(text = element_text(color = "white", family = "Lato"), 
        axis.line = element_line(color = "gray80"), 
        axis.text = element_text(color = "gray80"),
        axis.ticks = element_line(color = "gray80"),
        panel.background = element_rect(fill = "gray20", color = "gray20"),
        plot.caption = element_text(color = "gray50", size = 10),
        plot.background = element_rect(fill = "gray20", color = "gray20"),
        legend.direction = "horizontal",
        legend.position = c(0.55, 1.04)))



# NEW MEXICO PERMIAN VS EAGLE FORD - V1 -----------------------------------
## When did New Mexico Permian out number all of Williston?
vs_perm <- rc_master %>%
 filter(Country == "UNITED STATES" & (Basin %in% c("Permian", "Williston", "Eagle Ford"))) %>%
 mutate(loc = case_when(
  Basin == "Williston" ~ "Williston",
  Basin == "Eagle Ford" ~ "Eagle Ford",
  Basin == "Permian" & `State/Province` == "TEXAS" ~ "Permian (Texas)",
  Basin == "Permian" & `State/Province` == "NEW MEXICO" ~ "Permian (New Mexico)"
 )) %>%
 group_by(PublishDate, loc) %>%
 summarize(n = sum(RigCount)) %>%
 ungroup() %>%
 filter(loc != "Permian (Texas)") %>%
 ggplot() +
 geom_line(aes(PublishDate, n, color = loc)) +
 geom_text(aes(x, y, label = l, color = l), family = "Lato",
           tibble(x = c(as.Date("2018-07-24"), as.Date("2018-08-19"), as.Date("2018-03-08")),
                  y = c(66, 45, 100),
                  l = c("Eagle Ford", "Williston", "Permian (New Mexico)"))) +
 scale_x_date(NULL, expand = c(0, 0),
              limits = c(min(rc_master$PublishDate), max(rc_master$PublishDate) + 165)) +
 scale_y_continuous(NULL) +
 scale_color_manual(values = c("firebrick2", "skyblue", "green")) +
 guides(color = FALSE) +
 labs(title = "The Surging New Mexico Permian Rig Count",
      subtitle = paste0("During the boom, the New Mexico Permian had nearly 100 fewer rigs",
                        "\n",
                        "than either the Eagle Ford or the Williston. It Now has more than either."),
      caption = "Source: Baker Hughes") +
 theme_classic() +
 theme(text = element_text(color = "white", family = "Lato"), 
       axis.line = element_line(color = "gray80"), 
       axis.text = element_text(color = "gray80"),
       axis.ticks = element_line(color = "gray80"),
       panel.background = element_rect(fill = "gray20", color = "gray20"),
       plot.caption = element_text(color = "gray50", size = 10),
       plot.background = element_rect(fill = "gray20", color = "gray20"),
       legend.direction = "horizontal",
       legend.position = c(0.55, 1.04))

ggsave(vs_perm_f <- "imgs/vs_perm.png", vs_perm, width = 9, height = 4.5, dpi = 300)
(x <- image_read("imgs/vs_perm.png"))
x %>%
 image_resize("800x400")


# NEW MEXICO PERMIAN VS EAGLE FORD - V2 -----------------------------------
vs_text2 <- tibble(x = c(as.Date("2014-12-15")),
                   y = c(200),
                   l = c(paste0("Before the price crash,\n",
                         "   the Eagle Ford and Williston\n",
                         "     each had nearly 100 more rigs\n",
                         "       than the New Mexico Permian.")))

(vs_perm2 <- rc_master %>%
 filter(Country == "UNITED STATES" & (Basin %in% c("Permian", "Williston", "Eagle Ford"))) %>%
 mutate(loc = case_when(
  Basin == "Williston" ~ "Williston",
  Basin == "Eagle Ford" ~ "Eagle Ford",
  Basin == "Permian" & `State/Province` == "TEXAS" ~ "Permian (Texas)",
  Basin == "Permian" & `State/Province` == "NEW MEXICO" ~ "Permian (New Mexico)"
 )) %>%
 group_by(PublishDate, loc) %>%
 summarize(n = sum(RigCount)) %>%
 ungroup() %>%
 filter(loc != "Permian (Texas)") %>%
 ggplot() +
 geom_line(aes(PublishDate, n, color = loc)) +
 # geom_text(aes(x, y, label = l), data = vs_text2, family = "Lato", color = "white", hjust = 0) +
 scale_x_date(NULL, expand = c(0, 0),
              limits = c(min(rc_master$PublishDate), max(rc_master$PublishDate) + 165)) +
 scale_y_continuous(NULL) +
 scale_color_manual(values = c("firebrick2", "skyblue", "green")) +
 guides(color = FALSE) +
 labs(title = "The Surging New Mexico Permian",
      subtitle = "Rig Count",
      # subtitle = paste0("During the boom, the New Mexico Permian had nearly 100 fewer rigs",
      #                   "\n",
      #                   "than either the Eagle Ford or the Williston. It Now has more than either."),
      caption = "Source: Baker Hughes") +
 theme_classic() +
 theme(text = element_text(color = "white", family = "Lato"), 
       axis.line = element_line(color = "gray80"), 
       axis.text = element_text(color = "gray80"),
       axis.ticks = element_line(color = "gray80"),
       panel.background = element_rect(fill = "gray20", color = "gray20"),
       plot.caption = element_text(color = "gray50", size = 10),
       plot.background = element_rect(fill = "gray20", color = "gray20"),
       legend.direction = "horizontal",
       legend.position = c(0.55, 1.04)))

ggsave(vs_perm_f2 <- "imgs/vs_perm2.png", vs_perm2, width = 9, height = 4.5, dpi = 300)
(x2 <- image_read("imgs/vs_perm2.png"))
x2 %>%
 image_resize("800x400")



# PERMIAN MAP -------------------------------------------------------------
## Permian Rig Count
perm <- rc_master %>%
 filter(Basin == "Permian") %>%
 group_by(PublishDate, County, `State/Province`) %>%
 summarize(n = sum(RigCount)) %>%
 left_join(counties_map, ., by = c("NAME" = "County", "STATE_NAME" = "State/Province")) 

perm_only <- perm %>%
 filter(!is.na(n))

po_bbox <- st_bbox(perm_only)

tnmo_lines <- perm %>%
 filter(STATE_NAME %in% c("TEXAS", "NEW MEXICO", "OKLAHOMA")) %>%
 group_by(STATE_NAME) %>%
 summarize() %>%
 mutate(geometry = st_cast(geometry, "LINESTRING"))

tex_newmex <- perm %>%
 filter(STATE_NAME %in% c("TEXAS", "NEW MEXICO"))

tn_bbox <- st_bbox(tex_newmex)

perm %>%
 filter(STATE_NAME %in% c("TEXAS", "NEW MEXICO", "OKLAHOMA")) %>%
 ggplot() +
 geom_sf(aes(fill = n)) +
 geom_sf(data = tnmo_lines, color = "gray90") +
 scale_fill_gradientn(colors = pals::viridis(256)) +
 # scale_x_continuous(expand = expand_scale()) +
 # scale_y_continuous(expand = expand_scale()) +
 coord_sf(xlim = c(po_bbox[["xmin"]] - 1, po_bbox[["xmax"]]),
          ylim = c(po_bbox[["ymin"]], po_bbox[["ymax"]]),
          datum = NA) +
 theme(text = element_text(family = "Lato"),
       plot.background = element_rect("gray20", "gray20"),
       legend.position = c(0.2, 0.2),
       legend.text = element_text(color = "white"),
       legend.title = element_text(color = "white"))


