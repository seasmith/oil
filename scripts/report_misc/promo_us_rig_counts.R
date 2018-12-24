library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magick)
library(scales)
library(cowplot)

load("~/R/oil/data/rig_counts/rc_master.RData")
load("~/R/oil/data/rig_counts/rc_basin.RData")
source("~/R/oil/scripts/tidy/rig-counts/summarize-rc_basin.R")

# Make pretty dates
pretty_date <- function(n) format(n, "%B %d, %Y")

theme_gray20 <- function(text_size = 16,
                         bg_color = "gray20",
                         title_color   = "gray80", text_color = "gray90",
                         grid_color    = "gray70",
                         caption_color = "gray50") {
 
 theme(text = element_text("Open Sans", size = text_size, color = text_color),
       panel.grid.major   = element_line(color = grid_color),
       panel.grid.major.y = element_line(linetype = "dotted"),
       axis.text  = element_text(color = text_color),
       axis.title = element_blank(),
       plot.title = element_text(color = title_color),
       plot.subtitle = element_text(color = title_color),
       plot.caption  = element_text(colour = caption_color, size = 10),
       plot.background = element_rect(fill = bg_color,
                                      color = bg_color))
 
}

bg_color <- "gray20"

# Creates title-like text at the top of an image
set_top_text <- function(i, text, height = 125, size = 100, font = "Arial", color = "black", location = "0+0") {
 i %>%
  image_join(
   image_blank(image_info(.)$width, height = height, color = bg_color) %>%
    image_annotate(text = text, font = font, color = color, size = size, location = location)
   , .
  ) %>%
  image_append(stack = TRUE)
}

set_bottom_text <- function(i, text, height = 125, size = 100, location = "0+0", font = "Arial", color = "black") {
 i %>%
  image_join(
   .,
   image_blank(image_info(.)$width, height = height, color = bg_color) %>%
    image_annotate(text = text, font = font, color = color, size = size,
                   location = location)
  ) %>%
  image_append(stack = TRUE)
}

# Pads an image with whitespace on the right-hand side
set_padding_r <- function(i, size = 100, color = bg_color) {
 i %>%
  image_join(
   image_blank(size, image_info(.)$height, color = bg_color) 
  ) %>%
  image_append(stack = FALSE)
}

# Pads an image with whitespace on the left-hand side
set_padding_b <- function(i, size = 100, color = bg_color) {
 i %>%
  image_join(
   image_blank(image_info(.)$width, size, color = bg_color) 
  ) %>%
  image_append(stack = TRUE)
}

# Bind (append) images horizontally -- for use with purrr::reduce()
bind_images_h <- function(x, y, p = 100) {
 x %>% 
  set_padding_r(size = p) %>%
  image_join(y) %>%
  image_append(stack = FALSE)
}

# Bind (append) images vertically -- for use with purrr::reduce()
bind_images_v <- function(x, y, p = 100) {
 x %>% 
  set_padding_b(size = p) %>%
  image_join(y) %>%
  image_append(stack = TRUE)
}


purp00 <- "#bdadb8"
purp0 <- "#a6819c"
purp1 <- "#814b72"
purp2 <- "#5a344f"
purp3 <- "#361f2f"

# Summary basins per week
rcm <- rc_master %>%
  filter(Country == "UNITED STATES") %>%
  filter(!is.na(RigCount)) %>%
  group_by(Basin, PublishDate) %>%
  summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
  arrange(RigCount)

rcm_current <- rcm %>%
  filter(PublishDate == key_dates[["this_week"]])

rcm_current <- rcm_current %>%
  ungroup() %>%
  add_row(Basin = "Fayetteville", PublishDate = key_dates[["this_week"]], RigCount = 0)

rcm <- rcm %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  ungroup()

# Get levels to rank basins
basin_levels <- rcm_current %>%
  arrange(desc(RigCount)) %>%
  pull(Basin) %>%
  unique()

# Rank basins
rcm <- rcm %>%
  mutate(rank = as.integer(factor(Basin, levels = rev(basin_levels), ordered = TRUE)))

# Convert to wide format
rcm <- rcm %>%
  select(-PublishDate) %>%
  group_by(Basin) %>%
  mutate(cat = if_else(RigCount == max(RigCount), "max", "min")) %>%
  ungroup() %>%
  spread(cat, RigCount)

rcm <- rcm %>%
  inner_join(rcm_current, "Basin")

rcm_label <- tibble(x = 267, y = 7.5, label = "= current rig count")

rcm %>%
  ggplot() +
  geom_rect(aes(ymin = rank + 0.25, ymax = rank - 0.25, xmin = min, xmax = max, fill = "1")) +
  geom_rect(aes(ymin = rank + 0.4, ymax = rank - 0.4, xmin = RigCount - 2.5, xmax = RigCount + 2.5), fill = purp3) +
  geom_text(aes(x, y, label = label), rcm_label, nudge_x = 115) +
  geom_rect(aes(ymin = y + 0.25, ymax = y - 0.25, xmin = x - 2.5, xmax = x + 2.5), rcm_label, fill = purp3) +
  scale_y_continuous(breaks = seq_along(basin_levels),
                     labels = rev(basin_levels),
                     expand = expand_scale()) +
  scale_x_continuous(expand = expand_scale(),
                     labels = comma) +
  scale_fill_manual(values = purp0) +
  labs(title = paste0("Room to grow"),
       subtitle = paste0("Rig count ranges per basin", " (",
                         pretty_date(min(rc_master$PublishDate)), " to ",
                         pretty_date(max(rc_master$PublishDate)), ")"),
       caption = paste0("Source: Baker Hughes"),
       x = NULL, y = NULL) +
  guides(fill = FALSE) +
  # theme(plot.title = element_text(hjust = -0.85),
  #       plot.subtitle = element_text(size = 12, hjust = 0.96, margin = margin(7, 0, 5, 0, "pt")),
  #       plot.caption = element_text(size = 10, color = "gray50", margin = margin(5, 0, 0, 0, "pt")),
  #       axis.title.x = element_text(size = 14, hjust = 0.3),
  #       axis.text.y = element_text(size = 11),
  #       axis.text.x = element_text(size = 11),
  #       panel.grid.major.y = element_blank())
 blg::blg_theme_default() +
 theme(panel.grid.major.y = element_blank(),
       plot.caption = element_text(color = "gray50"))



#   -----------------------------------------------------------------------
# RASTER CRASH ------------------------------------------------------------
#   -----------------------------------------------------------------------

bg_color <- "gray20"

# fnt <- "Bitstream Vera Sans"
fnt <- "Open Sans"
clr <- "white"

basin_order <- rc_basin %>%
  filter(key == "Total") %>%
  filter(year(Date) == 2017) %>%
  mutate(basin = if_else(basin == "Others", "Other Basins", as.character(basin))) %>%
  group_by(basin) %>%
  summarize(m = mean(value, na.rm = TRUE)) %>%
  arrange(desc(m)) %>%
  pull(basin)

rc_basin <- rc_basin %>%
  mutate(basin = if_else(basin == "Others", "Other Basins", as.character(basin))) %>%
  mutate(basin = factor(basin, levels = rev(basin_order), ordered = TRUE)) %>%
  arrange(Date)

rc_basin_index <- rc_basin %>%
 filter(key == "Total") %>%
 group_by(basin) %>%
 mutate(index = scale(value)) %>%
 ungroup() %>%
 mutate(basin = if_else(basin == "Others", "Other Basins", as.character(basin))) %>%
 mutate(basin = factor(basin, levels = rev(basin_order), ordered = TRUE))

basin_order_index <- rc_basin_index %>%
 filter(key == "Total") %>%
 filter(Date == max(Date)) %>%
 mutate(basin = if_else(basin == "Others", "Other Basins", as.character(basin))) %>%
 arrange(desc(index)) %>%
 pull(basin)

rc_basin_index <- rc_basin_index %>%
 mutate(basin = if_else(basin == "Others", "Other Basins", as.character(basin))) %>%
 mutate(basin = factor(basin, levels = rev(basin_order_index), ordered = TRUE)) %>%
 arrange(Date)


(crash <- rc_basin_index %>%
  filter(key == "Total") %>%
  # mutate()
  ggplot() +
  geom_raster(aes(factor(Date, ordered = TRUE, exclude = NA), basin, fill = index), interpolate = TRUE) +
  scale_fill_viridis_c("Rig Count (Index)") +
  scale_x_discrete(expand = expand_scale()) +
  scale_y_discrete(expand = expand_scale()) +
  guides(fill = guide_colorbar(title = "Rig Count  (Index)",
                               title.position = "top",
                               barwidth = 19.2, barheight = 1,
                               raster = FALSE, ticks = FALSE)) +
  theme_gray20() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "white"),
        axis.line = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(-0.33, 1.16),
        plot.margin = unit(c(4.4, 0.2, 0, 0), "lines")))

ggsave("crash.png", crash, width = 6, height = 4, dpi = 600)


(oc <- image_read("crash.png") %>% image_trim())

oc_info <- image_info(oc)

oc %>%
 image_join(
  image_blank(oc_info$width, 80, bg_color)
 ) %>%
 image_append(stack = TRUE) %>%
 image_annotate(format(min(rc_basin$Date), "%b %d, %Y"),
                location = geometry_point(830, 2365),
                color = clr, font = fnt, size = 90) %>%
 image_join(
  image_blank(43, image_info(.)$height, bg_color)
 ) %>%
 image_append() %>%
 image_annotate(format(max(rc_basin$Date), "%b %d, %Y"),
                location = geometry_point(3050, 2365),
                color = clr, font = fnt, size = 90) %>%
 set_top_text("Crash And Recovery In US Oil And Gas", color = "white",
              size = 200, height = 225, location = "-5+0") %>%
 set_bottom_text("@lksmth", color = "springgreen", size = 70, height = 100, location = "-5+10") %>%
 image_annotate("Source: Baker Hughes", color = "white", location = geometry_point(300, 2715), size = 70)



# INDEXED/SCALED ----------------------------------------------------------
rc_basin_margin <- rc_basin_index %>%
  group_by(basin) %>%
  summarize(value = mean(value, na.rm = TRUE))

rc_basin_summ <- rc_basin %>%
  group_by(Date) %>%
  summarize(value = sum(value, na.rm = TRUE))

crash3 <- rc_basin_index %>%
  ggplot() +
  geom_raster(aes(factor(Date), basin, fill = index), interpolate = TRUE) +
  scale_fill_viridis_c() +
  scale_x_discrete(expand = expand_scale()) +
  scale_y_discrete(expand = expand_scale()) +
  guides(fill = guide_colorbar(title = "",
                               title.position = "top",
                               barwidth =1, barheight = 19,
                               raster = FALSE, ticks = FALSE)) +
  theme_gray20() +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "white"),
        axis.line = element_blank(),
        legend.direction = "vertical",
        plot.margin = unit(c(0, 0, 0, 0), "lines"))

ggsave("crash3.png", crash3, width = 600 / 100, height = 400 / 100, dpi = 100)


oc3 <- image_read("crash3.png") %>% image_trim()

oc3_info <- oc3 %>% image_info()

oc3 <- oc3 %>%
  image_join(
    image_blank(oc3_info$width, 100, bg_color),
    .,
    image_blank(oc3_info$width, 45, bg_color)
  ) %>%
  image_append(stack = TRUE)

(oc3 <- oc3 %>%
    image_annotate("The Oil Price Crash As Told By Rig Count",
                   location = geometry_point(3,-3),
                   color = clr, font = fnt, size = 32) %>%
    image_annotate("Indexed rig counts of various major US basins show both the",
                   location = geometry_point(3, 38),
                   color = clr, font = fnt, size = 18) %>%
    image_annotate("2014 oil price crash and each basin's multiyear high and low.",
                   location = geometry_point(3, 60),
                   color = clr, font = fnt, size = 18))

(oc3 <- oc3 %>%
    image_annotate(format(min(rc_basin$Date), "%b %Y"),
                   location = geometry_point(102, 503),
                   color = clr, font = fnt, size = 14) %>%
    image_annotate(format(max(rc_basin$Date), "%b %Y"),
                   location = geometry_point(498, 503),
                   color = clr, font = fnt, size = 14) %>%
    image_annotate("Dec 2014",
                   location = geometry_point(340, 503),
                   color = clr, font = fnt, size = 14) %>%
    image_annotate("Rig Count",
                   location = geometry_point(537, 82),
                   color = clr, font = fnt, size = 13) %>%
    image_annotate("Index",
                   location = geometry_point(537, 97),
                   color = clr, font = fnt, size = 13)
  )
  

(oc3 <- oc3 %>%
    image_annotate("Luke Smith (@lksmth)",
                   location = geometry_point(452, 525),
                   color = "gray90", font = fnt, size = 14) %>%
    image_annotate("Source: Baker Hughes",
                   location = geometry_point(3, 527),
                   color = "gray70", font = fnt, size = 12))

image_write(oc3, "oil_price_crash2.png")




xdens <- crash3 %>%
  axis_canvas("x") +
  geom_density(aes(Date, value), data = rc_basin_summ, stat = "identity")

ybars <- crash3 %>%
  axis_canvas("y", coord_flip = TRUE) +
  geom_col(aes(basin, value), rc_basin_margin, fill = viridis::viridis(1)) +
  coord_flip()

p1 <- crash3 %>%
  insert_xaxis_grob(xdens, grid::unit(0.2, "null"), position = "top")

p2 <- crash3 %>%
  insert_yaxis_grob(ybars, grid::unit(0.2, "null"), position = "right")

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# RASTER CRASH ------------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(lemon)
library(gtable)
library(patchwork)
library(grid)
library(gridExtra)

load("~/R/misc/oil/price/oil_price.RData")

rcb <- rc_basin %>%
  filter(key == "Total") %>%
  group_by(basin) %>%
  mutate(index = as.numeric(scale(value))) %>%
  ungroup() %>%
  select(basin, Date, index, value) %>%
  rename(key = basin)

rct <- rc_basin %>%
  filter(key == "Total") %>%
  group_by(Date) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(index = scale(value)) %>%
  mutate(key = "rig_count")

oil_price <- oil_price %>%
  semi_join(rcb, by = c("date" = "Date")) %>%
  mutate(index = scale(price),
         key = factor("oil_price")) %>%
  rename(Date = date, value = price)

rcb %>%
  bind_rows(oil_price) %>%
  ggplot() +
  geom_raster(aes(factor(Date), key, fill = index)) +
  scale_fill_viridis_c() +
  theme_gray20()


rc_raster <- rct %>%
  ggplot() +
  geom_raster(aes(factor(Date), key, fill = value)) +
  scale_fill_viridis_c(option = "C") +
  scale_x_discrete(expand = expand_scale()) +
  scale_y_discrete(expand = expand_scale()) +
  guides(fill = guide_colorbar(title = "",
                               barwidth = 1, barheight = 7,
                               raster = FALSE, ticks = FALSE,
                               label.position = "left")) +
  theme_gray20() +
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "left",
        legend.background = element_rect(fill = "gray10", color = "gray10"),
        legend.margin = margin(0.0, 0.25, 0, 0.25, "lines"))

leg <- g_legend(rc_raster)

rc_raster <- rc_raster + guides(fill = FALSE)

rc_line <- rct %>%
  ggplot() +
  geom_line(aes(Date, value, color = value), size = 0.8) +
  scale_color_viridis_c(option = "C") +
  scale_x_date(expand = expand_scale()) +
  scale_y_continuous(expand = expand_scale()) +
  guides(color = FALSE) +
  theme_gray20() +
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank())

rig_count_plots <- rc_raster + rc_line + plot_layout(ncol = 1)


# GROBS!!! ----------------------------------------------------------------

pg <- patchworkGrob(rig_count_plots)
rg <- ggplotGrob(rc_raster)
lg <- ggplotGrob(rc_line)

heights <- list(pg, leg) %>%
  map(~.x$heights[2:5])
mH <- heights %>% pmap(unit.pmax)
class(mH) <- c("unit.list", "unit")
# rg$heights[2:5] <- as.list(mH)
leg$heights[2:5] <- as.list(mH)


ggsave("raster_rig_count.png", pg, width = 500 / 100, height = 300 / 100, dpi = 100)
ggsave("raster_rig_count_legend.png", leg, width = 68 / 100, height = 150 / 100, dpi = 100)
i <- image_read("raster_rig_count.png")
j <- image_read("raster_rig_count_legend.png")
j <- j %>%
  image_join(image_blank(80, 150, "gray10")) %>%
  image_append(stack = TRUE)

(k <- i %>%
  image_join(j, .) %>%
  image_append())

(k <- k %>%
  image_fill("gray10", geometry_point(70, 1)))
image_write(k, "oil_price_crash3.png")

# rig_count_plots <- rc_raster + rc_line + plot_layout(ncol = 1)
# pg <- patchworkGrob(rig_count_plots)
# 
# 
# a <- gtable(unit(c(1, 5), c("in")), unit(c(1, 1), "in"))
# b <- gtable_add_grob(a, rg, 1, 2)
# b <- gtable_add_grob(b, lg, 2, 2)
# d <- gtable_add_grob(a, leg, 1, 1, clip = "off")
# d <- gtable_add_grob(d, rectGrob(width = unit(1, "in"), height = unit(1, "in"), gp = gpar(fill = "gray10", color = "gray10")), 1, 1)
# 
# grid.newpage()
# grid.draw(b)
# grid.draw(d)
# 
# a <- gtable::gtable(unit(c(1, 5), c("in")), unit(c(2), "in"))
# b <- gtable_add_grob(a, pg, 1, 2)
# d <- gtable_add_grob(b, grid.rect(width = unit(1, "in"), height = unit(2, "in"), gp = gpar(fill = "gray10", col = "gray10")), 1, 1)
# e <- gtable_add_grob(b, leg, 1, 1)
# 
# 
# grid.draw(d)
# grid.draw(e)
# 
# pg$grobs <- pg$grobs[-which(pg$layout$name == "guide-box")[2]]
# pg$layout <- pg$layout[-which(pg$layout$name == "guide-box")[2], , drop = FALSE]
# 
# grid.newpage()
# grid.draw(pg)
# 
# 
# 
# 
# 
# a <- rct %>%
#   bind_rows(oil_price) %>%
#   ggplot() +
#   geom_raster(aes(factor(Date), key, fill = index)) +
#   scale_fill_viridis_c() +
#   theme_drk() +
#   theme(axis.text.x = element_blank(),
#         panel.grid.major = element_blank())
# 
# ggsave("price_and_rigs.svg", a)



#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#  PRICE AND RIG COUNT ----------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------

og_labels <- seq.int(1987, 2017, 1)
og_labels[c(FALSE, TRUE)] <- ""

og_ridge <- rc_og %>%
  select(Date, Oil, Gas) %>%
  filter(lubridate::year(Date) != 2018) %>%
  gather(... = -Date) %>%
  mutate(year = factor(lubridate::year(Date))) %>%
  ggplot() +
  ggridges::geom_density_ridges(aes(value, year, fill = key), alpha = 0.8) +
  scale_x_continuous(expand = expand_scale(),
                     breaks = c(400, 1200)) +
  scale_y_discrete(NULL,
                   expand = expand_scale(),
                   labels = og_labels) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.direction = "horizontal", legend.position = "top") +
  guides(fill = FALSE)

price_ridge <- oil_price %>% 
  filter(between(date, min(rc_og$Date), max(rc_og$Date))) %>%
  filter(lubridate::year(date) != 2018) %>%
  mutate(year = year(date)) %>%
  ggplot() +
  geom_density_ridges(aes(price, factor(year)), fill = "#66947f") +
  scale_x_continuous(expand = expand_scale()) +
  scale_y_discrete(NULL,
                   expand = expand_scale(),
                   labels = og_labels) +
  # labs(subtitle = paste0("Price",
  #                        "\nDollars ($) Per Barrel"),
  #      x = NULL, y = NULL) +
  theme(plot.subtitle = element_text(size = 11, margin = margin(b = 8, unit = "pt")),
        plot.margin = unit(c(0, 0, 0, 0), "pt"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))

grid.arrange(og_ridge, price_ridge, ncol = 2)


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#  PRICE AND RIG COUNT ----------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
