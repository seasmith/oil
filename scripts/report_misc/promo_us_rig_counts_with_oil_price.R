# library(lemon)
library(cowplot)
# library(gtable)
# library(patchwork)
library(grid)
library(gridExtra)
library(lubridate)
library(blg)
library(magick)
library(ggalt)


################################################################################
# Oil price                                                                    #
################################################################################

load("~/R/misc/oil/price/oil_price.RData")
load("~/R/misc/oil/US/rig_counts/rc_og.RData")
fnt <- "Open Sans"
clr <- "white"


oil_price2 <- oil_price %>%
  group_by(week = lubridate::week(date),
           year = lubridate::year(date)) %>%
  summarize(mprice = mean(price, na.rm = TRUE)) %>%
  ungroup()

oil_price2 <- oil_price %>%
  group_by(week = lubridate::week(date),
           year = lubridate::year(date)) %>%
  arrange(date) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  left_join(oil_price2, by = c("week", "year"))

mins <- c(min(oil_price2$date), min(rc_og$Date))
mn   <- sort(mins)[length(mins)]
maxs <- c(max(oil_price2$date), max(rc_og$Date))
mx   <- sort(maxs)[1L]

oil_price2 <- oil_price2 %>%
  filter(between(date, mn, mx))

# new_min_max <- rc_og %>%
#   inner_join(oil_price, by = c("Date" = "date")) %>%
#   pull(Date) %>%
#   range()
# 
# oil_price3 <- oil_price %>%
#   filter(between(date, new_min_max[1], new_min_max[2]))

## Line ##
op_line <- oil_price2 %>%
  ggplot() +
  geom_line(aes(date, mprice, color = mprice), size = 0.8) +
  geom_text(aes(x, y + 5, label = y), tibble(x = as.Date("1989-07-01"), y = c(50, 100)),
            color = "gray90", family = "Open Sans", size = 3) +
  scale_color_viridis_c(option = "B", begin = 0, end = 1) +
  scale_x_date(expand = expand_scale(),
               minor_breaks = NULL) +
  scale_y_continuous(expand = expand_scale(),
                     minor_breaks = NULL) +
  guides(color = guide_colorbar(title = "Dollars Per Barrel Of Oil",
                                title.position = "top",
                                raster = FALSE, ticks = FALSE,
                                barwidth = 6, barheight = .9)) +
  theme_drk() +
  theme(legend.position = c(0.215, 0.83),
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "gray10"))

op_ydens <- op_line %>%
  axis_canvas("y", coord_flip = TRUE) +
  geom_density(aes(mprice), oil_price2, stat = "bkde",
               fill = viridis::inferno(10)[5L],
               color = "#00000000",
               alpha = 0.3) +
  coord_flip()

op_comb <- op_line %>%
  insert_yaxis_grob(op_ydens)

## Raster ##
op_raster <- oil_price2 %>%
  ggplot() +
  geom_raster(aes(factor(date), "Oil Price", fill = mprice),
              interpolate = TRUE) +
  scale_fill_viridis_c(option = "B", begin = 0, end = 1) +
  scale_x_discrete(expand = expand_scale()) +
  scale_y_discrete(expand = expand_scale()) +
  guides(fill = FALSE) +
  theme_drk() +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

op_comb2 <- op_comb %>%
  insert_xaxis_grob(op_raster, position = "bottom")
## Arrange and save ##  
# op_raster_line <- grid.arrange(arrangeGrob(op_line, op_raster, layout_matrix = rbind(1, 1, 1, 1, 1, 1, 1, 2)))
# op_raster_line <- grid.arrange(arrangeGrob(op_comb, op_raster, layout_matrix = rbind(1, 1, 1, 1, 1, 1, 1, 2)))

# ggsave("op_raster_line.png", op_raster_line, width = 700 / 100, height = 250 / 100, dpi = 100)
# ggsave("op_raster_line2.png", op_raster_line, width = 700 / 100, height = 250 / 100, dpi = 100)
ggsave("op_raster_line3.png", op_comb2, width = 700 / 100, height = 250 / 100, dpi = 100)


################################################################################
# Toal oil and gas rig count                                                   #
################################################################################

rc_og <- rc_og %>%
  mutate(og = Oil + Gas)

rc_og <- rc_og %>%
  filter(between(Date, mn, mx))

## Line ##
(rt_line <- rc_og %>%
  ggplot() +
  geom_line(aes(Date, og, color = og), size = 0.8) +
  geom_text(aes(x, y + 60, label = comma(y)), tibble(x = as.Date("1989-02-01"), y = c(500, 1000, 1500)),
            color = "gray90", family = "Open Sans", size = 3) +
  scale_color_viridis_c(option = "D", breaks = pretty_breaks(2), labels = comma) +
  scale_x_date(expand = expand_scale(),
               minor_breaks = NULL) +
  scale_y_continuous(expand = expand_scale(),
                     minor_breaks = NULL,
                     labels = comma) +
  guides(color = guide_colorbar(title = "Total Oil And Gas Rig Count",
                                title.position = "top",
                                raster = FALSE, ticks = FALSE,
                                barwidth = 6, barheight = 0.9)) +
  theme_drk() +
  theme(legend.position = c(0.23, 0.835),
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        axis.text.y = element_blank()))

rt_ydens <- rt_line %>%
  axis_canvas("y", coord_flip = TRUE) +
  geom_density(aes(og), rc_og, stat = "bkde",
               fill = viridis::viridis(10)[5L],
               color = "#00000000", alpha = 0.3) +
  coord_flip()

rt_comb <- rt_line %>%
  insert_yaxis_grob(rt_ydens)

## Raster ##
(rt_raster <- rc_og %>%
  ggplot() +
  geom_raster(aes(factor(Date), "Rig Count", fill = og),
              interpolate = TRUE) +
  scale_fill_viridis_c(option = "D") +
  scale_x_discrete(expand = expand_scale()) +
  scale_y_discrete(expand = expand_scale()) +
  guides(fill = FALSE) +
  theme_drk() +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()))

rt_comb2 <- rt_comb %>%
  insert_xaxis_grob(rt_raster)
## Arrange and save ##  
# rt_raster_line <- grid.arrange(arrangeGrob(rt_line, rt_raster, layout_matrix = rbind(2, 1, 1, 1, 1, 1, 1, 1)))
# rt_raster_line <- grid.arrange(arrangeGrob(rt_comb, rt_raster, layout_matrix = rbind(2, 1, 1, 1, 1, 1, 1, 1)))

# ggsave("rt_raster_line.png", rt_raster_line, width = 700 / 100, height = 250 / 100, dpi = 100)
# ggsave("rt_raster_line2.png", rt_raster_line, width = 700 / 100, height = 250 / 100, dpi = 100)
ggsave("rt_raster_line3.png", rt_comb2, width = 700 / 100, height = 250 / 100, dpi = 100)





i <- image_read("op_raster_line3.png")
i_info <- image_info(i)

j <- image_read("rt_raster_line3.png")
j_info <- image_info(j)

(
  i_plot <- i %>%
    image_join(
      image_blank(i_info$width, 110, "gray10"),
      .,
      image_blank(i_info$width, 0, "gray10"),
      j,
      image_blank(i_info$width, 20, "gray10")
    ) %>%
    image_append(stack = TRUE)
  )

(
  i_plot <- i_plot %>%
    image_annotate("Rig Count Lags As Oil Rises",
                   location = geometry_point(3, -7),
                   color = clr, font = fnt, size = 32) %>%
    image_annotate("Oil prices have risen from $50 in October 2017 to $65",
                   location = geometry_point(3, 36),
                   color = clr, font = fnt, size = 19) %>%
    image_annotate("in February 2018 - a 30% increase.",
                   location = geometry_point(3, 61),
                   color = clr, font = fnt, size = 19) %>%
    image_annotate("Luke Smith (@lksmth)",
                   location = geometry_point(565, 612),
                   color = "springgreen", font = fnt, size = 13) %>%
    image_annotate("Source: Baker Hughes",
                   location = geometry_point(3, 614),
                   color = "gray50", font = fnt, size = 12)
  )

image_write(i_plot, "oil_price_rig_count2.png")





