library(ggplot2)
library(magick)
library(sf)
library(sp)
library(lemon)


maps_path <- "~/R/oil/data/maps"


load(file.path(maps_path, "basins", "sf_plays.RData"))
load(file.path(maps_path, "us", "states_48.RData"))

states_48 <- st_transform(states_48, crs = st_crs(102003))
bbox <- st_bbox(states_48)


load(file.path(maps_path, "basins", "text_mods.RData"))
load(file.path(maps_path, "basins", "line_barnett.RData"))
load(file.path(maps_path, "basins", "line_cana_woodford.RData"))
load(file.path(maps_path, "rasters", "us_48_only_no_ocean_gray_raster.RData"))

us48 <- SpatialPixelsDataFrame(us_48_only_no_ocean_gray_raster[, c("x", "y")],
                               us_48_only_no_ocean_gray_raster[, c("ele"), drop = FALSE],
                               proj4string = CRS(st_crs(4326)$proj4string))
us48 <- spTransform(us48, CRSobj = CRS(st_crs(102003)$proj4string))
us48 <- as.data.frame(us48)


# Sacraficial plot (for raster map legend)
sacrifice <- ggplot() +
 geom_col(aes(x, y, fill = z), tibble(x = 1:3, y = 3:1, z = factor(c("Increase", "No Change", "Decrease"), levels = c("Increase", "No Change", "Decrease"), ordered = TRUE))) +
 scale_fill_manual(NULL, values = c(positive, neutral, negative)) +
 guides(fill = guide_legend(keywidth = 5, keyheight = 0.5, direction = "horizontal", label.position = "top")) + theme_gray20()

lgnd <- g_legend(sacrifice)

# Make raster
base_raster <- ggplot() +
 geom_point(aes(x = x, y = y, color = ele), us48, size = 0.01) +
 geom_sf(data = states_48, fill = "#00000000", color = "gray70") +
 geom_text(data = text_mods, aes(X, Y, label = Shale_play),
           size = 3.5, family = "Open Sans", color = "cornsilk") +
 geom_line(data = line_barnett, aes(X, Y), color = "gray80") +
 geom_line(data = line_cana_woodford, aes(X, Y), color = "gray80") +
 scale_color_gradient(low = "gray10", high = "gray80") +
 scale_x_continuous(expand = expand_scale()) +
 scale_y_continuous(expand = expand_scale()) +
 guides(color = FALSE, alpha = FALSE, fill = FALSE) +
 coord_sf(datum = NA, crs = st_crs(102003)) +
 theme_gray20(16) +
 theme(axis.title = element_blank(),
       axis.text = element_blank(),
       legend.position = c(0.81, 0.13),
       legend.justification = c("left", "bottom"),
       legend.box.just = "right",
       legend.text = element_text(color = "gray80"),
       plot.background = element_rect(fill = "gray20",
                                      color = "gray20"),
       plot.caption = element_text(vjust = -1),
       plot.margin = margin(0, 0, 0, 0, "pt"))

base_raster <- base_raster +
 annotation_custom(lgnd, xmin = 60000, xmax = 100000, ymin = -1220000, ymax = -1120000)

ggsave(f_base_raster <- "~/R/oil/imgs/maps/base/us-whole-raster-annotated.png",
       base_raster, dpi = 600)

base_raster <- f_base_raster %>%
 image_read() %>%
 image_trim() %>%
 as.raster()
