library(ggplot2)

# Create blank geom to hold line and bar plots
gb <- ggplot() +
 geom_blank() +
 theme_gray20(16) +
 theme(text = element_text(size = 16),
       axis.title = element_blank(),
       plot.caption = element_text(hjust = 0))
