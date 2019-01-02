library(purrr)
library(magick)

# Make pretty dates
pretty_date <- function(n) format(n, "%B %d, %Y")
trunc_date <- function(x) strftime(x, format = "%b '%y")
trunc_date2 <- function(x) strftime(x, format = "%B %Y")
trunc_date3 <- function(x) strftime(x, format = "'%y")
week_segment <- function(x) if (lubridate::wday(x, label = FALSE, week_start = 1L) <= 5L) "weekday" else "weekend"

# Creates title-like text at the top of an image
set_top_text <- function(i, text, height = 125, size = 100, font = "Arial", 
                         color = "black", bg_color = "white", gravity = "northwest",
                         location = "0+0") {
 i %>%
  image_join(
   image_blank(image_info(.)$width, height = height, color = bg_color) %>%
    image_annotate(text = text, font = font, color = color, size = size, location = location, gravity = gravity)
   , .
  ) %>%
  image_append(stack = TRUE)
}

set_bottom_text <- function(i, text, height = 125, size = 100, font = "Arial",
                            bg_color = "white", color = "black", gravity = "northwest", 
                            location = "0+0") {
 i %>%
  image_join(
   .,
   image_blank(image_info(.)$width, height = height, color = bg_color) %>%
    image_annotate(text = text, font = font, color = color, size = size,
                   location = location, gravity = gravity)
  ) %>%
  image_append(stack = TRUE)
}

# Pads an image with whitespace on the left-hand side
set_padding_l <- function(i, size = 100, color = color) {
 i %>%
  image_join(
   image_blank(size, image_info(.)$height, color = color) 
   , .
  ) %>%
  image_append(stack = FALSE)
}

# Pads an image with whitespace on the right-hand side
set_padding_r <- function(i, size = 100, color = color) {
 i %>%
  image_join(
   image_blank(size, image_info(.)$height, color = color) 
  ) %>%
  image_append(stack = FALSE)
}

# Pads an image with whitespace at the top
set_padding_t <- function(i, size = 100, color = color) {
 i %>%
  image_join(
   image_blank(image_info(.)$width, size, color = color)
   , .
  ) %>%
  image_append(stack = TRUE)
}

# Pads an image with whitespace at the bottom
set_padding_b <- function(i, size = 100, color = color) {
 i %>%
  image_join(
   image_blank(image_info(.)$width, size, color = color) 
  ) %>%
  image_append(stack = TRUE)
}

# Bind (append) images horizontally -- for use with purrr::reduce()
bind_images_h <- function(x, y, p = 100, bg_color = "white") {
 x %>% 
  set_padding_r(size = p, color = bg_color) %>%
  image_join(y) %>%
  image_append(stack = FALSE)
}

# Bind (append) images vertically -- for use with purrr::reduce()
bind_images_v <- function(x, y, p = 100, bg_color = "white") {
 x %>% 
  set_padding_b(size = p, color = bg_color) %>%
  image_join(y) %>%
  image_append(stack = TRUE)
}
