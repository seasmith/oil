# Load dependencies
library(tidyverse)
library(sf)
load("data/rig_counts/rc_master.RData")
load("data/maps/d3/us_off.RData")
load("data/maps/d3/us_on.RData")

# CURRENT WEEK DATA -------------------------------------------------------
current_week <- max(rc_master$PublishDate, na.rm = TRUE)

# Offshore counts
off_data <- rc_master %>%
 filter(PublishDate == max(PublishDate) &
         Location == "Offshore") %>%
 group_by(County, `State/Province`, DrillFor) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 ungroup() %>%
 {
  x <- .
  group_by(x, County, `State/Province`) %>%
   summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
   ungroup() %>%
   bind_rows(x) %>%
   mutate(DrillFor = if_else(is.na(DrillFor), "Total", DrillFor))
 } %>%
 select(name = County, RigCount, DrillFor)

off_data <- off_data %>%
 inner_join(us_off %>% st_set_geometry(NULL), by = "name")

# Onshore counts
on_data <- rc_master %>%
 filter(PublishDate == max(PublishDate) &
         Location != "Offshore") %>%
 group_by(County, `State/Province`, DrillFor) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 ungroup() %>%
 {
  x <- .
  group_by(x, County, `State/Province`) %>%
   summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
   ungroup() %>%
   bind_rows(x) %>%
   mutate(DrillFor = if_else(is.na(DrillFor), "Total", DrillFor))
 } %>%
 select(name = County, state = `State/Province`, RigCount, DrillFor)

# Combine
rig_counts <- bind_rows(off_data, on_data)

# New column for matching in JA
rig_counts <- unite(rig_counts, location, name, state, remove = FALSE)
rig_counts <- unite(rig_counts, display_name, name, state, sep = ", ")

# Fill spaces with '-'
rig_counts <- rig_counts %>% mutate(location = str_replace(location, " ", "-"))

# Spread and fill in NA holders with 0
rig_counts <- rig_counts %>%
 spread(DrillFor, RigCount) %>%
 mutate_at(vars(Gas:Oil), function(x) if_else(is.na(x), 0L, x)) %>%
 mutate_at(vars(display_name), str_to_title) %>%
 mutate_at(vars(display_name), function(x) case_when(
  grepl("Gom$", x) ~ str_replace(x, "Gom$", "GOM"),
  grepl("Pac$", x) ~ str_replace(x, "Pac$", "PAC"),
  grepl("Ak", x)   ~ str_replace(x, "Ak$", "OAK"),
  TRUE             ~ x
 ))

# Create new directory (if needed) and save
if (! dir.exists("data/rig_counts/d3")) dir.create("data/rig_counts/d3")
write_csv(rig_counts, "data/rig_counts/d3/rig_counts.csv")



# WEEKLY DATA -------------------------------------------------------------

# Offshore weekly count by resource type
off_weekly <- rc_master %>%
 filter(Location == "Offshore") %>%
 group_by(County, `State/Province`, DrillFor, PublishDate) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 ungroup() %>%
 {
  x <- .
  group_by(x, County, `State/Province`, PublishDate) %>%
   summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
   ungroup() %>%
   bind_rows(x) %>%
   mutate(DrillFor = if_else(is.na(DrillFor), "Total", DrillFor))
 } %>%
 select(name = County, RigCount, DrillFor, PublishDate)

off_weekly <- off_weekly %>%
 inner_join(us_off %>% st_set_geometry(NULL), by = "name")

# Onshore weekly count by resource type
on_weekly <- rc_master %>%
 filter(Location != "Offshore") %>%
 group_by(County, `State/Province`, DrillFor, PublishDate) %>%
 summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
 ungroup() %>%
 {
  x <- .
  group_by(x, County, `State/Province`, PublishDate) %>%
   summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
   ungroup() %>%
   bind_rows(x) %>%
   mutate(DrillFor = if_else(is.na(DrillFor), "Total", DrillFor))
 } %>%
 select(name = County, state = `State/Province`, RigCount, DrillFor, PublishDate)


# Combine offshore and onshore
rig_counts_full <- bind_rows(off_weekly, on_weekly)

# New column for matching in JA
rig_counts_full <- unite(rig_counts_full, location, name, state, remove = FALSE)
rig_counts_full <- unite(rig_counts_full, display_name, name, state, sep = ", ")

# Fill spaces with '-'
rig_counts_full <- rig_counts_full %>% mutate(location = str_replace(location, " ", "-"))

# Spread and fill in NA holders with 0
rig_counts_full <- rig_counts_full %>%
 spread(DrillFor, RigCount) %>%
 mutate_at(vars(Gas:Oil), function(x) if_else(is.na(x), 0L, x))

# Arrange 
rig_counts_full <- rig_counts_full %>%
 arrange(PublishDate, location)

write_csv(rig_counts_full, "data/rig_counts/d3/rig-counts-full.csv")
