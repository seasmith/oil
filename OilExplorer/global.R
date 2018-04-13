
# DEPENDENCIES ------------------------------------------------------------

library(shiny)
packrat::extlib("shinydashboard")  # change this once I get the internet back
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(sf)
library(extrafont); loadfonts("win", quiet = TRUE)
library(nord)
packrat::extlib("zeallot")

# SET MAP THEME UTILITIES -------------------------------------------------

theme_set(blg::blg_theme_default())

pretty_date <- function(n) format(n, "%B %d, %Y")


# LOAD RIG COUNT DATA -----------------------------------------------------

rc_master <- readr::read_csv("data/rc_master.csv")
load("data/counties_2014_adjusted_simp.RData")
load("data/states_map_adjusted_simp.RData")
load("data/sf_plays.RData")
sf_plays <- st_transform(sf_plays, st_crs(states_map_adjusted_simp))

# TIDY RIG COUNT DATA -----------------------------------------------------

# Incompatible with map county names
rc_master <- rc_master %>%
 filter(grepl("Oil|Gas", DrillFor)) %>%  # !!! Place in pre-load script
 filter(Country == "UNITED STATES") %>%  # !!! Place in pre-load script
 mutate(County = str_replace(County, "^ST\\.", "SAINT"),
        County = str_replace(County, "DE WITT", "DEWITT"))


# CHECK RIG COUNT DATA ----------------------------------------------------

# This should be done in a script separate from this app
# no_join_check <- rc_master %>%
#  filter(Location == "Land") %>%
#  filter(Country == "UNITED STATES") %>%
#  filter(PublishDate == max(PublishDate)) %>%
#  group_by(`State/Province`, County) %>%
#  summarize(n = n()) %>%
#  ungroup() %>%
#  anti_join(counties_2014_adjusted_simp, c("County" = "NAME", "State/Province" = "state_name"))
# 
# if (nrow(no_join_check) > 0) stop("Some rows did not join. Check anti_join for more details.", call. = FALSE)


# LOAD EIA REGIONS --------------------------------------------------------

# !!! Place in pre-load script
# EIA Region Counties
cols <- c("state", "county", "state_id", "county_id", "region")
xl_path <- "../data/maps/eia_drilling_report.xlsx"
sh_names <- readxl::excel_sheets(xl_path)
xl_regions <- readxl::read_excel(xl_path, sh_names[sh_names == "RegionCounties"], NULL, cols, c(rep("text", 2), "numeric", rep("text", 2)), skip = 1L)
