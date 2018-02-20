## ----setup_lib_std, include=FALSE----------------------------------------
# ____ Library_Tidyverse_Setup
library(tidyverse)
library(lubridate)
library(forcats)

# ____ Library_Web_Setup
library(rvest)
library(jsonlite)

## ----csv_read------------------------------------------------------------
f_csv <- list.files("data/rig_counts", full.names = TRUE)

## ----tidy_state_split----------------------------------------------------
rx_state <- "by State"
f_state <- grep(rx_state, f_csv, value = TRUE)
print("here")
print(f_state)
rc_state <- read_csv(f_state, skip = 5L)

# Split columns between non-placeholder column names
# (i.e. where the column name is not blank and placed
# with something like X1, X22, etc)
col_match <- !grepl("^X\\d", names(rc_state))
split_match <- split(names(rc_state), cumsum(col_match))
names(split_match) <- map(split_match, ~.x[1])

# Gather columns and bind by row
rc_state <- split_match[-c(1L)] %>%
    map(~{
        h <- select(rc_state, 1, .x)
        names(h) <- as.character(h[1, ])
        h[-1L, ]
        }) %>%
    bind_rows(.id = "location")

# Last column is not needed (if all values are NA)
condition <- sum(!is.na(rc_state[, ncol(rc_state)])) == 0
if (condition) rc_state <- rc_state[, -ncol(rc_state)]

# NA's at bottom are useless
rc_state <- rc_state %>% filter(!is.na(DATE))

# Coerce to right data type
rc_state <- rc_state %>%
    mutate_at(vars(Land, Offshore, TOTAL), as.integer) %>%
    mutate_at(vars(DATE), as.Date, format = "%m/%d/%Y")

# Get rid of NA's and add both `land` and `offshore`
rc_state <- rc_state %>%
  mutate_at(vars(Land, Offshore), ~if_else(is.na(.x), 0L, .x)) %>%
  mutate(total = Land + Offshore)
  
# Keep case consistent
names(rc_state) <- tolower(names(rc_state))

## ----save_state_split----------------------------------------------------
save(rc_state, file = "data/rig_counts/rc_state.RData")

## ----tidy_og_split-------------------------------------------------------
f_og <- grep("US Oil & Gas", f_csv, value = TRUE)
rc_og <- read_csv(f_og, skip = 6L)

fun_1 <- function(x) str_replace(x, "%$", "")
fun_2 <- function(x) as.numeric(x) / 100

rc_og <- rc_og %>%
  filter(!is.na(Date))

rc_og <- rc_og %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         `% Oil` = fun_1(`% Oil`),
         `% Oil` = fun_2(`% Oil`),
         `% Gas` = fun_1(`% Gas`),
         `% Gas` = fun_2(`% Gas`))

rc_og <- rc_og %>%
  mutate(Oil = case_when(
    is.na(Oil) ~ as.numeric((Total) - (Gas + Misc)),
    TRUE ~ as.numeric(Oil)
  ))

## ----save_og-------------------------------------------------------------
save(rc_og, file = "data/rig_counts/rc_og.RData")

## ----tidy_basin----------------------------------------------------------
# ____ Read basin data
# Find and read file; skip first 10
f_basin <- grep("Basin", f_csv, value = TRUE)
rc_basin <- read_csv(f_basin, skip = 10L)
rc_basin <- rc_basin %>%
  select(-X66) %>% # need to check how many cols and if last is meaningless
  filter(!is.na(Date)) # stupid Excel

# Split by basin and gather variables
rc_basin <- seq.int(2, 65) %>%  # Create seq along columns except `Date`
  split(ceiling(seq_along(.) / 4)) %>%  # Split into groups of 4
  map(~gather(select(rc_basin, 1, .x), ... = -Date))  # Gather the groups of four

# Get names from row 10
basin_names <- read_csv(f_basin, skip = 9L, n_max = 1L)
basin_names <- names(basin_names)[seq.int(2, length(basin_names) - 1L, 4)]

# Assing to the list element names
names(rc_basin) <- basin_names

# Bind everything by row
rc_basin <- rc_basin %>% bind_rows(.id = "basin")

rc_basin <- rc_basin %>%
  mutate(key = case_when(
    grepl("Oil", key) ~ "Oil",
    grepl("Gas", key) ~ "Gas",
    grepl("Misc", key) ~ "Misc",
    grepl("Total", key) ~ "Total"
    )) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(basin != "Total US RigCount")

## ----save_basin----------------------------------------------------------
save(rc_basin, file = "data/rig_counts/rc_basin.RData")

## ----tidy_master---------------------------------------------------------
# ____ Read master data from pivot table
f_master <- grep("Master Data.csv$", f_csv, value = TRUE)

rc_master <- read_csv(f_master)

rc_master <- rc_master %>%
  mutate(Basin = if_else(Basin == "Dj-Niobrara", "DJ-Niobrara", Basin),
         PublishDate = str_replace(PublishDate, "\\s+0\\:00", ""),
         PublishDate = as.Date(PublishDate, format = "%m/%d/%Y"))

## ----save_master---------------------------------------------------------
save(rc_master, file = "data/rig_counts/rc_master.RData")

## ----cleanup, include=FALSE----------------------------------------------
rm(list = ls(pattern = "api_key_"))

