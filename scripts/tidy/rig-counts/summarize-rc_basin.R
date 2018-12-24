library(dplyr)
library(purrr)
library(tidyr)

# Path to rig count data
rc_path <- "~/R/oil/data/rig_counts"

# Load data into global env
load(file.path(rc_path, "rc_basin.RData"))
# load(file.path(rc_path, "rc_master.RData"))  # I don't even use this

# The dates I am interested in
nms <- c("this_week", "week_ago", "year_ago")

key_dates <- rc_basin$Date %>%
 unique() %>%
 sort(decreasing = TRUE) %>%
 .[c(1, 2, 53)] %>%
 set_names(nms)

# Find most recent week's change from previous week; Total
total_comparison <- rc_basin %>%
 group_by(basin, key) %>%
 arrange(Date) %>%
 mutate(net_1wk   = value - lag(value, 1L),
        pct_1wk  = net_1wk / lag(value),
        net_52wk  = value - lag(value, 52L),
        pct_52wk = net_52wk / lag(value, 52L)) %>%
 ungroup() %>%
 filter(Date %in% c(key_dates[c("this_week", "week_ago")])) # "year_ago"

# Find most recent week's oil and gas type
type_comparison <- total_comparison %>%
 filter(Date == max(Date)) %>%
 filter(key %in% c("Gas", "Oil")) %>%
 select(basin:value) %>%
 spread(key, value) %>%
 mutate(prcnt_oil = Oil / (Oil + Gas))

# Get totals and just the current week.
# Remove NaN's and NA's for plotting purposes.
this_week <- total_comparison %>%
 filter(Date == key_dates[["this_week"]]) %>%
 filter(key == "Total") %>%
 mutate_at(vars(pct_1wk, pct_52wk),
           function(x) if_else(is.na(x), 0, x))

# Create levels vector
tw_levels <- this_week %>%
 arrange(desc(pct_1wk), basin) %>%
 .$basin

# Assign levels
this_week <- this_week %>%
 mutate(basin = factor(basin, levels = tw_levels))

# Weekly summary
summ <- rc_basin %>%
 group_by(key, Date) %>%
 summarize(value = sum(value, na.rm = TRUE)) %>%
 ungroup() %>%
 filter(key != "Misc")

##  Summarize rc_master
# Total rig count by `Year` and `Week`
diff <- total_comparison %>%
 group_by(Date, key) %>%
 summarize(value = sum(value, na.rm = TRUE)) %>%
 ungroup() %>%
 spread(key, value)
