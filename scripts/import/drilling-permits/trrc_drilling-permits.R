library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(fs)



# NOTES -------------------------------------------------------------------

# rvest form allows only one county submission at a time. Need to check into
# this.

# Once submitted, data is in 'table.DataGrid'.

# There are at least 13 non-data 'tr' nodes.
# There will be at most 23 (10 data filled 'tr' nodes per page).
# Data starts on the 4th 'tr' node and every other node is the next data node.

# General xpath query form: "//table[@class='DataGrid']//tr//td[n]" for the nth
# column.



# FUNS --------------------------------------------------------------------

get_result_set <- function(html, xpath) {
 x <- html_nodes(html, xpath = xpath)
 x <- html_text(x)
 x <- str_extract(x, "\\d* results") 
 x <- str_extract(x, "\\d*")
 as.integer(x)
}

is_not_empty <- compose(`!`, is_empty)


# LOAD DATA ---------------------------------------------------------------

root_path <- getwd()
load(file.path("data/accidents/tidied/pcd_date_county.RData"))



# GET FORM ----------------------------------------------------------------

base_url <- "http://webapps2.rrc.texas.gov"
url      <- "http://webapps2.rrc.texas.gov/EWA/drillingPermitsQueryAction.do"
drill_session <- html_session(url)
drill_form    <- html_form(drill_session)[[1L]]



# EXTRACT COUNTY OPTION VALUES --------------------------------------------

county_codes <- drill_session %>%
 read_html() %>%
 html_nodes("#countyCodeHndlr\\:1004") %>%
 html_nodes("option") %>%
 {
  tibble(
   value = html_attr(., "value"),
   label = html_text(.)
  )
 }



# GET VALUES --------------------------------------------------------------

selected_codes <- pcd_date_county %>%
 distinct(county) %>%
 pull() %>%
 str_to_upper() %>%
 {tibble(counties = .)}

selected_codes <- selected_codes %>%
 inner_join(county_codes, by = c("counties" = "label"))

date_range <- pcd_date_county %>%
 pull(crash_date) %>%
 unique() %>%
 range()

start_dates <- seq.Date(date_range[1], date_range[2], by = "1 months")
start_dates <- c(start_dates, max(date_range) + days_in_month(max(date_range)) - 1)
start_dates <- unname(start_dates)
end_dates <- start_dates[-1L] - 1
range_groups <- sort(c(start_dates, end_dates[-length(end_dates)])) %>%
 format("%m/%d/%Y") %>%
 split(ceiling(seq_along(.) / 2))



# CREATE XPATHS -----------------------------------------------------------

x_path_api_no <- "//table[@class='DataGrid']//tr//td[1]//td[@width='50%']//a"
x_path_district <- "//table[@class='DataGrid']//tr//td[@style='text-align:center']"
x_path_lease <- "//table[@class='DataGrid']//tr//td[3]//a"

x_path_n <- function(n) sprintf("//table[@class='DataGrid']//tr//td[%s]", n)

# Look-ahead and get next page
x_path_result_set <- "//td[@class='PagerBanner']//td[1]"
x_path_next <- "//td[@class='PagerBanner']//a[contains(text(), 'Next')]"




# SCRAPE DATA -------------------------------------------------------------

drill_permits <- selected_codes$value %>%
 map_df(~{
  map_df(range_groups, value = .x, function(.x, value) {
   print(sprintf("%s (%s)", selected_codes$counties[selected_codes$value == value], .x))
   drill_form_filled <- drill_form %>%
    set_values(
     searchArgs.countyCodeHndlr.selectedCodes = value,
     searchArgs.approvedDtFromHndlr.inputValue = .x[1],
     searchArgs.approvedDtToHndlr.inputValue = .x[2]
    )
   
   drill_form_submitted <- submit_form(drill_session, drill_form_filled)
   drill_html <- read_html(drill_form_submitted)
   
   result_set <- get_result_set(drill_html, x_path_result_set)
   
   if (result_set %>% is_not_empty()) {
    
    result_pages <- (result_set %/% 10) + (result_set %% 10)
    
    # Iterator for each column
    it_char <- as.character(seq(1, 14))
    
    range_group_df <- map_df(seq_len(result_pages), ~{
     
     # Choose each xpath for each column
     data <- map(it_char, ~{
      x_path <- switch(
       .x, # it_char
       `1` = x_path_api_no,
       `2` = x_path_district,
       `3` = x_path_lease,
       `4` =,
       `5` =,
       `6` =,
       `7` =,
       `8` =,
       `9` =,
       `10` =,
       `11` =,
       `12` =,
       `13` =,
       `14` = x_path_n(.x),
      )
      
      # Return html text data for particular column
      html_text(html_nodes(drill_html, xpath = x_path))
      
     })
     
     if (.x != result_pages) {
      drill_html <<- read_html(
       paste0(base_url, 
              html_attr(
               html_nodes(drill_html, xpath = x_path_next),
               "href"
              )
       )
      )
      Sys.sleep(1)
     }
     
     bind_cols(data)
     
    })
    
    Sys.sleep(sample(1:10, 1))
    range_group_df
    
   } else {
    
    Sys.sleep(sample(1:10, 1))
    NULL
    
   }
  })
 })



# SET COLUMN NAMES --------------------------------------------------------

drill_permits <- drill_permits %>%
 set_names(c("api_no", "district", "lease", "well_number", "permitted_operator",
             "county", "status_date", "status_number", "wellbore_profiles",
             "filing_purpose", "amend", "total_depth",
             "stacked_lateral_parent_well_dp_no", "status"))



# SAVE RAW DATA -----------------------------------------------------------

save(drill_permits, file = file.path(root_path, "data/drilling-permits/raw/drill_permits.RData"))
