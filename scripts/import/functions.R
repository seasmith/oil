library(rvest)
library(httr)
library(jsonlite)
library(stringr)
library(purrr)



# GENERAL PURPOSE FUNCTIONS -----------------------------------------------

# Negation of purrr::is_empty()
is_not_empty <- purrr::negate(purrr::is_empty)

# Negation of %in%
`%not_in%` <- purrr::compose(`!`, `%in%`)



#   -----------------------------------------------------------------------
# TRRC FUNCTIONS ----------------------------------------------------------
#   -----------------------------------------------------------------------


get_result_set <- function(html, xpath) {
 x <- html_nodes(html, xpath = xpath)
 x <- html_text(x)
 x <- str_extract(x, "\\d* results") 
 x <- str_extract(x, "\\d*")
 as.integer(x)
}



#   -----------------------------------------------------------------------
# EIA FUNCTIONS -----------------------------------------------------------
#   -----------------------------------------------------------------------



# CATEGORY FUNCTIONS ------------------------------------------------------



# Form an API category call from an input 'id'
set_cat_url <- function(id) {
 modify_url("http://api.eia.gov",
            path = "category",
            query = list(api_key = getOption("api_key_eia"),
                         category_id = id))
}

# Get the associated JSON data (after reading the HTML)
get_cat_json <- function(html) {
 fromJSON(html_text(html))
}

# If child categories exist, get those categories
get_child_categories <- function(json) {
 if (is_empty(x <- json$category$childcategories)) NULL else x
}




# SERIES FUNCTIONS --------------------------------------------------------

set_series_url <- function(id) {
 modify_url("http://api.eia.gov",
            path = "series",
            query = list(api_key = getOption("api_key_eia"),
                         series_id = id))
}

get_series_json <- function(html) get_cat_json(html)

get_series_meta <- function(json) {
 json$series[, names(json$series) %not_in% "data"]
}

get_series_data <- function(json) {
 as_tibble(json$series[, names(json$series) %in% "data"][[1]])
}



# EXTRACTION FUNCTIONS ----------------------------------------------------

# Get elements by names; alternative to using purrr::safely(`[[`, "regex-here")
get_element_by_regex <- function(list, regex) {
 function(x) {
  which_el <- which(stringr::str_detect(names(x), regex))
  if (is_not_empty(which_el)) x[[which_el]] else NULL 
 }
}

# Get elements from 'content' (extracted using httr::content())
get_content_request <- get_element_by_regex(x, "^request$")
get_content_cat <- get_element_by_regex(x, "^category$")
get_content_child <- get_element_by_regex(x, "^childseries$")
