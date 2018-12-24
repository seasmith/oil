
# MISC --------------------------------------------------------------------

`%not_in%` <- purrr::compose(`!`, `%in%`)


# CATEGORY FUNCTIONS ------------------------------------------------------



# Form an API category call from an input 'id'
set_cat_url <- function(id) {
 httr::modify_url("http://api.eia.gov",
            path = "category",
            query = list(api_key = getOption("api_key_eia"),
                         category_id = id))
}

# Get the associated JSON data (after reading the HTML)
get_cat_json <- function(html) {
 jsonlite::fromJSON(rvest::html_text(html))
}

# If child categories exist, get those categories
get_child_categories <- function(json) {
 if (purrr::is_empty(x <- json$category$childcategories)) NULL else x
}




# SERIES FUNCTIONS --------------------------------------------------------

set_series_url <- function(id) {
 httr::modify_url("http://api.eia.gov",
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
