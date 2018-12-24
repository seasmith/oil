library(dplyr)
library(stringr)
library(tidyr)

root_path <- getwd()

load(file.path(root_path, "data/drilling-permits/raw/drill_permits.RData"))

drill_permits <- drill_permits %>%
 # lease
 mutate(
  lease = lease %>% 
   str_remove_all("\\r") %>%
   str_remove_all("\\n") %>%
   str_trim() %>%
   str_replace_all("\\s+", " ")) %>%
 # well_number
 mutate(
  well_number = well_number %>%
   str_trim()
 ) %>%
 # permitted_operator
 separate(permitted_operator, c("operator", "operator_id"), sep = "\\(") %>%
 # status_date
 mutate(
  status_date = status_date %>%
   str_remove_all("\\r\\n\\s+") %>%
   str_remove("^Submitted: ")
 ) %>%
 separate(status_date, c("submitted", "approved"), "Approved: ") %>%
 mutate_at(
  vars(submitted, approved), function(x) str_trim(x) %>% as.Date(format = "%m/%d/%Y")
 )

save(drill_permits, file = file.path(root_path, "data/drilling-permits/tidied/drill_permits.RData"))
