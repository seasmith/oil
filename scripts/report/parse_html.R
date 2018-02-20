
# Dependencies ------------------------------------------------------------


lib <- packrat::packrat_lib()
.libPaths(lib)
library(httr)
library(rvest)


# Script ------------------------------------------------------------------


h <- "page.html" %>%
 read_html()

links <- h %>%
 html_nodes(xpath = '//a[contains(@class, "ccbnTblLnk")]')

links_text <- links %>%
 html_text()

which_pivot <- links_text %>%
 grepl("North America Rotary Rig Count Pivot Table \\(Feb 2011 - Current\\)\\s?", .)

which_count <- links_text %>%
 grepl("North America Rotary Rig Count \\(Jan 2000 - Current\\)\\s?", .)


pivot_link <- html_attr(links[which_pivot], "href")
count_link <- html_attr(links[which_count], "href")

# pivot_query <- parse_url(pivot_link)$query[[1]]
# count_query <- parse_url(count_link)$query[[1]]

pl_con <- file("pivot_link.txt", open = "w")
cl_con <- file("count_link.txt", open = "w")

write(pivot_link, pl_con)
write(count_link, cl_con)

close(pl_con)
close(cl_con)
