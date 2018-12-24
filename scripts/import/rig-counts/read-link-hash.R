
# Dependencies ------------------------------------------------------------


lib <- packrat::packrat_lib()
.libPaths(lib)
library(httr)
library(rvest)


# Script ------------------------------------------------------------------


h <- read_html("page.html")

links <- html_nodes(h, xpath = '//a[contains(@class, "ccbnTblLnk")]')

links_text <- html_text(links)

which_pivot <- grepl("North America Rotary Rig Count Pivot Table \\(Feb 2011 - Current\\)\\s?", links_text)
which_count <- grepl("North America Rotary Rig Count \\(Jan 2000 - Current\\)\\s?", links_text)

pivot_link <- html_attr(links[which_pivot], "href")
count_link <- html_attr(links[which_count], "href")

# pivot_query <- parse_url(pivot_link)$query[[1]]
# count_query <- parse_url(count_link)$query[[1]]

pl_con <- file("~/R/oil/data/links/pivot_link.txt", open = "w")
cl_con <- file("~/R/oil/data/links/count_link.txt", open = "w")

write(pivot_link, pl_con)
write(count_link, cl_con)

close(pl_con)
close(cl_con)
