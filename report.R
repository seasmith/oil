
# Dependencies ------------------------------------------------------------


lib <- packrat::packrat_lib()
.libPaths(lib)
library(httr)
library(rvest)
library(rmarkdown)


# Script ------------------------------------------------------------------

url <- "http://phx.corporate-ir.net/phoenix.zhtml?c=79687&p=irol-reportsother"

previous_pivot_link <- readLines("data/links/previous_pivot_link.txt")
previous_count_link <- readLines("data/links/previous_count_link.txt")
counter <- 1

repeat {

 if (counter > 120 ) break
 
 h <- read_html(url)
 print("reading html")
 links <- html_nodes(h, xpath = '//a[contains(@class, "ccbnTblLnk")]')
 links_text <- html_text(links)
 
 which_pivot <- grepl("North America Rotary Rig Count Pivot Table \\(Feb 2011 - Current\\)\\s?",
                      links_text)
 
 which_count <- grepl("North America Rotary Rig Count \\(Jan 2000 - Current\\)\\s?",
                      links_text)
 
 pivot_link <- html_attr(links[which_pivot], "href")
 count_link <- html_attr(links[which_count], "href")
 
 if (pivot_link != previous_pivot_link & count_link != previous_count_link) break
 if (counter > 120) break 
 counter <- counter + 1
 Sys.sleep(60)
 next
 
}

print("finished reading html")

if (counter > 120) {
 
 stop("Max timeout reached. Report did not compile.")
 
} else {

 # Write new previous_*_link variables
 ppl_con <- file("data/links/previous_pivot_link.txt", open = "w")
 pcl_con <- file("data/links/previous_count_link.txt", open = "w")
 
 write(pivot_link, ppl_con)
 write(count_link, pcl_con)
 
 close(ppl_con)
 close(pcl_con)
 
 # Move previous data to data/rig_counts/previous_week
 f_move <- grepl("\\.csv$|\\.xlsb$", list.files("data"))
 
 file.rename(file.path("data/rig_counts", f_move),
             file.path("data/rig_counts/previous_week", f_move))
 
 
 # Download new data
 pt_con <- file(pt_file <- "data/rig_counts/pivot_table.xlsb", open = "w")
 ct_con <- file(ct_file <- "data/rig_counts/count_table.xlsb", open = "w")
 
 download.file(pivot_link, pt_file, mode = "wb")
 download.file(count_link, ct_file, mode = "wb")
 
 close(pt_con)
 close(ct_con)
 
 # Convert and get data
 system("cscript scripts/tidy/rig-counts/ConvertToCSV.vbs")
 system("Rscript scripts/tidy/rig-counts/csv-to-RData.R")
 
 render("scripts/pubs/index.Rmd",  output_file = "~/R/oil/pubs/index.html")
 render("scripts/pubs/index2.Rmd", output_file = "~/R/oil/pubs/index2.html")
 
}

# Send data to OilExplorer (should this be a CSV?)
file.copy("data/rig_counts/rc_master.RData", "OilExplorer/data")
file.copy("data/rig_counts/rc_master.csv", "OilExplorer/data")
