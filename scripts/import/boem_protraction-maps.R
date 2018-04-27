dwn_and_unz <- function(url, dest) {
 curl::curl_download(url, dest)
 unzip(dest, exdir = tools::file_path_sans_ext(dest))
}


# main url: https://www.data.boem.gov/Main/Mapping.aspx
# Datum used: https://www.boem.gov/uploadedFiles/BOEM/Oil_and_Gas_Energy_Program/Mapping_and_Data/NADstatus.pdf

# Just need 'protractions' for now.

# Gulf of Mexico
# https://www.boem.gov/GOM-Official-Protraction-Diagrams/
url <- "https://www.data.boem.gov/Mapping/Files/protrac.zip"
dest <- "~/R/oil/data/maps/gom_protrac.zip"
dwn_and_unz(url, dest)


# Pacific
url <- "https://www.boem.gov/Oil-and-Gas-Energy-Program/Mapping-and-Data/Pacific-files/PC_PROTLMT.aspx"
dest <- "~/R/oil/data/maps/pac_protrac.zip"
dwn_and_unz(url, dest)

url <- "https://www.boem.gov/Oil-and-Gas-Energy-Program/Mapping-and-Data/Pacific-files/PC_PROT_CLIP.aspx"
dest <- "~/R/oil/data/maps/pac_protrac_clipped.zip"
dwn_and_unz(url, dest)


# Alaska
url <- "http://www.boem.gov/Oil-and-Gas-Energy-Program/Mapping-and-Data/Alaska/protrac.aspx"
dest <- "~/R/oil/data/maps/ak_protrac.zip"
dwn_and_unz(url, dest)

url <- "http://www.boem.gov/Oil-and-Gas-Energy-Program/Mapping-and-Data/Alaska/protclip.aspx"
dest <- "~/R/oil/data/maps/ak_protrac_clipped.zip"
dwn_and_unz(url, dest)
