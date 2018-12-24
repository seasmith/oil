#!/bin/bash

# Download webpage and parse it
curl "http://phx.corporate-ir.net/phoenix.zhtml?c=79687&p=irol-reportsother" > page.html
Rscript ../import/rig-counts/read-link-hash.R

# Read `last-modified` fields (parsed by parse_html.R)
pivot_link=$(cat pivot_link.txt)
count_link=$(cat count_link.txt)

# Read the previous `last-modified` fields
previous_pivot_link=$(cat previous_pivot_link.txt)
previous_count_link=$(cat previous_count_link.txt)

counter=0

while [ "$pivot_link" = "$previous_pivot_link" -o "$count_link" = "$previous_count_link" ]
do

  # Don't spend an eternity.
  if [ "$counter" -eq 120 ];
    then
      echo "No update detected."
      break;
  fi

  ((counter+=1))
    
  sleep 60
  
  curl "http://phx.corporate-ir.net/phoenix.zhtml?c=79687&p=irol-reportsother" > page.html
  Rscript ../import/rig-counts/read-link-hash.R

  pivot_link=$(cat pivot_link.txt)
  count_link=$(cat count_link.txt)

done

# Move variables into previous_*_link.txt files 
echo "$pivot_link" > previous_pivot_link.txt
echo "$count_link" > previous_count_link.txt

# Create folder and move old files into it
today=$(date +"%Y-%m-%d")
last_week=$(date --date="${today} - 7 day" +%Y-%m-%d)
mkdir "$last_week"
ls --file-type | grep '\.csv$\|.xlsb$' | xargs -I '{}' mv {} ./"$last_week"

# Download files
curl -o pivot_table.xlsb "$pivot_link" 
curl -o count.xlsb "$count_link" 

# Convert from .xlsb to .csv
cscript ../tidy/rig-counts/ConvertToCSV.vbs

# Extract data
Rscript ../tidy/rig-counts/rig-counts.R

# RMarkdown cannot find pandoc unless it is added to this session's PATH
PATH=${PATH}:~C:/"Program Files"/RStudio/bin/pandoc/pandoc.exe
R -q -e "rmarkdown::render('C:/Users/Luke/R/my_blog/_draft/us_rig_counts.Rmd')"

echo "Update complete."