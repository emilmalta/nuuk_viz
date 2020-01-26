# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(RCurl)
library(here)

# Connect to FTP server, and download all shp data -----------------------------

# FTP url
url <- "ftp://Asiaqs_grundkort:%20@ftp.asiaq.gl/06_NUK/0600_NUK/SHP/"

# Downloading the whole thing is ~750 MB. Inspect files first.
getURL(url) %>% 
  writeLines(here("data/map_overview.txt"))

overview <- read.table(here("data/map_overview.txt")) %>% 
  select(file = ncol(.), filesize = V5)

# Select which files to download -----------------------------------------------

# Limit filesizes to 5MB
to_download <- overview %>% 
  count(
    feature = str_sub(file, end = -5), 
    wt = filesize, 
    sort = T) %>% 
  filter(n < 5000000) %>% 
  pull(feature)

# Pull to vector
filenames <- overview %>% 
  filter(
    str_sub(file, end = -5) %in% to_download,
  ) %>% 
  pull(file)

# Download all the things ------------------------------------------------------

# This will download ~550 files to mapdata folder (~ 25 MB)
for(file in filenames){
  download.file(
    url = paste0(url, file),
    destfile = paste0(here("data/mapdata/"), file)
  )
}
