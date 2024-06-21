library(rmarkdown)

# Code to build each of the trackers
# Includes loading pre-processed and stored dfs
# Grouped by each page to allow for individual or mass processing

# MURDERS
# Load RDS
murders_district <- readRDS("scripts/rds/murders_district.rds")
murders_city <- readRDS("scripts/rds/murders_city.rds")
asofdate <- readRDS("scripts/rds/asofdate.rds")
# Render page
rmarkdown::render('scripts/Philadelphia_Safety_Tracker.Rmd', 
                  output_dir = "docs",
                  output_file = 'Philadelphia_Safety_Tracker.html')