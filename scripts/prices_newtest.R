library(tidyverse)
library(blsAPI)
library(jsonlite)
library(dplyr)

# WORK TO BE DONE
# AUTOMATING USING THE MOST RECENT MONTHLY DATA AND THE SAME FOR PRIOR YEARS

# Load bls series data from data folder
series <- read_tsv("data/ap.series.txt") %>% filter(begin_year < 2019 & end_year > 2023) %>% filter(area_code == "0000" )
# download.file("https://download.bls.gov/pub/time.series/ap/ap.data.0.Current","raw_prices.txt")

library(httr)

url <- "https://download.bls.gov/pub/time.series/ap/ap.data.0.Current"
destfile <- "latest_prices.txt"

response <- GET(url, 
                add_headers(
                  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3",
                  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                  `Accept-Language` = "en-US,en;q=0.5",
                  `Connection` = "keep-alive"
                ), 
                write_disk(destfile, overwrite = TRUE))

if (status_code(response) == 200) {
  message("File downloaded successfully")
} else {
  message("Failed to download file: ", status_code(response))
}

# Access the API key from an environment variable
key <- Sys.getenv("BLS_API_KEY")

# Define series IDs and corresponding item names
series_ids <- c('APU0000709112', 'APU0000704111', 'APU0000711211', 'APU0000703112', 'APU0000FF1101', 
                'APU0000702111', 'APU0000708111', 'APU0000702421', 'APU0000703511', 'APU0000712112', 
                'APU0000717311', 'APU000072620', 'APU0000FN1101', 'APU0000710212', 'APU000072610', 
                'APU0000713111', 'APU0000FJ4101', 'APU0000712311','APU0000720311','APU0000711415',
                'APU0000701312', 'APU0000710411', 'APU0000FS1101', 'APU0000701322')
item_names <- c("Milk (half gallon)", "Bacon (pound)", "Bananas (pound)", "Ground Beef (pound)", 
                "Chicken Breast (pound)", "Loaf of Bread", "Dozen eggs", "Cookies", "Steak (pound)", 
                "Potatoes (pound)", "Coffee (pound)", "Utility gas", "2-liter soft drink", 
                "Cheddar Cheese (pound)", "Electricity (kw hour)", "Frozen orange juice", 
                "Yogurt (8 ounces)", "Tomatoes (pound)", "Table Wine", "Strawberries (pint)", "Rice (pound)",
                "Ice Cream (half gallon)","Butter (pound)","Spaghetti and Macaroni")

# Pull the data via the API
payload <- list(
  'seriesid'  = series_ids,
  'startyear' = 2019,
  'endyear'   = 2024
)
response <- blsAPI(payload, 2)
json     <- fromJSON(response)

# Initialize an empty list to store the results
prices_list <- vector("list", length(series_ids))

# Loop through each series ID and corresponding item name
for (i in seq_along(series_ids)) {
  # Extract the data and convert it to a data frame
  prices_data <- as.data.frame(json[["Results"]][["series"]][["data"]][[i]])
  
  # Apply select() and mutate() to the data frame
  prices_data <- prices_data %>%
    select(-6) %>%
    mutate(item = item_names[i])
  
  # Store the modified data frame in the list
  prices_list[[i]] <- prices_data
}

# Combine all data frames into one table
prices <- bind_rows(prices_list)
# Convert item column to upper case
prices$item <- toupper(prices$item)

# pivot the table with dates on the rows and items in the columns
# Set to always grab the latest month
prices_pivot <- prices %>% filter(period==prices$period[1]) %>% pivot_wider(names_from = item, values_from = value)

# Create a check value for the period that is the latest month
latest_month_price <- prices %>%
  filter(latest == "true") %>%
  select(period) %>%
  group_by(period) %>%
  slice(1) %>%
  pull(period) %>%
  first()

# Fix coffee price problem 
# Step 1: Find the closest available 2019 M10 value for "Coffee (pound)" from the `prices` table
coffee_2019_M10_value <- prices$value[which(prices$item == "COFFEE (POUND)" & prices$year == "2019" & prices$period == "M10")]

# Ensure there's only one value to avoid potential issues
if(length(coffee_2019_M10_value) == 1) {
  # Step 2: Identify rows in `prices_pivot` where year is 2019 and "Coffee (pound)" is NA
  rows_to_update <- which(prices_pivot$year == "2019" & is.na(prices_pivot$`COFFEE (POUND)`))
  
  # Step 3: Update those rows with the 2019 M10 value for "Coffee (pound)"
  if(length(rows_to_update) > 0) { # Check if there are any rows to update
    prices_pivot$`COFFEE (POUND)`[rows_to_update] <- coffee_2019_M10_value
  }
} else {
  warning("Multiple or no values found for Coffee (pound) for 2019 M10. No updates made.")
}

# pivot the table items in rows the dates in columns
prices_pivot2 <- prices %>% filter(period==prices$period[1]) %>% select(-latest,-period,-periodName) %>% pivot_wider(names_from = year, values_from = value)
# Reorder the columns so that columns 2-6 are in proper order, sorted by number
prices_pivot2 <- prices_pivot2 %>% select(1,7,6,5,4,3,2)

# Fix coffee data in 2nd table
#Ensure there's only one value to avoid potential issues
if(length(coffee_2019_M10_value) == 1) {
  # Step 2: Identify rows in `prices_pivot` where year is 2019 and "Coffee (pound)" is NA
  rows_to_update <- which(prices_pivot2$item == "COFFEE (POUND)" & is.na(prices_pivot2$`2019`))
  
  # Step 3: Update those rows with the 2019 M10 value for "Coffee (pound)"
  if(length(rows_to_update) > 0) { # Check if there are any rows to update
    prices_pivot2$`2019`[rows_to_update] <- coffee_2019_M10_value
  }
} else {
  warning("Multiple or no values found for COFFEE (POUND) for 2019 M10. No updates made.")
}

# Change columns 2-7 to numeric
prices_pivot2[,2:7] <- sapply(prices_pivot2[,2:7], as.numeric)
# Add column for percentage increase between 2019 and 2024
prices_pivot2 <- prices_pivot2 %>% mutate(percent_increase = ((`2024` - `2019`)/`2019`)*100)
# Round percentage increase to 0 decimal places
prices_pivot2$percent_increase <- round(prices_pivot2$percent_increase, 0)


# export prices as csv
write_csv(prices, "data/prices.csv")
write_csv(prices_pivot, "data/prices_pivot.csv")
write_csv(prices_pivot2, "data/prices_pivot_table.csv")

# Convert the date to the desired format
cpi_date <- prices$periodName[1]

# Create the description string
description <- paste("Hover over charts to see the average nationwide price as of the end of ", cpi_date,".",sep="")

# Create a list to represent the JSON structure
json_data <- list(
  describe = list(
    intro = description
  )
)

# Convert the list to JSON format
json_string <- toJSON(json_data, pretty = TRUE)

# Write the JSON string to a file
write(json_string, file = "data/cpi_update.json")



# Notes on data sources / links
# 0000	is U.S. city average
# S	Seasonally Adjusted
# U	Not Seasonally Adjusted

# URLS for the latest data
#url_food <- "https://download.bls.gov/pub/time.series/ap/ap.data.3.Food"
#url_gas <- "https://download.bls.gov/pub/time.series/ap/ap.data.2.Gasoline"
#url_current <- "https://download.bls.gov/pub/time.series/ap/ap.data.0.Current"
#url_item <- "https://download.bls.gov/pub/time.series/ap/ap.item"
#url_period <- "https://download.bls.gov/pub/time.series/ap/ap.period"