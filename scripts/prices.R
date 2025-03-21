library(tidyverse)
library(blsAPI)
library(jsonlite)
library(dplyr)
library(devtools)

# Load bls series data from data folder
series <- read_tsv("data/ap.series.txt") %>% filter(begin_year < 2019 & end_year > 2023) %>% filter(area_code == "0000" )

# Access the API key from an environment variable
#key <- Sys.getenv("BLS_API_KEY")
#key <- "cf3e40aa46974c2da9d82c28b56a8d6a"

# Define series IDs and corresponding item names
series_ids <- c('APU0000709112', 'APU0000704111', 'APU0000711211', 'APU0000703112', 'APU0000FF1101', 
                'APU0000702111', 'APU0000708111', 'APU0000702421', 'APU0000703511', 'APU0000712112', 
                'APU0000717311', 'APU000072620', 'APU0000FN1101', 'APU0000710212', 'APU000072610', 
                'APU0000713111', 'APU0000FJ4101', 'APU0000712311','APU0000720311','APU0000711415',
                'APU0000701312', 'APU0000710411', 'APU0000FS1101', 'APU0000701322',
                'APU0000715211', 'APU0000704312', 'APU0000714221', 'APU0000FD3101',
                'APU0000711412', 'APU0000701111', 'APU0000711411', 'APU0000712211',
                'APU0000711311', 'APU0000714233', 'APU0000720111', 'APU0000710211',
                'APU0000703432', 'APU0000703311', 'APU0000702212', 'APU0000FN1102')
item_names <- c("Milk (half gallon)", "Bacon", "Bananas", "Ground Beef", 
                "Chicken Breast", "White Bread", "Dozen eggs", "Cookies", "Steak", 
                "Potatoes", "Coffee", "Utility gas", "Soft Drinks (2 liter)",
                "Cheddar Cheese", "Electricity (kw hour)", "Frozen orange juice", 
                "Yogurt (8 ounces)", "Tomatoes", "Table Wine", "Strawberries (pint)", "Rice",
                "Ice Cream (half gallon)","Butter","Spaghetti and Macaroni",
                "Sugar","Boneless Ham","Canned corn","Pork Chops",
                "Lemons","Flour","Grapefruit","Lettuce",
                "Oranges","Dried Beans","Malt beverages","Processed American cheese",
                "Beef for Stew","Round roast","Wheat bread","Soft Drinks (12 pack)")

# Function to split a vector into chunks
split_into_chunks <- function(vec, chunk_size) {
  split(vec, ceiling(seq_along(vec) / chunk_size))
}

# Split series_ids into chunks of 25
series_chunks <- split_into_chunks(series_ids, 25)
item_chunks <- split_into_chunks(item_names, 25)

# Initialize an empty list to store the results
prices_list <- list()

# Loop through each chunk and make API calls
for (chunk_index in seq_along(series_chunks)) {
  chunk_series_ids <- series_chunks[[chunk_index]]
  chunk_item_names <- item_chunks[[chunk_index]]
  
  # Pull the data via the API
  payload <- list(
    'seriesid'  = chunk_series_ids,
    'startyear' = 2019,
    'endyear'   = 2025
  )
  
  response <- blsAPI(payload, 2)
  json <- fromJSON(response)
  
  # Loop through each series ID and corresponding item name in the chunk
  for (i in seq_along(chunk_series_ids)) {
    # Extract the data and convert it to a data frame
    prices_data <- as.data.frame(json[["Results"]][["series"]][["data"]][[i]])
    
    # Apply select() and mutate() to the data frame
    prices_data <- prices_data %>%
#      select(-6) %>%
      mutate(item = chunk_item_names[i])
    
    # Store the modified data frame in the list
    prices_list <- append(prices_list, list(prices_data))
  }
}

# START HERE (MINIMIZE RUNS OF THE ABOVE CODE TO PREVENT API LIMITS)

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

# pivot the table items in rows the dates in columns
prices_pivot2 <- prices %>% filter(period==prices$period[1]) %>% select(-latest,-period,-periodName) %>% pivot_wider(names_from = year, values_from = value)
# Reorder the columns so that columns 2-6 are in proper order, sorted by number
prices_pivot2 <- prices_pivot2 %>% select(1,9,8,7,6,5,4,3,2)

# Change columns 2-7 to numeric
prices_pivot2[,2:7] <- sapply(prices_pivot2[,2:7], as.numeric)
# Add column for percentage increase between 2019 and 2024
prices_pivot2 <- prices_pivot2 %>% mutate(percent_increase = ((`2024` - `2019`)/`2019`)*100)
# Round percentage increase to 0 decimal places
prices_pivot2$percent_increase <- round(prices_pivot2$percent_increase, 0)
# Sort pivot2 alphabet
prices_pivot2 <- prices_pivot2 %>% arrange(item)

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


