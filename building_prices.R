library(tidyverse)
library(blsAPI)
library(jsonlite)
library(dplyr)
library(devtools)

# Load bls series data from data folder
producer_series <- read_tsv("data/wp.series") %>% filter(end_year > 2023)
# Filter producer_series to new df with selected producer_series items from building series ids
select_building_series <- producer_series %>% filter(series_id %in% building_series_ids)



# Access the API key from an environment variable
key <- Sys.getenv("BLS_API_KEY")

# Define series IDs and corresponding item names
building_series_ids <- c('WPUIP231100',
                'WPU0811', 
                'WPU137', 
                'WPU1017', 
                'WPU1331',
                'WPU13330101', 
                'WPU102501', 
                'WPU062101',
                'WPU1311',
                'WPU136101')
building_item_names <- c("INPUTS TO RESIDENTIAL CONSTRUCTION",
                         "Softwood Lumber", 
                "Gypsum Products (Drywall)", 
                "Steel Mill Products", 
                "Concrete Block and Brick",
                "Ready-Mix Concrete", 
                "Aluminum", 
                "Architectural Coatings (Paint)", 
                "Glass",
                "Asphalt Roofing and Siding")

# Initialize an empty list to store the results
building_prices_list <- list()

# Pull the data via the API
payload <- list(
  'seriesid'  = building_series_ids,
  'startyear' = 2016,
  'endyear'   = 2024
)
response <- blsAPI(payload, 2)
json <- fromJSON(response)

# Loop through each series ID and corresponding item name
for (i in seq_along(building_series_ids)) {
  # Extract the data and convert it to a data frame
  building_prices_data <- as.data.frame(json[["Results"]][["series"]][["data"]][[i]])
  
  # Apply select() and mutate() to the data frame
  building_prices_data <- building_prices_data %>%
    select(-5) %>%
    mutate(item = building_item_names[i])
  
  # Store the modified data frame in the list
  building_prices_list <- append(building_prices_list, list(building_prices_data))
}

# Combine all data frames into one table
building_prices <- bind_rows(building_prices_list)
# Convert item column to upper case
building_prices$item <- toupper(building_prices$item)

# pivot the table with dates on the rows and items in the columns
# Set to always grab the latest month
building_prices_pivot <- building_prices %>% filter(period==building_prices$period[1]) %>% pivot_wider(names_from = item, values_from = value)
building_prices_pivot3 <- building_prices %>% pivot_wider(names_from = item, values_from = value)

# Create a check value for the period that is the latest month
#latest_month_building_price <- building_prices %>%
#  filter(latest == "true") %>%
#  select(period) %>%
#  group_by(period) %>%
#  slice(1) %>%
#  pull(period) %>%
# first()

# pivot the table items in rows the dates in columns
building_prices_pivot2 <- building_prices %>% filter(period==building_prices$period[1]) %>% select(-period,-periodName) %>% pivot_wider(names_from = year, values_from = value)
# Reorder the columns so that columns 2-6 are in proper order, sorted by number
building_prices_pivot2 <- building_prices_pivot2 %>% select(1,10,9,8,7,6,5,4,3,2)

# Change columns 2-7 to numeric
building_prices_pivot2[,2:10] <- sapply(building_prices_pivot2[,2:10], as.numeric)
# Round prices to 2 decimal places
building_prices_pivot2[,2:10] <- round(building_prices_pivot2[,2:10], 0)
# Add column for percentage increase between 2019 and 2024
building_prices_pivot2 <- building_prices_pivot2 %>% mutate(percent_increase_2016 = ((`2024` - `2016`)/`2016`)*100)
# Add column for percentage increase between 2019 and 2024
building_prices_pivot2 <- building_prices_pivot2 %>% mutate(percent_increase_2020 = ((`2024` - `2020`)/`2020`)*100)
# Add column for percentage increase between 2019 and 2024
building_prices_pivot2 <- building_prices_pivot2 %>% mutate(percent_increase_1yr = ((`2024` - `2023`)/`2023`)*100)
# Round percentage increase fields to 0 decimal places
building_prices_pivot2$percent_increase_2016 <- round(building_prices_pivot2$percent_increase_2016, 0)
building_prices_pivot2$percent_increase_2020 <- round(building_prices_pivot2$percent_increase_2020, 0)
building_prices_pivot2$percent_increase_1yr <- round(building_prices_pivot2$percent_increase_1yr, 0)
# Sort pivot2 alphabet
building_prices_pivot2 <- building_prices_pivot2 %>% arrange(item)

# export prices as csv
write_csv(building_prices, "data/building_prices.csv")
write_csv(building_prices_pivot, "data/building_prices_pivot.csv")
write_csv(building_prices_pivot2, "data/building_prices_pivot_table.csv")

# Convert the date to the desired format
ppi_date <- building_prices$periodName[1]

# Create the description string
description <- paste("Hover over charts to see the average nationwide price as of the end of ", ppi_date,".",sep="")

# Create a list to represent the JSON structure
json_data <- list(
  describe = list(
    intro = description
  )
)

# Convert the list to JSON format
json_string <- toJSON(json_data, pretty = TRUE)

# Write the JSON string to a file
write(json_string, file = "data/ppi_update.json")