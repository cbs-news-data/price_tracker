library(tidyverse)
library(dplyr)

# Load bls series data from data folder
building_series <- read_tsv("data/wp.series") %>% filter(begin_year < 2019 & end_year > 2023)

# Load bls average price data file, downloaded manually
building_prices <- read_tsv("data/wp.data.0.Current.txt")

# Define series IDs and corresponding item names
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
                         "Drywall", 
                         "Steel Mill Products", 
                         "Concrete Block and Brick",
                         "Ready-Mix Concrete", 
                         "Aluminum", 
                         "Paint", 
                         "Glass",
                         "Asphalt Roofing & Siding")

# Create a table from the above two series called prices_items
building_prices_items <- tibble(series_id = building_series_ids, item = building_item_names)
# Set the second column item names to upper case
building_prices_items$item <- toupper(building_prices_items$item)

# Merge the prices_items table with the prices table, keeping records from prices only if they match the series_id column
building_prices <- building_prices %>% inner_join(building_prices_items, by = "series_id") %>% select(-5)
# Filter for the latest month
building_prices <- building_prices %>% filter(period == prices$period[1])
# Filter for only the years 2019 through 2025
building_prices <- building_prices %>% filter(year >= 2019 & year <= 2025)
# Add a column using case when where if period == M01, then the new column is January and so on
building_prices <- building_prices %>% mutate(periodName = case_when(
  period == "M01" ~ "January",
  period == "M02" ~ "February",
  period == "M03" ~ "March",
  period == "M04" ~ "April",
  period == "M05" ~ "May",
  period == "M06" ~ "June",
  period == "M07" ~ "July",
  period == "M08" ~ "August",
  period == "M09" ~ "September",
  period == "M10" ~ "October",
  period == "M11" ~ "November",
  period == "M12" ~ "December"
))


# pivot the table with dates on the rows and items in the columns
# Set to always grab the latest month
building_prices_pivot <- building_prices %>% filter(period==building_prices$period[1]) %>% select(-series_id) %>% pivot_wider(names_from = item, values_from = value)

# pivot the table items in rows the dates in columns
building_prices_pivot2 <- building_prices %>% filter(period==building_prices$period[1]) %>% select(-period,-periodName,-series_id) %>% pivot_wider(names_from = year, values_from = value)
# Add column for percentage increase between 2019 and 2025
building_prices_pivot2 <- building_prices_pivot2 %>% mutate(percent_increase = ((`2025` - `2019`)/`2019`)*100)
# Round percentage increase to 0 decimal places
building_prices_pivot2$percent_increase <- round(building_prices_pivot2$percent_increase, 0)
# Sort pivot2 alphabet
building_prices_pivot2 <- building_prices_pivot2 %>% arrange(item)

# export prices as csv
write_csv(building_prices, "data/building_prices.csv")
write_csv(building_prices_pivot, "data/building_prices_pivot.csv")
write_csv(building_prices_pivot2, "data/building_prices_pivot_table.csv")

# Convert the date to the desired format
cpi_date <- building_prices$periodName[1]

# Create the description string
building_description <- paste("Hover over charts to see the average nationwide price as of the end of ", cpi_date,".",sep="")

# Create a list to represent the JSON structure
json_data <- list(
  describe = list(
    intro = building_description
  )
)

# Convert the list to JSON format
json_string <- toJSON(json_data, pretty = TRUE)

# Write the JSON string to a file
write(json_string, file = "data/building_ppi_update.json")


