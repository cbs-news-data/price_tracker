library(tidyverse)
library(dplyr)
library(jsonlite)


# Load bls series data from data folder
series <- read_tsv("data/ap.series.txt") %>% filter(begin_year < 2020 & end_year > 2023) %>% filter(area_code == "0000" )

# Load bls average price data file, downloaded manually
prices <- read_tsv("data/ap.data.0.Current.txt")

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

# Create a table from the above two series called prices_items
prices_items <- tibble(series_id = series_ids, item = item_names)
# Set the second column item names to upper case
prices_items$item <- toupper(prices_items$item)

# temp egg prices in northeast
#eggprices <- prices %>% filter(series_id == "APU0100708111")

# Merge the prices_items table with the prices table, keeping records from prices only if they match the series_id column
prices <- prices %>% inner_join(prices_items, by = "series_id") %>% select(-5)
# Filter for the latest month # manual change # need to develop way to automate
prices <- prices %>% filter(period == "M03")
# Filter for only the years 2019 through 2025
prices <- prices %>% filter(year >= 2019 & year <= 2025)
# Add a column using case when where if period == M01, then the new column is January and so on
prices <- prices %>% mutate(periodName = case_when(
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
prices_pivot <- prices %>% filter(period==prices$period[1]) %>% select(-series_id) %>% pivot_wider(names_from = item, values_from = value)

# pivot the table items in rows the dates in columns
prices_pivot2 <- prices %>% filter(period==prices$period[1]) %>% select(-period,-periodName,-series_id) %>% pivot_wider(names_from = year, values_from = value)
# Add column for percentage increase between 2019 and 2025
prices_pivot2 <- prices_pivot2 %>% mutate(percent_increase = ((`2025` - `2019`)/`2019`)*100)
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


