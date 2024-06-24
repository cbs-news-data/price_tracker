library(tidyverse)
library(blsAPI)
library(jsonlite)

# Load bls series data from data folder
series <- read_tsv("data/ap.series.txt") %>% filter(begin_year < 2019 & end_year > 2023) %>% filter(area_code == "0000" )

# Access the API key from an environment variable
key <- Sys.getenv("BLS_API_KEY")

# Define series IDs and corresponding item names
series_ids <- c('APU0000709112', 'APU0000704111', 'APU0000711211', 'APU0000703112', 'APU0000FF1101', 
                'APU0000702111', 'APU0000708111', 'APU0000702421', 'APU000074714', 'APU0000712112', 
                'APU0000717311', 'APU000072620', 'APU0000FN1101', 'APU0000710212', 'APU000072610', 
                'APU0000713111', 'APU0000FJ4101', 'APU0000712311','APU0000720311','APU0000711415',
                'APU0000701312')
item_names <- c("Milk (half gallon)", "Bacon (pound)", "Bananas (pound)", "Ground Beef (pound)", 
                "Chicken Breast (pound)", "Loaf of Bread", "Dozen eggs", "Cookies", "Regular Unleaded Gasoline (gallon)", 
                "Potatoes (pound)", "Coffee (pound)", "Utility gas", "2-liter soft drink", 
                "Cheddar Cheese (pound)", "Electricity (kilowatt hour)", "Frozen orange juice", 
                "Yogurt (8 ounces)", "Tomatoes (pound)", "Table Wine", "Strawberries (pint)", "Rice (pound)")

# Pull the data via the API
payload <- list(
  'seriesid'  = series_ids,
  'startyear' = 2019,
  'endyear'   = 2024
)
response <- blsAPI(payload, 2)
json     <- fromJSON(response)

# Initialize an empty list to store data frames
prices_list <- list()

# Loop through each series ID and corresponding item name
for (i in seq_along(series_ids)) {
  prices_data <- json[["Results"]][["series"]][["data"]][[i]] %>% 
    select(-6) %>% 
    mutate(item = item_names[i])
  prices_list[[i]] <- prices_data
}

# Combine all data frames into one table
prices <- bind_rows(prices_list)

# pivot the table with dates on the rows and items in the columns
prices_pivot <- prices %>% filter(period==prices$period[1]) %>% pivot_wider(names_from = item, values_from = value)

# pivot the table items in rows the dates in columns
prices_pivot2 <- prices %>% filter(period==prices$period[1]) %>% select(-latest,-period,-periodName) %>% pivot_wider(names_from = year, values_from = value)
# Reorder the columns so that columns 2-6 are in proper order, sorted by number
prices_pivot2 <- prices_pivot2 %>% select(1,7,6,5,4,3,2)
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




