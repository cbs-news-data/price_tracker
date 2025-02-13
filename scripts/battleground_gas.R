library(tidyverse)
library(blsAPI)
library(jsonlite)

# WORK TO BE DONE
# AUTOMATING USING THE MOST RECENT MONTHLY DATA AND THE SAME FOR PRIOR YEARS

# Load bls series data from data folder
series <- read_tsv("data/ap.series.txt") %>% filter(begin_year < 2019 & end_year > 2023)

# Access the API key from an environment variable
key <- Sys.getenv("BLS_API_KEY")

# Define series IDs and corresponding item names
series_ids <- c('APU000074714', 'APU012074714', 'APU023074714', 'APU048074714', 'APUS23B74714', 
                'APUS48A74714', 'APUS12B74714', 'APUS35C74714')
item_names <- c("United States", "North Carolina (Middle Atlantic)", "East North Central (Wisconsin)", "Mountain (Nevada)", "Detroit (Michigan)",
                "Phoenix (Arizona)", "Philadelphia (Pennsylvania)", "Atlanta (Georgia)")

# Pull the data via the API
payload <- list(
  'seriesid'  = series_ids,
  'startyear' = 2018,
  'endyear'   = 2024
)
response <- blsAPI(payload, 2)
json     <- fromJSON(response)

# Initialize an empty list to store data frames
gas_prices_list <- list()

# Loop through each series ID and corresponding item name
for (i in seq_along(series_ids)) {
  gas_prices_data <- json[["Results"]][["series"]][["data"]][[i]] %>% 
    select(-6) %>% 
    mutate(item = item_names[i])
  gas_prices_list[[i]] <- gas_prices_data
}

# Combine all data frames into one table
gas_prices <- bind_rows(gas_prices_list)
# Convert item column to upper case
# gas_prices$item <- toupper(gas_prices$item)

# pivot the table with dates on the rows and items in the columns
# Set to always grab the latest month
gas_prices_pivot <- gas_prices %>% filter(period==gas_prices$period[1]) %>% pivot_wider(names_from = item, values_from = value)

# Create a check value for the period that is the latest month
gas_latest_month_price <- gas_prices %>%
  filter(latest == "true") %>%
  select(period) %>%
  group_by(period) %>%
  slice(1) %>%
  pull(period) %>%
  first()

# pivot the table items in rows the dates in columns
gas_prices_pivot2 <- gas_prices %>% filter(period==gas_prices$period[1]) %>% select(-latest,-period,-periodName) %>% pivot_wider(names_from = year, values_from = value)
# Reorder the columns so that columns 2-6 are in proper order, sorted by number
gas_prices_pivot2 <- gas_prices_pivot2 %>% select(1,8,7,6,5,4,3,2)

# Change columns 2-7 to numeric
gas_prices_pivot2[,2:8] <- sapply(gas_prices_pivot2[,2:8], as.numeric)
# Add column for percentage increase between 2019 and 2024
gas_prices_pivot2 <- gas_prices_pivot2 %>% mutate(percent_increase_6y = ((`2024` - `2018`)/`2018`)*100)
# Round percentage increase to 0 decimal places
gas_prices_pivot2$percent_increase_6y <- round(gas_prices_pivot2$percent_increase_6y, 0)
# Add column for percentage increase between 2019 and 2024
gas_prices_pivot2 <- gas_prices_pivot2 %>% mutate(percent_increase_2y = ((`2024` - `2022`)/`2022`)*100)
# Round percentage increase to 0 decimal places
gas_prices_pivot2$percent_increase_2y <- round(gas_prices_pivot2$percent_increase_2y, 0)
# Add column for percentage increase between 2019 and 2024
gas_prices_pivot2 <- gas_prices_pivot2 %>% mutate(percent_increase_1y = ((`2024` - `2023`)/`2023`)*100)
# Round percentage increase to 0 decimal places
gas_prices_pivot2$percent_increase_1y <- round(gas_prices_pivot2$percent_increase_1y, 0)

# add a column called state and populate in this order United States, North Carolina, Wisconsin, Nevada, Michigan, Arizona, Pennsylvania, Georgia
gas_prices_pivot2$state <- c("United States", "North Carolina", "Wisconsin", "Nevada", "Michigan", "Arizona", "Pennsylvania", "Georgia")
# Name columns starting at 6 in gas_prices_pivot as United States, North Carolina, Wisconsin, Nevada, Michigan, Arizona, Pennsylvania, Georgia
colnames(gas_prices_pivot)[5:12] <- c("United States", "North Carolina", "Wisconsin", "Nevada", "Michigan", "Arizona", "Pennsylvania", "Georgia")



# export prices as csv
write_csv(gas_prices, "data/battleground_gas_prices.csv")
write_csv(gas_prices_pivot, "data/battleground_gas_prices_pivot.csv")
write_csv(gas_prices_pivot2, "data/battleground_gas_prices_pivot_table.csv")

# Convert the date to the desired format
gas_cpi_date <- gas_prices$periodName[1]

# Create the description string
#description <- paste("Hover over charts to see the average nationwide price as of the end of ", cpi_date,".",sep="")

# Create a list to represent the JSON structure
#json_data <- list(
#  describe = list(
#    intro = description
#  )
#)

# Convert the list to JSON format
#json_string <- toJSON(json_data, pretty = TRUE)

# Write the JSON string to a file
#write(json_string, file = "data/cpi_update.json")

