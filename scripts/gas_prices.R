# Load necessary libraries
library(rvest)
library(dplyr)
library(tidyverse)
library(readxl)
library(jsonlite)


# Read this excel file for oil prices
url_gas <- "https://www.eia.gov/petroleum/gasdiesel/xls/pswrgvwall.xls"
download.file(url_gas, destfile = "data/gas_prices.xls", mode = "wb")
gas_prices <- read_excel("data/gas_prices.xls", sheet = 4, skip = 2)

# Transform to long data
# as_prices <- gas_prices %>% pivot_longer(cols = -1, names_to = "date", values_to = "price")
# colnames(gas_prices) <- c("date","region","price")
# Eliminate rows where the price is NA
# gas_prices <- gas_prices %>% filter(!is.na(price))

# Edit column names to keep only the string before " Regular"
colnames(gas_prices) <- sub(" Regular.*", "", colnames(gas_prices))
# Repeat to remove "Weekly " from the column names
colnames(gas_prices) <- sub("Weekly ", "", colnames(gas_prices))
# Repeat to keep only the string before " ("
colnames(gas_prices) <- sub(" \\(.*", "", colnames(gas_prices))
# Repeat to keep only the remaining string before ", "
colnames(gas_prices) <- sub(", .*", "", colnames(gas_prices))
# Rename the first column to "date"
colnames(gas_prices)[1] <- "date"
# convert the date to a date
gas_prices$date <- as.Date(gas_prices$date, format = "%Y-%m-%d")
# include the prices since the beginning of 2019
gas_prices <- gas_prices %>% filter(date >= "2014-01-01")
# Round any number in any column to two decimals
gas_prices <- gas_prices %>% mutate(across(where(is.numeric), ~ round(., 2)))
# Sort by date
gas_prices <- gas_prices %>% arrange(date)

# export the oil prices
write_csv(gas_prices, "data/gas_prices.csv")
# Create US and regions
gas_prices %>% select(1:10) %>% write_csv("data/gas_prices_us.csv")
# Create US and states
gas_prices %>% select(1:2,11:19) %>% write_csv("data/gas_prices_states.csv")
# Create US and cities
gas_prices %>% select(1:2,20:29) %>% write_csv("data/gas_prices_cities.csv")





# Assuming 'oil_prices' is a data frame with a 'date' column
# Convert the date to the desired format
gas_date <- format(max(gas_prices$date), "%B %-d")

# Create the description string
gas_description <- paste("The price per gallon reported by the Energy Information Administration as of ", gas_date, ". Hover anywhere on the chart to see the price for any specific week. The red line shows the U.S. average by comparison.",sep="")

# Create a list to represent the JSON structure
json_data <- list(
  describe = list(
    intro = gas_description
  )
)

# Convert the list to JSON format
json_string <- toJSON(json_data, pretty = TRUE)

# Write the JSON string to a file
write(json_string, file = "data/gas_update.json")








# URL of the website containing gas price averages by state from AAA
# url <- 'https://gasprices.aaa.com/state-gas-price-averages/'

# Read the HTML
# webpage <- read_html(url)

# Extract the table containing the gas price averages
# gas_price_state <- webpage %>%
#  html_node('table') %>%
#  html_table()

# Clean and format the table as needed
#gas_price_state <- gas_price_state %>%
#  rename(State = 1, Regular = 2, MidGrade = 3, Premium = 4, Diesel = 5) %>%
#  mutate(across(Regular:Diesel, ~ as.numeric(gsub("[$,]", "", .))))


