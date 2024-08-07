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
gas_prices <- gas_prices %>% pivot_longer(cols = -1, names_to = "date", values_to = "price")
colnames(gas_prices) <- c("date","region","price")
# Eliminate rows where the price is NA
gas_prices <- gas_prices %>% filter(!is.na(price))
# Edit region to include only the string before " Regular"
gas_prices$region <- str_remove(gas_prices$region, " Regular.*")
# Remove weekly from the region
gas_prices$region <- str_remove(gas_prices$region, "Weekly ")
# Retain only the string prior to " (" in the region
gas_prices$region <- str_remove(gas_prices$region, " \\(.*")
# Round the price to two decimals
gas_prices$price <- round(gas_prices$price, 2)
# convert the date to a date
gas_prices$date <- as.Date(gas_prices$date, format = "%Y-%m-%d")
# include the prices since the beginning of 2019
gas_prices <- gas_prices %>% filter(date >= "2014-01-01")
# Sort by region then date
gas_prices <- gas_prices %>% arrange(region, date)


# export the oil prices
write_csv(gas_prices, "data/gas_prices.csv")








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


