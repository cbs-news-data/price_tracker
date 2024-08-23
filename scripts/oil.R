library(tidyverse)
library(readxl)
library(jsonlite)


# Read this excel file for oil prices
url_oil <- "https://www.eia.gov/dnav/pet/xls/PET_PRI_SPT_S1_W.xls"
download.file(url_oil, destfile = "data/oil_prices.xls", mode = "wb")
oil_prices <- read_excel("data/oil_prices.xls", sheet = 2, skip = 2)
# drop column three
oil_prices <- oil_prices %>% select(-3)
colnames(oil_prices) <- c("date","price")
# convert the date to a date
oil_prices$date <- as.Date(oil_prices$date, format = "%Y-%m-%d")
# include the prices since the beginning of 2019
oil_prices <- oil_prices %>% filter(date >= "2014-01-01")
# export the oil prices
write_csv(oil_prices, "data/oil_prices.csv")


# Assuming 'oil_prices' is a data frame with a 'date' column
# Convert the date to the desired format
oil_date <- format(max(oil_prices$date), "%B %d")

# Create the description string
description <- paste("The price per barrel reported by the Energy Information Administration as of ", oil_date, ". Hover anywhere on the chart to see the price for any specific week.",sep="")

# Create a list to represent the JSON structure
json_data <- list(
  describe = list(
    intro = description
  )
)

# Convert the list to JSON format
json_string <- toJSON(json_data, pretty = TRUE)

# Write the JSON string to a file
write(json_string, file = "data/oil_update.json")
