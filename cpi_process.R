library(tidyverse)
library(lubridate)
library(httr)
#library(devtools)
#install_github("mikeasilva/blsAPI")
library(blsAPI)
library(jsonlite)


# Load bls series data from data folder
series <- read_tsv("data/ap.series.txt")

# Your API key is: 3309567024fa47bca0255b8a42a122f7.

key <- "3309567024fa47bca0255b8a42a122f7"

## Pull the data via the API
payload <- list(
  'seriesid'  = c('APU0000709112', # milk
                  'APU0000704111', # bacon
                  'APU0000711211', # bananas
                  'APU0000703112', #beef
                  'APU0000FF1101', # chicken breast
                  'APU0000702111', # bread
                  'APU0000708111', # eggs
                  'APU0000702421', # cookies
                  'APU000074714', # gasoline reg unleaded
                  'APU0000712112', # potatoes
                  'APU0000717311', # coffee
                  'APU000072620', # gas utility
                  'APU0000FN1101', # 2 liter soft drink
                  'APU0000710212', # pound cheddar cheese
                  'APU000072610'), # electricity
  'startyear' = 2019,
  'endyear'   = 2024)
response <- blsAPI(payload, 2)
json     <- fromJSON(response)

prices_1 <- json[["Results"]][["series"]][["data"]][[1]] %>% select(-6) %>% mutate(item = "Milk (half gallon)")
prices_2 <- json[["Results"]][["series"]][["data"]][[2]] %>% select(-6) %>% mutate(item = "Bacon (pound)")
prices_3 <- json[["Results"]][["series"]][["data"]][[3]] %>% select(-6) %>% mutate(item = "Bananas (pound)")
prices_4 <- json[["Results"]][["series"]][["data"]][[4]] %>% select(-6) %>% mutate(item = "Ground Beef (pound)")
prices_5 <- json[["Results"]][["series"]][["data"]][[5]] %>% select(-6) %>% mutate(item = "Chicken Breast (pound)")
prices_6 <- json[["Results"]][["series"]][["data"]][[6]] %>% select(-6) %>% mutate(item = "Loaf of Bread")
prices_7 <- json[["Results"]][["series"]][["data"]][[7]] %>% select(-6) %>% mutate(item = "Dozen eggs")
prices_8 <- json[["Results"]][["series"]][["data"]][[8]] %>% select(-6) %>% mutate(item = "Cookies")
prices_9 <- json[["Results"]][["series"]][["data"]][[9]] %>% select(-6) %>% mutate(item = "Gasoline (gallon)")
prices_10 <- json[["Results"]][["series"]][["data"]][[10]] %>% select(-6) %>% mutate(item = "Potatoes (pound)")
prices_11 <- json[["Results"]][["series"]][["data"]][[11]] %>% select(-6) %>% mutate(item = "Coffee (pound)")
prices_12 <- json[["Results"]][["series"]][["data"]][[12]] %>% select(-6) %>% mutate(item = "Utility gas")
prices_13 <- json[["Results"]][["series"]][["data"]][[13]] %>% select(-6) %>% mutate(item = "2-liter soft drink")
prices_14 <- json[["Results"]][["series"]][["data"]][[14]] %>% select(-6) %>% mutate(item = "Cheddar Cheese (pound)")
prices_15 <- json[["Results"]][["series"]][["data"]][[15]] %>% select(-6) %>% mutate(item = "Electricity (kilowatt hour)")

# combine this series of prices into one table
prices <- rbind(prices_1, prices_2, prices_3, prices_4, prices_5, prices_6, prices_7, prices_8, prices_9, prices_10, prices_11, prices_12, prices_13, prices_14, prices_15)

# pivot the table with dates on the rows and items in the columns
prices_pivot <- prices %>% filter(period=="M05") %>% pivot_wider(names_from = item, values_from = value)

# pivot the table items in rows the dates in columns
prices_pivot2 <- prices %>% filter(period=="M05") %>% select(-latest,-period,-periodName) %>% pivot_wider(names_from = year, values_from = value)
# Reorder the columns so that columns 2-6 are in proper order, sorted by number
prices_pivot2 <- prices_pivot2 %>% select(1,7,6,5,4,3,2)


# export prices as csv
write_csv(prices, "data/prices.csv")
write_csv(prices_pivot, "data/prices_pivot.csv")
write_csv(prices_pivot2, "data/prices_pivot_table.csv")



# Note 
# 0000	is U.S. city average
# S	Seasonally Adjusted
# U	Not Seasonally Adjusted

# URLS for the latest data
#url_food <- "https://download.bls.gov/pub/time.series/ap/ap.data.3.Food"
#url_gas <- "https://download.bls.gov/pub/time.series/ap/ap.data.2.Gasoline"
#url_current <- "https://download.bls.gov/pub/time.series/ap/ap.data.0.Current"
#url_item <- "https://download.bls.gov/pub/time.series/ap/ap.item"
#url_period <- "https://download.bls.gov/pub/time.series/ap/ap.period"


