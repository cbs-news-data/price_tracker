library(tidyverse)
library(readxl)


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
oil_prices <- oil_prices %>% filter(date >= "2019-01-01")
# export the oil prices
write_csv(oil_prices, "data/oil_prices.csv")

