library(tidyverse)
library(blsR)
library(jsonlite)
library(dplyr)
library(devtools)
library(DatawRappr)

# Load bls series data from data folder
series <- read_tsv("data/ap.series.txt") %>% filter(begin_year < 2019 & end_year > 2023) %>% filter(area_code == "0000" )

# Access the API key from an environment variable
bls_key <- Sys.getenv("BLS_API_KEY")

# Define series IDs and corresponding item names
series_ids <- c('APU0000709112', 'APU0000704111', 'APU0000711211', 'APU0000703112', 'APU0000FF1101', 
                'APU0000702111', 'APU0000708111', 'APU0000702421', 'APU0000703511', 'APU0000712112', 
                'APU0000717311', 'APU000072620', 'APU0000FN1101', 'APU0000710212', 'APU000072610', 
                'APU0000713111', 'APU0000FJ4101', 'APU0000712311','APU0000720311','APU0000711415',
                'APU0000701312', 'APU0000710411', 'APU0000FS1101', 'APU0000701322',
                'APU0000715211', 'APU0000704312', 'APU0000714221', 'APU0000FD3101',
                'APU0000711412', 'APU0000701111', 'APU0000711411', 'APU0000712211',
                'APU0000711311', 'APU0000714233', 'APU0000720111', 'APU0000710211',
                'APU0000703432', 'APU0000703311', 'APU0000702212', 'APU0000FN1102', 'APU000072610','APU000072620')
item_names <- c("Milk (half gallon)", "Bacon", "Bananas", "Ground Beef", 
                "Chicken Breast", "White Bread", "Dozen eggs", "Cookies", "Steak", 
                "Potatoes", "Coffee", "Utility gas", "Soft Drinks (2 liter)",
                "Cheddar Cheese", "Electricity (kw hour)", "Frozen orange juice", 
                "Yogurt (8 ounces)", "Tomatoes", "Table Wine", "Strawberries (pint)", "Rice",
                "Ice Cream (half gallon)","Butter","Spaghetti and Macaroni",
                "Sugar","Boneless Ham","Canned corn","Pork Chops",
                "Lemons","Flour","Grapefruit","Lettuce",
                "Oranges","Dried Beans","Malt beverages","Processed American cheese",
                "Beef for Stew","Round roast","Wheat bread","Soft Drinks (12 pack)", "Electricity per KWH","Utility (piped) gas per therm")

ids_names <- data.frame(series_id = series_ids, item_name = item_names, stringsAsFactors = FALSE)

monthly_prices <- data.frame()

for (i in 1:nrow(ids_names)) {
  series_id <- ids_names$series_id[i]
  item_name <- ids_names$item_name[i]
  
  print(series_id)
  print(item_name)
  
  current_item <- get_series_table(series_id, start_year = 2024, end_year = 2025, registrationKey = bls_key) %>% 
    mutate(month = str_sub(period, start=-2)) %>% 
    mutate(year = as.character(year)) %>% 
    mutate(month = as.character(month)) %>% 
    mutate(date = paste0(year, "-", month, "-01")) %>% 
    mutate(date = as.Date(date)) %>% 
    arrange(date) %>%
    select(date, value) %>% 
    rename(price = value) %>% 
    mutate(item = item_name) %>% 
    select(item, date, price)
  
  monthly_prices <- bind_rows(monthly_prices, current_item)
}


monthly_prices_select_groceries <- monthly_prices %>% 
  pivot_wider(names_from = item, values_from = price) %>% 
  select(date, `Milk (half gallon)`, `Dozen eggs`, `Cheddar Cheese`, `Butter`, `Coffee`, `Ground Beef`, `Chicken Breast`, `Steak`, `White Bread`, `Flour`)

write.csv(monthly_prices_select_groceries, "data/monthly_prices_select_groceries.csv", row.names = FALSE)

# Push to Datawrapper
dw_api_key <- Sys.getenv("DW_KEY")

max_date <- max(monthly_prices_select_groceries$date)
max_date_pretty <- format(max_date, "%B %Y")

datawrapper_auth(api_key = dw_api_key)

dw_data_to_chart(monthly_prices_select_groceries, "av0vf", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "av0vf",
  api_key = dw_api_key,
  annotate = paste("Note: Data through", max_date_pretty)
)

dw_publish_chart(chart_id = "av0vf")

monthly_prices_utilities <- monthly_prices %>% 
  pivot_wider(names_from = item, values_from = price) %>% 
  select(date, `Electricity per KWH`, `Utility (piped) gas per therm`)

write.csv(monthly_prices_utilities, "data/monthly_prices_utilities.csv", row.names = FALSE)

# Push to Datawrapper
max_date <- max(monthly_prices_utilities$date)
max_date_pretty <- format(max_date, "%B %Y")

datawrapper_auth(api_key = dw_api_key)

dw_data_to_chart(monthly_prices_utilities, "TN9fZ", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "TN9fZ",
  api_key = dw_api_key,
  annotate = paste("Note: Data through", max_date_pretty)
)

dw_publish_chart(chart_id = "TN9fZ")


#CPI for other goods and services

series_ids <- c('CUUR0000SAA', 'CUUR0000SEHK', 'CUUR0000SEEE02', 'CUUR0000SEHJ', 'CUUR0000SEAG02', 'CUUR0000SETA01', 'CUUR0000SEGB', 'CUUR0000SS61031', 'CUUR0000SEHM', 'CUUR0000SETA02')
item_names <- c('Apparel', 'Appliances', 'Computer software and accessories', 'Furniture and bedding', 'Jewelry', 'New vehicles', 'Personal care products', 'Pet food and treats', 'Tools, hardware, outdoor equipment and supplies', 'Used cars and trucks')

ids_names <- data.frame(series_id = series_ids, item_name = item_names, stringsAsFactors = FALSE)

monthly_CPI_changes <- data.frame()

for (i in 1:nrow(ids_names)) {
  series_id <- ids_names$series_id[i]
  item_name <- ids_names$item_name[i]
  
  print(series_id)
  print(item_name)
  
  
  CPI_current_item <- get_series_table(series_id, start_year = 2023, end_year = 2025, registrationKey = bls_key) %>% 
    mutate(month = str_sub(period, start=-2)) %>% 
    mutate(year = as.character(year)) %>% 
    mutate(month = as.character(month)) %>% 
    mutate(date = paste0(year, "-", month, "-01")) %>% 
    mutate(date = as.Date(date)) %>% 
    arrange(date) %>%
    mutate(price_change_yoy = (value / lag(value, 12) - 1) * 100) %>% 
    mutate(price_change_yoy = round(price_change_yoy, digits = 1)) %>% 
    filter(!is.na(price_change_yoy)) %>% 
    mutate(item = item_name) %>% 
    select(item, date, price_change_yoy)
  
  
  monthly_CPI_changes <- bind_rows(monthly_CPI_changes, CPI_current_item)
}

monthly_CPI_changes_select_goods_services <- monthly_CPI_changes %>% 
  pivot_wider(names_from = item, values_from = price_change_yoy)

write.csv(monthly_CPI_changes_select_goods_services, "data/monthly_CPI_changes_select_goods_services.csv", row.names = FALSE)

# Push to Datawrapper
max_date <- max(monthly_CPI_changes_select_goods_services$date)
max_date_pretty <- format(max_date, "%B %Y")

datawrapper_auth(api_key = dw_api_key)

dw_data_to_chart(monthly_CPI_changes_select_goods_services, "bawsu", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "bawsu",
  api_key = dw_api_key,
  annotate = paste("Note: Data through", max_date_pretty)
)

dw_publish_chart(chart_id = "bawsu")

