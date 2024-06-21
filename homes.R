library(tidyverse)
library(tidycensus)

# Import zillow rent data
homes_metro <- read_csv("https://files.zillowstatic.com/research/public_csvs/median_sale_price/Metro_median_sale_price_uc_sfrcondo_sm_month.csv?t=1718976907")
# Repeat with counties
#homes_county <- read_csv("https://files.zillowstatic.com/research/public_csvs/zori/County_zori_uc_sfrcondomfr_sm_month.csv")
# Repeat with zip codes
#homes_zip <- read_csv("https://files.zillowstatic.com/research/public_csvs/zori/Zip_zori_uc_sfrcondomfr_sm_month.csv")
# Repeat with cities
#homes_city <- read_csv("https://files.zillowstatic.com/research/public_csvs/zori/City_zori_uc_sfrcondomfr_sm_month.csv")

# Filter the most recent month and the same month from the five previous years
homes_metro_5yrs <- homes_metro %>%
  select(3,5,138,150,162,174,186,198)
#homes_county_5yrs <- homes_county %>%
#  select(3,5,61,73,85,97,109,121)
#homes_zip_5yrs <- homes_zip %>%
#  select(3,5,61,73,85,97,109,121)
#homes_city_5yrs <- homes_city %>%
#  select(3,5,60,72,84,96,108,120)

# Format the five year tables with rounded figures in the last five columns with no demical places
homes_metro_5yrs <- homes_metro_5yrs %>%
  mutate(across(4:8, round, digits = 0))
#homes_county_5yrs <- homes_county_5yrs %>%
#  mutate(across(4:9, round, digits = 0))
#homes_zip_5yrs <- homes_zip_5yrs %>%
#  mutate(across(4:9, round, digits = 0))
#homes_city_5yrs <- homes_city_5yrs %>%
#  mutate(across(4:9, round, digits = 0))

# Rename the last five columns to the first four characters of the current column name
homes_metro_5yrs <- homes_metro_5yrs %>%
  rename_with(~substr(., 1, 4), 3:8)
#homes_county_5yrs <- homes_county_5yrs %>%
#  rename_with(~substr(., 1, 4), 4:9)
#homes_zip_5yrs <- homes_zip_5yrs %>%
#  rename_with(~substr(., 1, 4), 4:9)
#homes_city_5yrs <- homes_city_5yrs %>%
#  rename_with(~substr(., 1, 4), 4:9)

# In metro data, add a percentage change from column named 2023 to 2024
homes_metro_5yrs <- homes_metro_5yrs %>%
  mutate(change1yr = ((`2024` - `2023`) / `2023`) * 100)
homes_metro_5yrs$change1yr <- round(homes_metro_5yrs$change1yr,0)
# Repeat for change from 2020 to 2024
homes_metro_5yrs <- homes_metro_5yrs %>%
  mutate(change5yr = ((`2024` - `2019`) / `2019`) * 100)
homes_metro_5yrs$change5yr <- round(homes_metro_5yrs$change5yr,0)

# Repeat for other geographies county, city and zip
#homes_county_5yrs <- homes_county_5yrs %>%
#  mutate(change1yr = ((`2024` - `2023`) / `2023`) * 100)
#homes_county_5yrs$change1yr <- round(homes_county_5yrs$change1yr,0)
#homes_county_5yrs <- homes_county_5yrs %>%
#  mutate(change5yr = ((`2024` - `2019`) / `2019`) * 100)
#homes_county_5yrs$change5yr <- round(homes_county_5yrs$change5yr,0)

#homes_zip_5yrs <- homes_zip_5yrs %>%
#  mutate(change1yr = ((`2024` - `2023`) / `2023`) * 100)
#homes_zip_5yrs$change1yr <- round(homes_zip_5yrs$change1yr,0)
#homes_zip_5yrs <- homes_zip_5yrs %>%
#  mutate(change5yr = ((`2024` - `2019`) / `2019`) * 100)
#homes_zip_5yrs$change5yr <- round(homes_zip_5yrs$change5yr,0)

#homes_city_5yrs <- homes_city_5yrs %>%
#  mutate(change1yr = ((`2024` - `2023`) / `2023`) * 100)
#homes_city_5yrs$change1yr <- round(homes_city_5yrs$change1yr,0)
#homes_city_5yrs <- homes_city_5yrs %>%
#  mutate(change5yr = ((`2024` - `2019`) / `2019`) * 100)
#homes_city_5yrs$change5yr <- round(homes_city_5yrs$change5yr,0)


# create a table with the first column being values from state.name and the second being the state.abbr
states <- data.frame(state.name, state.abb)
# add a third row called swing and assign as true if the state is PA, WI, MI, OH, FL, NC, AZ, GA, NV or TX
states$swing <- states$state.abb %in% c("PA", "WI", "MI", "NC", "AZ", "GA", "NV")




# Remove from homes_metro_5yrs all rows where the any of columns 3:8 are NA
#homes_metro_5yrs <- homes_metro_5yrs %>%
#  filter(!is.na(`2024`), !is.na(`2023`), !is.na(`2019`))
# Remove if region name is United States
homes_metro_5yrs <- homes_metro_5yrs %>%
  filter(RegionName != "United States")
# Append swing state column to homes_metro_5yrs by joining with states on state abbreviation
homes_metro_5yrs <- homes_metro_5yrs %>%
  left_join(states %>% select(2:3), by = c("StateName" = "state.abb"))

# Remove from homes_city_5yrs all rows where the any of columns 3:8 are NA
#homes_city_5yrs <- homes_city_5yrs %>%
#  filter(!is.na(`2024`), !is.na(`2023`), !is.na(`2019`))
# Remove if region name is United States
#homes_city_5yrs <- homes_city_5yrs %>%
#  filter(RegionName != "United States")
# Append swing state column to homes_city_5yrs by joining with states on state abbreviation
#homes_city_5yrs <- homes_city_5yrs %>%
#  left_join(states %>% select(2:3), by = c("StateName" = "state.abb"))

# Reduce to the Top 200 metros using head
homes_metro_5yrs <- homes_metro_5yrs %>% select(1,3,8,10) %>% filter(!is.na(`2019`)) %>% head(200)
# Repeat for Top 200 cities using head
#homes_city_5yrs <- homes_city_5yrs %>%
#  head(200)

# output CSV of rent for tables
write_csv(homes_metro_5yrs, "data/homes_metro_5yrs.csv")
#write_csv(homes_county_5yrs, "data/homes_county_5yrs.csv")
#write_csv(homes_zip_5yrs, "data/homes_zip_5yrs.csv")
#write_csv(homes_city_5yrs, "data/homes_city_5yrs.csv")





