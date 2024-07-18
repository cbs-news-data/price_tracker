library(tidyverse)
library(tidycensus)
library(readxl)

ppsf_county <- read_excel("data/ppsf_county_redfin.xlsx", 
                          skip = 1) %>% janitor::clean_names()
ppsf_metro <- read_excel("data/ppsf_metro_redfin.xlsx", 
                         skip = 1) %>% janitor::clean_names()

rfmedian_metro <- read_excel("data/med_sale_price_county_redfin.xlsx", 
                          skip = 1) %>% janitor::clean_names() %>%
  select(1, ncol(.) - (10:0)*12) %>% select(1,7,12)
# rename the last two columns 2019_median and 2024_median
rfmedian_metro <- rfmedian_metro %>%
  rename(`median_home_2019` = `may_2019`,
         `median_home_2024` = `may_2024`)

rfmedian_county <- read_excel("data/med_sale_price_metro_redfin.xlsx", 
                         skip = 1) %>% janitor::clean_names() %>%
  select(1, ncol(.) - (10:0)*12) %>% select(1,7,12)
# Rename the last two columns 2019_median and 2024_median
rfmedian_county <- rfmedian_county %>%
  rename(`median_home_2019` = `may_2019`,
         `median_home_2024` = `may_2024`)

# remove all but May 2024, May 2019, May 2014
ppsf_county <- ppsf_county %>%
  select(1, ncol(.) - (10:0)*12) %>% select(1,7,12)
ppsf_metro <- ppsf_metro %>%
  select(1, ncol(.) - (10:0)*12) %>% select(1,7,12)
# Round the last 3 cols to 0 decimal places
ppsf_county <- ppsf_county %>%
  mutate(across(2:3, round, digits = 1))
ppsf_metro <- ppsf_metro %>%
  mutate(across(2:3, round, digits = 1))
# calculate a field for starter house based on 1500 sq ft for both the 2019 and 2024 ppsf price
ppsf_county <- ppsf_county %>%
  mutate(starter_home_2019 = 1500 * `may_2019`,
         starter_home_2024 = 1500 * `may_2024`)
ppsf_metro <- ppsf_metro %>%
  mutate(starter_home_2019 = 1500 * `may_2019`,
         starter_home_2024 = 1500 * `may_2024`)
# calculate the difference between the 2019 and 2024 starter home price
ppsf_county <- ppsf_county %>%
  mutate(diff_starter_home = starter_home_2024 - starter_home_2019)
ppsf_metro <- ppsf_metro %>%
  mutate(diff_starter_home = starter_home_2024 - starter_home_2019)
# calculate the percentage increase between 2019 and 2024
ppsf_county <- ppsf_county %>%
  mutate(percent_increase_starter = ((starter_home_2024 - starter_home_2019) / starter_home_2019) * 100)
ppsf_metro <- ppsf_metro %>%
  mutate(percent_increase_starter = ((starter_home_2024 - starter_home_2019) / starter_home_2019) * 100)

# split the region in metro by dropping the phrase metro area and separating city and state
ppsf_metro <- ppsf_metro %>%
  mutate(region = str_remove(region, " metro area")) %>%
  separate(region, c("city", "state"), sep = ", ", remove = FALSE)
# Separate county and state
ppsf_county <- ppsf_county %>%
  separate(region, c("county", "state"), sep = ", ", remove = FALSE)
# Repeat with the median price data
rfmedian_metro <- rfmedian_metro %>%
  mutate(region = str_remove(region, " metro area")) %>%
  separate(region, c("city", "state"), sep = ", ", remove = FALSE)
rfmedian_county <- rfmedian_county %>%
  separate(region, c("county", "state"), sep = ", ", remove = FALSE)

# Calculate the difference between the median home price in 2019 and 2024
rfmedian_county <- rfmedian_county %>%
  mutate(diff_median_home = median_home_2024 - median_home_2019)
rfmedian_metro <- rfmedian_metro %>%
  mutate(diff_median_home = median_home_2024 - median_home_2019)
# Calculate the percentage increase between 2019 and 2024
rfmedian_county <- rfmedian_county %>%
  mutate(percent_increase_median = ((median_home_2024 - median_home_2019) / median_home_2019) * 100)
rfmedian_metro <- rfmedian_metro %>%
  mutate(percent_increase_median = ((median_home_2024 - median_home_2019) / median_home_2019) * 100)


###### EVALUATE WITH Z DATA
# Download and make a df of https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1721160925
starter_2br <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1721160925")

starter_2br <- starter_2br %>%
  select(2, 3, 5, ncol(.) - (5:0)*12) %>% select(1:4,9)
# name columns 
names(starter_2br) <- c("rank", "city", "state", "starter_home_2019", "starter_home_2024")
# calculate the difference between the 2019 and 2024 starter home price
starter_2br <- starter_2br %>%
  mutate(diff_starter_home = starter_home_2024 - starter_home_2019)
# calculate the percentage increase between 2019 and 2024
starter_2br <- starter_2br %>%
  mutate(percent_increase_starter = ((starter_home_2024 - starter_home_2019) / starter_home_2019) * 100)


# Download and make a df of https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1721160925
typical_home <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_month.csv")

# Repeat the same month from the five previous years
typical_home <- typical_home %>%
  select(2, 3, 5, ncol(.) - (5:0)*12) %>% select(1:4,9)
# name columns 
names(typical_home) <- c("rank", "city", "state", "typical_home_2019", "typical_home_2024")
# calculate the difference between the 2019 and 2024 starter home price
typical_home <- typical_home %>%
  mutate(diff_typical_home = typical_home_2024 - typical_home_2019)
# calculate the percentage increase between 2019 and 2024
typical_home <- typical_home %>%
  mutate(percent_increase_typical = ((typical_home_2024 - typical_home_2019) / typical_home_2019) * 100)

# create a new metro_starter df that combines the ppsf and median price data
metro_starter <- starter_2br %>%
  left_join(typical_home %>% select(-rank), by = c("city" = "city","state" = "state")) 

# Calculate mortgage payment with 6.5% interest for 30 years with 10 down for starter_home_2019 and starter_home_2024
metro_starter <- metro_starter %>%
  mutate(starter_mortgage_2019 = (starter_home_2019 * 0.007),
         starter_mortgage_2024 = (starter_home_2024 * 0.007))

# Repeat for the median price for 2019 and 2024
metro_starter <- metro_starter %>%
  mutate(typical_mortgage_2019 = (typical_home_2019 * 0.007),
         typical_mortgage_2024 = (typical_home_2024 * 0.007))

# Calculate starter_income_2019 and starter_income_2024 as if the mortage payment is 28% of the income
metro_starter <- metro_starter %>%
  mutate(starter_income_2019 = starter_mortgage_2019 * 12 * 3.33333,
         starter_income_2024 = starter_mortgage_2024 * 12 * 3.33333)

# Calculate a required income to afford the median home price in 2019 and 2024 if 12 months of payments needs to be 30% of income
metro_starter <- metro_starter %>%
  mutate(typical_income_2019 = typical_mortgage_2019 * 12 * 3.33333,
         typical_income_2024 = typical_mortgage_2024 * 12 * 3.33333)

# Calculate the difference in the starter_income and median_income
metro_starter <- metro_starter %>%
  mutate(diff_starter_income = starter_income_2024 - starter_income_2019,
         diff_typical_income = typical_income_2024 - typical_income_2019)
# Calculate the percentage increase in the starter_income and median_income
metro_starter <- metro_starter %>%
  mutate(percent_increase_starter_income = ((starter_income_2024 - starter_income_2019) / starter_income_2019) * 100,
         percent_increase_typical_income = ((typical_income_2024 - typical_income_2019) / typical_income_2019) * 100)

# Round to zero decimal places
metro_starter <- metro_starter %>%
  mutate(across(4:23, round, digits = 0))

# Add a column called starter_more if the percent increase for starter income is higher than the percent increase for typical income
metro_starter <- metro_starter %>%
  mutate(starter_more = ifelse(percent_increase_starter_income > percent_increase_typical_income, "yes", "no"))

# Sort by SizeRank and then select the top 100 items
metro_starter <- metro_starter %>%
  arrange(rank) %>%
  head(101)




