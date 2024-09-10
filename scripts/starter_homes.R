library(tidyverse)
library(tidycensus)

# Download Zillow's home value data by metro for 2br homes as a stand-in for 'starter homes'
# We looked at multiple other datasets and methods to estimate this data
# But Zillow's data was the most consistent in terms of data and expert perspectives
starter_2br <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1721160925")
# drop the final column
starter_2br <- starter_2br %>% select(-ncol(.))

# Extract the most recent month and the same month in 2019
starter_2br <- starter_2br %>%
  select(2, 3, 5, ncol(.) - (5:0)*12) %>% select(1:4,9)
# Rename the columns
names(starter_2br) <- c("rank", "city", "state", "starter_home_2019", "starter_home_2024")
# Calculate difference between the 2019 and 2024 starter home price
starter_2br <- starter_2br %>%
  mutate(diff_starter_home = starter_home_2024 - starter_home_2019)
# Calculate % increase between 2019 and 2024
starter_2br <- starter_2br %>%
  mutate(percent_increase_starter = ((starter_home_2024 - starter_home_2019) / starter_home_2019) * 100)

# Download Zillow's typical home value data by metro for all homes as a stand-in for 'typical homes'
# We looked at median, list, sales, etc. and upon review the most accurate and up to date measure of the real market is the Zillow value estimate
typical_home <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_month.csv")
# drop the final column
typical_home <- typical_home %>% select(-ncol(.))

# Extract the most recent month and the same month in 2019
typical_home <- typical_home %>%
  select(2, 3, 5, ncol(.) - (5:0)*12) %>% select(1:4,9)
# Rename the columns
names(typical_home) <- c("rank", "city", "state", "typical_home_2019", "typical_home_2024")
# Calculate difference between the 2019 and 2024 typical home price
typical_home <- typical_home %>%
  mutate(diff_typical_home = typical_home_2024 - typical_home_2019)
# Calculate % increase between 2019 and 2024 for typical home price
typical_home <- typical_home %>%
  mutate(percent_increase_typical = ((typical_home_2024 - typical_home_2019) / typical_home_2019) * 100)

# Create a new metro_starter dataframe for analysis of starter home and typical home values
metro_starter <- starter_2br %>%
  left_join(typical_home %>% select(-rank), by = c("city" = "city","state" = "state")) 

# Calculate mortgage payment for 30 years with 10% down for starter_home_2019 and starter_home_2024
# Used 6.6% interest for 2024 and 4% for 2019
metro_starter <- metro_starter %>%
  mutate(starter_mortgage_2019 = (starter_home_2019 * 0.0055),
         starter_mortgage_2024 = (starter_home_2024 * 0.007))

# Repeat for the median price for 2019 and 2024
# Used 6.6% interest for 2024 and 4% for 2019
metro_starter <- metro_starter %>%
  mutate(typical_mortgage_2019 = (typical_home_2019 * 0.0055),
         typical_mortgage_2024 = (typical_home_2024 * 0.007))

# Calculate starter_income_2019 and starter_income_2024 as if the total annual mortgage cost is 30% of the annual income
metro_starter <- metro_starter %>%
  mutate(starter_income_2019 = starter_mortgage_2019 * 12 * 3.33333,
         starter_income_2024 = starter_mortgage_2024 * 12 * 3.33333)

# Calculate a required income to afford the median home price in 2019 and 2024 if 12 months of payments needs to be 30% of income
metro_starter <- metro_starter %>%
  mutate(typical_income_2019 = typical_mortgage_2019 * 12 * 3.33333,
         typical_income_2024 = typical_mortgage_2024 * 12 * 3.33333)

# Calculate the difference in the required starter_income and required median_income from 2019 to 2024
metro_starter <- metro_starter %>%
  mutate(diff_starter_income = starter_income_2024 - starter_income_2019,
         diff_typical_income = typical_income_2024 - typical_income_2019)

# Remove the comma, space and state abbreviation, everything after comma from city field
# But preserve the combined city-state field by creating new one at end, possible for charting
metro_starter$metro <- metro_starter$city
metro_starter <- metro_starter %>%
  mutate(city = str_remove(city, ", .*"))

# Round $ and % numerical data to zero decimal places
metro_starter <- metro_starter %>%
  mutate(across(4:21, round, digits = 0))

# Edit city name to change Ubran Honolulu to Honolulu
metro_starter$city[metro_starter$city == "Urban Honolulu"] <- "Honolulu"
# Change the state on the city WA to DC
metro_starter$state[metro_starter$city == "Washington"] <- "DC"

# Add a column called starter_more to flag if % increase for starter income is higher than % increase for typical income
metro_starter <- metro_starter %>%
  mutate(starter_up_more = ifelse(percent_increase_starter > percent_increase_typical, "yes", "no"))

# Sort metro by size (use Zillow size for consistency) and 
# then select the top 100 markets plus the United States national figure
metro_starter <- metro_starter %>%
  arrange(rank) %>%
  head(101)

# Use tidyverse to get the median household income for metro areas in the United States
# This data is from the 2019 American Community Survey 5-year estimates
v22 <- load_variables(2022, "acs1", cache = TRUE)
income <- get_acs(geography = "cbsa", variables = "B19013_001", output = "wide", year=2022, survey = "acs1", cache = TRUE) %>% janitor::clean_names()
# Add a new column to income called city that is the string before the comma in the name column
income$city <- str_remove(income$name, ", .*")
# Retain in the city column only the text before the first hyphen
income$city <- str_remove(income$city, "-.*")
# Create a state column starting by extracting all text after the first comma and space in the name column
income$state <- str_extract(income$name, ", .*")
# Now keep just the third and fourth character in that string
income$state <- str_sub(income$state, 3, 4)
# drop columns 1 and 4
income <- income %>% select(-c(4))
# Rename the columns as metro, income, city, state
names(income) <- c("geoid","metro", "med_hhincome", "city", "state")
# Reorder as city, state, income, metro
income <- income %>% select(geoid, city, state, med_hhincome, metro)
# Edit city name to change Ubran Honolulu to Honolulu
income$city[income$city == "Urban Honolulu"] <- "Honolulu"
# Change city name for Louisville/Jefferson County to just Louisville
income$city[income$city == "Louisville/Jefferson County"] <- "Louisville"

# Merge the income data with the metro_starter data
metro_starter <- metro_starter %>%
  left_join(income %>% select(1:4), by = c("city" = "city","state" = "state"))

# Add median income of 74755 to the United States row
metro_starter$med_hhincome[metro_starter$city == "United States"] <- 74755
# Add geoid of 1 to the United States row
metro_starter$geoid[metro_starter$city == "United States"] <- 1

# Create a flag if the median household income is below the required income for starter home
metro_starter <- metro_starter %>%
  mutate(income_toolow_starter = ifelse(med_hhincome < starter_income_2024, "yes", "no"))
# Repeat for typical home
metro_starter <- metro_starter %>%
  mutate(income_toolow_typical = ifelse(med_hhincome < typical_income_2024, "yes", "no"))

# define the required income group for the metro areas for 2024
metro_starter <- metro_starter %>%
  mutate(typical_income_group_24 = case_when(
    typical_income_2024 < 50000 ~ "under50k",
    typical_income_2024 < 63000 & typical_income_2024 > 50000  ~ "under60k",
    typical_income_2024 < 79000 & typical_income_2024 > 63000  ~ "under75k",
    typical_income_2024 < 105000 & typical_income_2024 > 79000  ~ "under100k",
    typical_income_2024 < 130000 & typical_income_2024 > 105000  ~ "under125k",
    typical_income_2024 < 155000 & typical_income_2024 > 130000  ~ "under150k",
    typical_income_2024 < 180000 & typical_income_2024 > 155000  ~ "under175k",
    typical_income_2024 < 205000 & typical_income_2024 > 180000  ~ "under200k", 
    typical_income_2024 > 205000 ~ "over200k",
    TRUE ~ "unknown"
  ))
# Repeat for starter income for 2024
metro_starter <- metro_starter %>%
  mutate(starter_income_group_24 = case_when(
    starter_income_2024 < 50000 ~ "under50k",
    starter_income_2024 < 63000 & starter_income_2024 > 50000  ~ "under60k",
    starter_income_2024 < 79000 & starter_income_2024 > 63000  ~ "under75k",
    starter_income_2024 < 105000 & starter_income_2024 > 79000  ~ "under100k",
    starter_income_2024 < 130000 & starter_income_2024 > 105000  ~ "under125k",
    starter_income_2024 < 155000 & starter_income_2024 > 130000  ~ "under150k",
    starter_income_2024 < 180000 & starter_income_2024 > 155000  ~ "under175k",
    starter_income_2024 < 205000 & starter_income_2024 > 180000  ~ "under200k",  
    starter_income_2024 > 205000 ~ "over200k",
    TRUE ~ "unknown"
  ))

# Repeat for typical income for 2019
metro_starter <- metro_starter %>%
  mutate(typical_income_group_19 = case_when(
    typical_income_2019 < 50000 ~ "under50k",
    typical_income_2019 < 63000 & typical_income_2019 > 50000  ~ "under60k",
    typical_income_2019 < 79000 & typical_income_2019 > 63000  ~ "under75k",
    typical_income_2019 < 105000 & typical_income_2019 > 79000  ~ "under100k",
    typical_income_2019 < 130000 & typical_income_2019 > 105000  ~ "under125k",
    typical_income_2019 < 155000 & typical_income_2019 > 130000  ~ "under150k",
    typical_income_2019 < 180000 & typical_income_2019 > 155000  ~ "under175k",
    typical_income_2019 < 205000 & typical_income_2019 > 180000  ~ "under200k", 
    typical_income_2019 > 205000 ~ "over200k",
    TRUE ~ "unknown"
  ))

# Repeat for starter income for 2019
metro_starter <- metro_starter %>%
  mutate(starter_income_group_19 = case_when(
    starter_income_2019 < 50000 ~ "under50k",
    starter_income_2019 < 63000 & starter_income_2019 > 50000  ~ "under60k",
    starter_income_2019 < 79000 & starter_income_2019 > 63000  ~ "under75k",
    starter_income_2019 < 105000 & starter_income_2019 > 79000  ~ "under100k",
    starter_income_2019 < 130000 & starter_income_2019 > 105000  ~ "under125k",
    starter_income_2019 < 155000 & starter_income_2019 > 130000  ~ "under150k",
    starter_income_2019 < 180000 & starter_income_2019 > 155000  ~ "under175k",
    starter_income_2019 < 205000 & starter_income_2019 > 180000  ~ "under200k",  
    starter_income_2019 > 205000 ~ "over200k",
    TRUE ~ "unknown"
  ))

## GATHER INCOME BRACKETING FOR METROS
# Create an array from variables df called v22 of every variable name that begins B19001_
income_variables <- v22 %>% filter(str_detect(name, "B19001_"))
# Create a vector of the variable names
income_variables <- income_variables$name
# Create a table of income ranges
income_ranges <- get_acs(geography = "cbsa", variables = income_variables, output = "wide", year=2022, survey = "acs1", cache = TRUE) %>% janitor::clean_names()
income_ranges_us <- get_acs(geography = "us", variables = income_variables, output = "wide", year=2022, survey = "acs1", cache = TRUE) %>% janitor::clean_names()
# merge us into income_ranges
income_ranges <- rbind(income_ranges, income_ranges_us)
# Select columns we want by removing the columns with col names ending in m
income_ranges <- income_ranges %>% select(-ends_with("m"))
# Rename the columns to be more descriptive
names(income_ranges) <- c("geoid","name",
                          "under10k",
                          "10kto15k",
                          "15kto20k",
                          "20kto25k",
                          "25kto30k",
                          "30kto35k",
                          "35kto40k",
                          "40kto45k",
                          "45kto50k",
                          "50kto60k",
                          "60kto75k",
                          "75kto100k",
                          "100kto125k",
                          "125kto150k",
                          "150kto175k",
                          "175kto200k",
                          "200kormore")
# Add a column called totalhh that is the sum of all the income ranges
income_ranges$totalhh <- rowSums(income_ranges[,3:19])
# Calculate a percent under 50k
income_ranges$under50k <- rowSums(income_ranges[,3:11]) / income_ranges$totalhh
# Calculate a percent under 60k
income_ranges$under60k <- rowSums(income_ranges[,3:12]) / income_ranges$totalhh
# Calculate a percent under 75k
income_ranges$under75k <- rowSums(income_ranges[,3:13]) / income_ranges$totalhh
# Calculate a percent under 100k
income_ranges$under100k <- rowSums(income_ranges[,3:14]) / income_ranges$totalhh
# Calculate a percent under 125k
income_ranges$under125k <- rowSums(income_ranges[,3:15]) / income_ranges$totalhh
# Calculate a percent under 150k
income_ranges$under150k <- rowSums(income_ranges[,3:16]) / income_ranges$totalhh
# Calculate a percent under 175k
income_ranges$under175k <- rowSums(income_ranges[,3:17]) / income_ranges$totalhh
# Calculate a percent under 200k
income_ranges$under200k <- rowSums(income_ranges[,3:18]) / income_ranges$totalhh
# Calculate a percent over 200k
income_ranges$over200k <- income_ranges$`200kormore` / income_ranges$totalhh
# Multiple each of those by 100 to get a percentage and round to 1 decimal place
income_ranges <- income_ranges %>%
  mutate(across(under50k:over200k, ~round(. * 100, 1)))

# Add to income ranges the typical and starer income group label columns, join by geoid
income_ranges <- income_ranges %>%
  left_join(metro_starter %>% select
             (geoid, typical_income_group_19, starter_income_group_19, typical_income_group_24, starter_income_group_24), by = c("geoid" = "geoid"))
# Then drop all cities from income_ranges that do not have a typical_income_group
income_ranges <- income_ranges %>% filter(!is.na(typical_income_group_24) | geoid=="1")

# For each city in this table we want to look at the value in the typical_income_group column and then create a new column that returns the value from the column with an identical name
income_ranges <- income_ranges %>%
  rowwise() %>%
  mutate(typical_income_group_pct_24 = 100-cur_data()[[typical_income_group_24]]) %>%
  ungroup()
# Repeat the same for starter_income_group
income_ranges <- income_ranges %>%
  rowwise() %>%
  mutate(starter_income_group_pct_24 = 100-cur_data()[[starter_income_group_24]]) %>%
  ungroup()
# If the starter_income_group equals over200K, replace the starter_income_group_pct with the value in under200K
income_ranges$starter_income_group_pct_24[income_ranges$starter_income_group_24 == "over200k"] <- 100-income_ranges$under200k[income_ranges$starter_income_group_24 == "over200k"]
# If the typical_income_group equals over200K, replace the typical_income_group_pct with the value in under200K
income_ranges$typical_income_group_pct_24[income_ranges$typical_income_group_24 == "over200k"] <- 100-income_ranges$under200k[income_ranges$typical_income_group_24 == "over200k"]
# Now repeat the last four steps for the 2019 data
income_ranges <- income_ranges %>%
  rowwise() %>%
  mutate(typical_income_group_pct_19 = 100-cur_data()[[typical_income_group_19]]) %>%
  ungroup()
income_ranges <- income_ranges %>%
  rowwise() %>%
  mutate(starter_income_group_pct_19 = 100-cur_data()[[starter_income_group_19]]) %>%
  ungroup()
income_ranges$starter_income_group_pct_19[income_ranges$starter_income_group_19 == "over200k"] <- 100-income_ranges$under200k[income_ranges$starter_income_group_19 == "over200k"]
income_ranges$typical_income_group_pct_19[income_ranges$typical_income_group_19 == "over200k"] <- 100-income_ranges$under200k[income_ranges$typical_income_group_19 == "over200k"]

# round the starter_income_group_pct and typical_income_group_pct to 0 decimal place
#income_ranges <- income_ranges %>%
#  mutate(across(34:37, ~round(. , 0)))

# Now let's add those percentages that CAN afford into the metro_starter data analysis frame
metro_starter <- metro_starter %>%
  left_join(income_ranges %>% select(geoid, typical_income_group_pct_19, starter_income_group_pct_19, typical_income_group_pct_24, starter_income_group_pct_24), by = c("geoid" = "geoid"))

# Create a text list of city, state for all cases where the percent of starter home group in 19 is 33.3 or above
metro_starter$starter_afford_19 <- ifelse(metro_starter$starter_income_group_pct_19 > 33.3, paste(metro_starter$city, metro_starter$state, sep=", "), "")
# Repeat for 24
metro_starter$starter_afford_24 <- ifelse(metro_starter$starter_income_group_pct_24 > 33.3, paste(metro_starter$city, metro_starter$state, sep=", "), "")
# sort by starter_income_group_pct_24 and print a text file
metro_starter <- metro_starter %>%
  arrange(desc(starter_income_group_pct_24))
writeLines(metro_starter$starter_afford_24, "starter_afford_24.txt")
# Repeat for 19
metro_starter <- metro_starter %>%
  arrange(desc(starter_income_group_pct_19))
writeLines(metro_starter$starter_afford_19, "starter_afford_19.txt")




