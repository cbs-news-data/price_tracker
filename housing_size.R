library(tidyverse)

# Download and make a df of https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1721160925
homes_2br <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1721160925")

homes_2br <- homes_2br %>%
  select(3, 5, ncol(.) - (5:0)*12)

# Download and make a df of https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1721160925
homes_allbr <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_month.csv")
homes_allbr_sm <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")


# Repeat the same month from the five previous years
homes_allbr <- homes_allbr %>%
  select(3, 5, ncol(.) - (5:0)*12)
# Rpeat for allbr_sm
homes_allbr_sm <- homes_allbr_sm %>%
  select(3, 5, ncol(.) - (5:0)*12)
