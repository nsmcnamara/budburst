### Model for chlorophyll (Apogee)
### This script is part of the ACORN budburst analysis project
### Last update:  2023-07-10
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)

# resolve conflicts
library(conflicted)


#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)
chelsa <- read.csv("~/budburst/data/processed/coordinates_chelsa_values.csv", stringsAsFactors = TRUE)
sumstat_gdd_120 <- read.csv("~/budburst/data/processed/sumstat_gdd-120-1980-2019.csv", stringsAsFactors = TRUE)
chlorophyll_zh <- read.csv("~/budburst/data/processed/chlorophyll_zurich-2023.csv", stringsAsFactors = TRUE)

str(chlorophyll_zh)

# check NAs
sapply(chlorophyll_zh, function(x) sum(is.na(x)))

#### Data Wrangling ####
# use only 2023 data
stage_2_2023 <- stage_2_for_analysis %>%
  filter(year == "2023")
# inner join drops where no complete information available 
chlorophyll <- inner_join(chlorophyll_zh, stage_2_2023, by = "acorn_id")

## add chelsa to DF
# drop unnecessary columns
chelsa <- chelsa %>%
  select(- c(Long, Lat, optional))

# rename
colnames(chelsa) <- gsub("CHELSA_|_1981.2010_V.2.1", "", colnames(chelsa))
chelsa <- chelsa %>%
  rename(temp_ann_mean = bio1) %>%
  rename(precip_ann = bio12) %>%
  rename(precip_seasonality = bio15) %>%
  mutate(kg2 = as.factor(kg2))

# join chelsa
chlorophyll <- left_join(chlorophyll, chelsa, by = "mother_id")

# add gdd sumstats to DF
# rename
sumstat_gdd_120 <- sumstat_gdd_120 %>%
  rename(mean_gdd_120 = mean) %>%
  rename(var_gdd_120 = var)

# join
chlorophyll <- left_join(chlorophyll, sumstat_gdd_120, by = "site_name")
