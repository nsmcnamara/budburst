### Data import, cleaning and transformation for budburst scoring and information from mother trees.
### This script is part of the ACORN budburst analysis project
### Last update:  2023-05-26
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)


#### Data Import ####
### Importing budburst scoring
budburst <- read.csv("~/budburst/data/processed/20230526_budburst-zurich_corrected.csv", stringsAsFactors=TRUE)

### Importing information from mother trees
mother_info <- read.csv("~/budburst/data/processed/20230526_mother-info_corrected.csv", stringsAsFactors=TRUE)

# Budburst
## Checking out the data
glimpse(budburst)
## Checking NAs
budburst %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Mother Info
## Checking out the data
glimpse(mother_info)  
## Checking NAs
mother_info %>%
  summarise(across(everything(), ~ sum(is.na(.))))


#### Transforming the data ####
# Budburst
## make new column with date as day of year
budburst <- budburst %>%
  mutate(date = as.Date(date, format = "%d.%m.%y")) %>%
  mutate(day_of_year = lubridate::yday(date))

## add mother_id column for joining data frames
budburst <- budburst %>%
  mutate(mother_id = case_when(str_detect(acorn_id, "K") ~ str_sub(acorn_id, 3, str_length(acorn_id)),
                               TRUE ~ acorn_id)) %>%
  mutate(mother_id = str_sub(mother_id, 1, -3))

## Drop NAs
budburst_drop_na <- budburst %>%
  drop_na(budburst_score)

### check how many acorns were dropped
budburst %>%
  summarise(n_distinct(acorn_id))
budburst_drop_na %>%
  summarise(n_distinct(acorn_id))
### total acorns planted: 1143
### total acorns measured: 793
### total acorns dropped: 350

#### DF for Stage 2 ####
## make a new DF: Date of First Stage 2
## if not measured, make linear interpolation between the two neighbouring measurements

### find all for which stage 2 was measured
direct_first_stage_2 <- budburst_drop_na %>%
  filter(budburst_score == 2) %>%
  group_by(acorn_id) %>%
  slice_max(day_of_year) %>%
  mutate(doy_stage_2 = day_of_year)
### 639 observations

### for those who went directly to stage 3, 4 or 5, make linear interpolation
### filter those for which stage 2 was not measured
stage_2_missed <- budburst_drop_na %>%
  filter(!acorn_id %in% direct_first_stage_2$acorn_id)
nrow(distinct(stage_2_missed, acorn_id))
### 154
### sanity check: 154 + 639 = 793, ie all measurements accounted for

### find first observation after stage 2 was reached
stage_2_post <- stage_2_missed %>%
  group_by(acorn_id) %>%
  filter(budburst_score >= 2) %>%
  slice_min(day_of_year) %>%
  rename(budburst_score_post_2 = budburst_score) %>%
  rename(day_of_year_post_2 = day_of_year) %>%
  select(acorn_id, day_of_year_post_2, budburst_score_post_2)
### 141 observations

### select last observation for each acorn.id before stage 2
stage_2_pre <- stage_2_missed %>%
  group_by(acorn_id) %>%
  filter(budburst_score <= 2) %>%
  slice_max(day_of_year) %>%
  rename(budburst_score_pre_2 = budburst_score) %>%
  rename(day_of_year_pre_2 = day_of_year)
### 154 observations, 

### check why 13 have pre but not post
stage_2_pre %>%
  filter(!acorn_id %in% stage_2_post$acorn_id)
### 11 dead, 1 missing, 1 stuck on budburst score 1 --> ok

### combine dfs
stage_2_missed_combined <- inner_join(stage_2_pre, stage_2_post, by = "acorn_id", keep = FALSE)

### linear interpolation of stage 2
stage_2_interpolated <- stage_2_missed_combined %>%
  mutate(difference_days = day_of_year_post_2 - day_of_year_pre_2) %>%
  mutate(difference_budburst_score = budburst_score_post_2 - budburst_score_pre_2) %>%
  mutate(days_per_score_step = difference_days/difference_budburst_score) %>%
  mutate(amount_steps = 2 - budburst_score_pre_2) %>%
  mutate(doy_stage_2 = day_of_year_pre_2 + (amount_steps * days_per_score_step))

### combine calculated and interpolated stage 2 df
stage_2_all <- rbind(direct_first_stage_2, stage_2_interpolated)
### 780 in total
### sanity check: 639 + 141 = 780 --> ok

### clean up df
stage_2 <- stage_2_all %>%
  select(planting_location, acorn_id, mother_id, doy_stage_2)
### checking NAs
which(is.na(stage_2))


### combine stage 2 and mother info
stage_2_for_analysis <- left_join(stage_2, mother_info, by = "mother_id")
### no mother info found (these arrived late and must be excluded from analysis)
stage_2_for_analysis <- stage_2_for_analysis %>%
  drop_na(species)
### total: 756 acorns for analysis

### add column about age
stage_2_for_analysis <- stage_2_for_analysis %>%
  mutate(age = case_when(str_detect(acorn_id, "K") ~ 3, TRUE ~ 2))


### export DF for stage 2
write_csv(stage_2_for_analysis, "~/budburst/data/processed/stage_2_for_analysis.csv")


#### Meteorological Data ####
### Import meteorological data for Zurich Site 
zurich_weather_jan_till_may_2023 <- read.csv("~/budburst/data/raw/meteo_UIF_2023-05-31.csv", stringsAsFactors=TRUE)

### check out data
glimpse(zurich_weather_jan_till_may_2023)
## Checking NAs
zurich_weather_jan_till_may_2023 %>%
  summarise(across(everything(), ~ sum(is.na(.))))


