### Data import, cleaning and transformation for budburst scoring and information from mother trees.
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-02
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)


#### Budburst 23 ####
### Importing budburst data from 2023
budburst <- read.csv("~/budburst/data/processed/budburst-zurich.csv", stringsAsFactors=TRUE)


## Checking out the data
glimpse(budburst)
## Checking NAs
budburst %>%
  summarise(across(everything(), ~ sum(is.na(.))))


### Cleaning the data
## Remove unwanted rows
# drop na and remove dead
budburst_clean <- budburst %>%
  drop_na(budburst_score)  %>%
  filter(notes_2023 != "DEAD")

# check how many acorns were removed
budburst %>%
  summarise(n_distinct(acorn_id))
budburst_clean %>%
  summarise(n_distinct(acorn_id))
### total acorns planted: 1143
### total acorns measured: 780
### total acorns dropped: 363


### Transforming the data
budburst_transformed <- budburst_clean %>%
  # new column: changing date to day of year
  mutate(date = as.Date(date, format = "%d.%m.%y")) %>%
  mutate(day_of_year = lubridate::yday(date)) %>%
  
  ## add mother_id column for joining data frames
  # if starts with a K, remove this
  # then remove last two characters from all
  mutate(mother_id = case_when(str_detect(acorn_id, "K") ~ str_sub(acorn_id, 3, str_length(acorn_id)),
                               TRUE ~ acorn_id)) %>%
  mutate(mother_id = str_sub(mother_id, 1, -3))



#### Meteorological Data ####
### Import meteorological data for Zurich Site 
zurich_weather_jan_till_may_2023 <- read.csv("~/budburst/data/raw/meteo_UIF_2023-05-31.csv", stringsAsFactors=TRUE)

## check out data
glimpse(zurich_weather_jan_till_may_2023)
## Checking NAs
zurich_weather_jan_till_may_2023 %>%
  summarise(across(everything(), ~ sum(is.na(.))))

### Transforming the data
zurich_weather_jan_till_may_2023 <- zurich_weather_jan_till_may_2023 %>%
  # Change date to DOY
  mutate(date = as.character(MESSDAT)) %>%
  mutate(date = clock::date_time_parse_RFC_3339(date)) %>%
  mutate(day_of_year = lubridate::yday(date)) %>%
  # calculate temp above 5 degrees per day
  mutate(mean_temp_above_5 = case_when(MESSWERT_mean >= 5 ~ MESSWERT_mean - 5, .default = 0)) %>%
  # cumulative temp
  mutate(gdd_above_5 = cumsum(mean_temp_above_5)) %>%
  mutate(gdd_above_0 = cumsum(MESSWERT_mean))



#### Mother Info #####
### Importing information from mother trees
mother_info <- read.csv("~/budburst/data/processed/mother-info.csv", stringsAsFactors=TRUE)

## Checking out the data
glimpse(mother_info)  
## Checking NAs
mother_info %>%
  summarise(across(everything(), ~ sum(is.na(.))))


#### Stage 2 DF ####
## make a new DF: Date of First Stage 2
## if not measured, make linear interpolation between the two neighbouring measurements

# find all for which stage 2 was measured
direct_first_stage_2 <- budburst_transformed %>%
  filter(budburst_score == 2) %>%
  group_by(acorn_id) %>%
  slice_max(day_of_year) %>%
  mutate(doy_stage_2 = day_of_year)
### 639 observations

# for those who went directly to stage 3, 4 or 5, make linear interpolation
# filter those for which stage 2 was not measured
stage_2_missed <- budburst_transformed %>%
  filter(!acorn_id %in% direct_first_stage_2$acorn_id)
nrow(distinct(stage_2_missed, acorn_id))
### 141
### sanity check: 141 + 639 = 780, ie all measurements accounted for

# find first observation after stage 2 was reached
stage_2_post <- stage_2_missed %>%
  group_by(acorn_id) %>%
  filter(budburst_score >= 2) %>%
  slice_min(day_of_year) %>%
  rename(budburst_score_post_2 = budburst_score) %>%
  rename(day_of_year_post_2 = day_of_year) %>%
  select(acorn_id, day_of_year_post_2, budburst_score_post_2)
### 141 observations

# select last observation for each acorn.id before stage 2
stage_2_pre <- stage_2_missed %>%
  group_by(acorn_id) %>%
  filter(budburst_score <= 2) %>%
  slice_max(day_of_year) %>%
  rename(budburst_score_pre_2 = budburst_score) %>%
  rename(day_of_year_pre_2 = day_of_year)
### 141 observations, 

# combine dfs
stage_2_missed_combined <- inner_join(stage_2_pre, stage_2_post, by = "acorn_id", keep = FALSE)

# linear interpolation of stage 2
stage_2_interpolated <- stage_2_missed_combined %>%
  mutate(difference_days = day_of_year_post_2 - day_of_year_pre_2) %>%
  mutate(difference_budburst_score = budburst_score_post_2 - budburst_score_pre_2) %>%
  mutate(days_per_score_step = difference_days/difference_budburst_score) %>%
  mutate(amount_steps = 2 - budburst_score_pre_2) %>%
  mutate(doy_stage_2 = day_of_year_pre_2 + (amount_steps * days_per_score_step))

# combine calculated and interpolated stage 2 df
stage_2_all <- rbind(direct_first_stage_2, stage_2_interpolated)
### 780 in total
### sanity check: 639 + 141 = 780 --> ok

# clean up df
stage_2 <- stage_2_all %>%
  select(planting_location, acorn_id, mother_id, doy_stage_2)
# checking NAs
which(is.na(stage_2))

# combine stage 2 and mother info
stage_2_combined_mother <- left_join(stage_2, mother_info, by = "mother_id")
# no mother info found (these arrived late and must be excluded from analysis)
stage_2_combined_mother <- stage_2_combined_mother %>%
  drop_na(species)
### total: 756 acorns for analysis

# add column about age
stage_2_combined_mother <- stage_2_combined_mother %>%
  mutate(age = case_when(str_detect(acorn_id, "K") ~ 3, TRUE ~ 2))

# add cumulative temperature
stage_2_combined_mother <-  stage_2_combined_mother %>%
  mutate(day_of_year = round(doy_stage_2))

stage_2_combined_mother_weather <- left_join(stage_2_combined_mother, zurich_weather_jan_till_may_2023, by = "day_of_year")

# drop not relevant columns
stage_2_for_analysis <- stage_2_combined_mother_weather %>%
  select(-c(17:26))

### export DF for stage 2
write_csv(stage_2_for_analysis, "~/budburst/data/processed/stage_2_for_analysis.csv")

