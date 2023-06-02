### Data import, cleaning and transformation for budburst scoring and information from mother trees.
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-02
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)


#### Budburst 23 ####
### Importing budburst data from 2023
budburst_zh23 <- read.csv("~/budburst/data/processed/budburst-zurich-2023.csv", stringsAsFactors=TRUE)


## Checking out the data
glimpse(budburst_zh23)
## Checking NAs
budburst_zh23 %>%
  summarise(across(everything(), ~ sum(is.na(.))))


### Cleaning the data
## Remove unwanted rows
# drop na and remove dead
budburst_zh23_clean <- budburst_zh23 %>%
  drop_na(budburst_score)  %>%
  filter(notes_2023 != "DEAD")

# check how many acorns were removed
budburst_zh23 %>%
  summarise(n_distinct(acorn_id))
budburst_zh23_clean %>%
  summarise(n_distinct(acorn_id))
### total acorns planted: 1143
### total acorns measured: 780
### total acorns dropped: 363


### Transforming the data
budburst_zh23_transformed <- budburst_zh23_clean %>%
  # new column: changing date to day of year
  mutate(date = as.Date(date, format = "%d.%m.%y")) %>%
  mutate(day_of_year = lubridate::yday(date)) %>%
  
  ## add mother_id column for joining data frames
  # if starts with a K, remove this
  # then remove last two characters from all
  mutate(mother_id = case_when(str_detect(acorn_id, "K") ~ str_sub(acorn_id, 3, str_length(acorn_id)),
                               TRUE ~ acorn_id)) %>%
  mutate(mother_id = str_sub(mother_id, 1, -3)) %>%
  # add column for age cohort
  mutate(cohort = case_when(str_detect(acorn_id, "K") ~ "2023_3", TRUE ~ "2023_2")) %>%
  mutate(age = case_when(str_detect(acorn_id, "K") ~ "3", TRUE ~ "2")) %>%
  mutate(year = "2023")


#### Budburst 22 ####
### Importing budburst data from 2022
budburst_zh22 <- read.csv("~/budburst/data/processed/budburst-zurich-2022.csv", stringsAsFactors=TRUE)

## Checking out the data
glimpse(budburst_zh22)
## Checking NAs
budburst_zh22 %>%
  summarise(across(everything(), ~ sum(is.na(.))))


### Cleaning the data
## Remove unwanted rows
# drop na and remove dead
budburst_zh22_clean <- budburst_zh22 %>%
  drop_na(budburst_score)  %>%
  filter(notes_2022 != "DEAD") %>%
  filter(notes_2022 != "MISSING")


# check how many acorns were removed
budburst_zh22 %>%
  summarise(n_distinct(acorn_id))
budburst_zh22_clean %>%
  summarise(n_distinct(acorn_id))
### total acorns planted: 366
### total acorns measured: 356
### total acorns dropped: 10



### Transforming the data
budburst_zh22_transformed <- budburst_zh22_clean %>%
  # new column: changing date to day of year
  mutate(date = as.Date(date, format = "%d.%m.%y")) %>%
  mutate(day_of_year = lubridate::yday(date)) %>%
  ## add mother_id column for joining data frames
  # if starts with a K, remove this
  # then remove last two characters from all
  mutate(mother_id = case_when(str_detect(acorn_id, "K") ~ str_sub(acorn_id, 3, str_length(acorn_id)),
                               TRUE ~ acorn_id)) %>%
  mutate(mother_id = str_sub(mother_id, 1, -3)) %>%
  # add column for age cohort
  mutate(cohort = "2022_2") %>%
  mutate(age = "2") %>%
  mutate(year = "2022")



#### Meteorological Data ####
### Import meteorological data for Zurich Site 
weather_zh_2023 <- read.csv("~/budburst/data/processed/weather-zurich-2023.csv", stringsAsFactors=TRUE)
weather_zh_2022 <- read.csv("~/budburst/data/processed/weather-zurich-2022.csv", stringsAsFactors=TRUE)


## check out data
glimpse(weather_zh_2023)
## Checking NAs
weather_zh_2023 %>%
  summarise(across(everything(), ~ sum(is.na(.))))

## check out data
glimpse(weather_zh_2022)
## Checking NAs
weather_zh_2022 %>%
  summarise(across(everything(), ~ sum(is.na(.))))

### Transforming the data
weather_zh_2023 <- weather_zh_2023 %>%
  # Change date to DOY
  mutate(date = as.character(MESSDAT)) %>%
  mutate(date = clock::date_time_parse_RFC_3339(date)) %>%
  mutate(day_of_year = lubridate::yday(date)) %>%
  mutate(year = "2023") %>%
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
## combine budburst 2022 and 2023
budburst_zh_all <- bind_rows(budburst_zh22_transformed, budburst_zh23_transformed)

## make a new DF: Date of First Stage 2
## if not measured, make linear interpolation between the two neighbouring measurements

# find all for which stage 2 was measured
direct_first_stage_2 <- budburst_zh_all %>%
  filter(budburst_score == 2) %>%
  group_by(acorn_id) %>%
  slice_max(day_of_year) %>%
  mutate(doy_stage_2 = day_of_year)
### 728 observations

# for those who went directly to stage 3, 4 or 5, make linear interpolation
# filter those for which stage 2 was not measured
stage_2_missed <- budburst_zh_all %>%
  filter(!acorn_id %in% direct_first_stage_2$acorn_id)
nrow(distinct(stage_2_missed, acorn_id))
### 233
nrow(distinct(budburst_zh_all, acorn_id))
### sanity check: 233 + 728 = 961, ie all measurements accounted for

# find first observation after stage 2 was reached
stage_2_post <- stage_2_missed %>%
  group_by(acorn_id) %>%
  filter(budburst_score >= 2) %>%
  slice_min(day_of_year) %>%
  rename(budburst_score_post_2 = budburst_score) %>%
  rename(day_of_year_post_2 = day_of_year) %>%
  select(acorn_id, day_of_year_post_2, budburst_score_post_2)
### 233 observations

# select last observation for each acorn.id before stage 2
stage_2_pre <- stage_2_missed %>%
  group_by(acorn_id) %>%
  filter(budburst_score <= 2) %>%
  slice_max(day_of_year) %>%
  rename(budburst_score_pre_2 = budburst_score) %>%
  rename(day_of_year_pre_2 = day_of_year)
### 233 observations

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
### 961 in total
### sanity check: same as unique in budburst_zh_all

# clean up df
stage_2 <- stage_2_all %>%
  select(acorn_id, mother_id, doy_stage_2, cohort, age, year)
# checking NAs
which(is.na(stage_2))

# combine stage 2 and mother info
stage_2_combined_mother <- left_join(stage_2, mother_info, by = "mother_id")
### 961 observations
# for 24 acorns, no mother info found (these arrived late and must be excluded from analysis)
stage_2_combined_mother <- stage_2_combined_mother %>%
  drop_na(species)
### total: 937 acorns for analysis

# add cumulative temperature
stage_2_combined_mother <-  stage_2_combined_mother %>%
  mutate(day_of_year = round(doy_stage_2))

by <- join_by(day_of_year, year == year)
stage_2_combined_mother_weather <- left_join(stage_2_combined_mother, weather_zh_2023, by = by)

# drop not relevant columns
stage_2_for_analysis <- stage_2_combined_mother_weather %>%
  select(-c(18:26))

### export DF for stage 2
write_csv(stage_2_for_analysis, "~/budburst/data/processed/stage_2_for_analysis.csv")



#### Time 2 To 5 ####
