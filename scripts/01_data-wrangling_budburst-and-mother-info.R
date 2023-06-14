### Data import, cleaning and transformation for budburst scoring and information from mother trees.
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-14
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
# weather_zh_2023 <- read.csv("~/budburst/data/processed/weather-zurich-2023.csv", stringsAsFactors=TRUE)
# weather_zh_2022 <- read.csv("~/budburst/data/processed/weather-zurich-2022.csv", stringsAsFactors=TRUE)
weather_zh_22_23 <- read.csv("~/budburst/data/processed/weather-zurich-2022-2023.csv", stringsAsFactors=TRUE)

## check out data
glimpse(weather_zh_22_23)
## Checking NAs
weather_zh_22_23 %>%
  summarise(across(everything(), ~ sum(is.na(.))))


### Transforming the data
weather_zh_22_23 <- weather_zh_22_23 %>%
  # rename columns to english
  rename(date = MESSDAT) %>%
  rename(temp_mean = MESSWERT_mean) %>%
  rename(temp_max = MESSWERT_max) %>%
  rename(temp_min = MESSWERT_min) %>%
  # Change date to DOY
  mutate(date = as.character(date)) %>%
  mutate(date = clock::date_time_parse_RFC_3339(date)) %>%
  mutate(day_of_year = lubridate::yday(date)) %>%
  mutate(year = lubridate::year(date))
  
## separate in 2023 and 2022
weather_zh_22 <- weather_zh_22_23 %>%
  filter(year == "2022") %>%
  dplyr::select(-c(1:4)) %>%
  # calculate temp above 5 degrees per day
  mutate(mean_temp_above_5 = case_when(temp_mean >= 5 ~ temp_mean - 5, .default = 0)) %>%
  # cumulative temp
  mutate(gdd_above_5 = cumsum(mean_temp_above_5))

weather_zh_23 <- weather_zh_22_23 %>%
  filter(year == "2023") %>%
  dplyr::select(-c(1:4)) %>%
  # calculate temp above 5 degrees per day
  mutate(mean_temp_above_5 = case_when(temp_mean >= 5 ~ temp_mean - 5, .default = 0)) %>%
  # cumulative temp
  mutate(gdd_above_5 = cumsum(mean_temp_above_5))



## check out data
glimpse(weather_zh_22)
## Checking NAs
weather_zh_22 %>%
  summarise(across(everything(), ~ sum(is.na(.))))

## check out data
glimpse(weather_zh_23)
## Checking NAs
weather_zh_23 %>%
  summarise(across(everything(), ~ sum(is.na(.))))


### export weather zurich 2023
write_csv(weather_zh_23, "~/budburst/data/processed/weather_zh_2023_processed.csv")
write_csv(weather_zh_22, "~/budburst/data/processed/weather_zh_2022_processed.csv")


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
budburst_zh_all <- bind_rows(budburst_zh22_transformed, budburst_zh23_transformed) %>%
# make new unique identifier
  mutate(UID = paste0(acorn_id, "_", year))
nrow(distinct(budburst_zh22_transformed, acorn_id))
nrow(distinct(budburst_zh23_transformed, acorn_id))
### 356 + 780 = 1136
nrow(distinct(budburst_zh_all, UID))
### 1136

## make a new DF: Date of First Stage 2
## if not measured, make linear interpolation between the two neighbouring measurements

#### replace this ####
# find all for which stage 2 was measured
direct_first_stage_2 <- budburst_zh_all %>%
  filter(budburst_score == 2) %>%
  group_by(UID) %>%
  slice_min(day_of_year) %>%
  mutate(doy_stage_2 = day_of_year)
### 788 observations

# for those who went directly to stage 3, 4 or 5, make linear interpolation
# filter those for which stage 2 was not measured
stage_2_missed <- budburst_zh_all %>%
  filter(!UID %in% direct_first_stage_2$UID)
nrow(distinct(stage_2_missed, UID))
### 348
nrow(distinct(budburst_zh_all, UID))
### sanity check: 348 + 788 = 1136, ie all measurements accounted for

# find first observation after stage 2 was reached
stage_2_post <- stage_2_missed %>%
  group_by(UID) %>%
  filter(budburst_score >= 2) %>%
  slice_min(day_of_year) %>%
  rename(budburst_score_post_2 = budburst_score) %>%
  rename(day_of_year_post_2 = day_of_year) %>%
  select(UID, acorn_id, day_of_year_post_2, budburst_score_post_2)
### 348 observations

# select last observation for each acorn.id before stage 2
stage_2_pre <- stage_2_missed %>%
  group_by(UID) %>%
  filter(budburst_score <= 2) %>%
  slice_max(day_of_year) %>%
  rename(budburst_score_pre_2 = budburst_score) %>%
  rename(day_of_year_pre_2 = day_of_year)
### 348 observations

# combine dfs
by_s2 <- join_by(UID, acorn_id == acorn_id)
stage_2_missed_combined <- left_join(stage_2_pre, 
                                     stage_2_post,
                                     by = by_s2)

# linear interpolation of stage 2
stage_2_interpolated <- stage_2_missed_combined %>%
  mutate(difference_days = day_of_year_post_2 - day_of_year_pre_2) %>%
  mutate(difference_budburst_score = budburst_score_post_2 - budburst_score_pre_2) %>%
  mutate(days_per_score_step = difference_days/difference_budburst_score) %>%
  mutate(amount_steps = 2 - budburst_score_pre_2) %>%
  mutate(doy_stage_2 = day_of_year_pre_2 + (amount_steps * days_per_score_step))

# ### improved but check! ####
# stage_2_interpolated <- stage_2_missed_combined %>%
#   mutate(
#     doy_stage_2 = day_of_year_pre_2 + ((2 - budburst_score_pre_2) * (day_of_year_post_2 - day_of_year_pre_2) / (budburst_score_post_2 - budburst_score_pre_2))
#   )

###
# combine calculated and interpolated stage 2 df
stage_2_all <- rbind(direct_first_stage_2, stage_2_interpolated)
### 1136 in total
### sanity check: same as unique in budburst_zh_all

# clean up df
stage_2 <- stage_2_all %>%
  select(UID, acorn_id, mother_id, doy_stage_2, cohort, age, year)
# checking NAs
which(is.na(stage_2))

# combine stage 2 and mother info
stage_2_combined_mother <- left_join(stage_2, mother_info, by = "mother_id")
### 1136 observations
# for 24 acorns, no mother info found (these arrived late and must be excluded from analysis)
stage_2_combined_mother <- stage_2_combined_mother %>%
  drop_na(species)
### total: 1112 acorns for analysis

#### replace until here ####
# 
# # Find all observations for which stage 2 was measured
# direct_first_stage_2_gpt <- budburst_zh_all %>%
#   filter(budburst_score == 2) %>%
#   group_by(UID) %>%
#   filter(day_of_year == min(day_of_year)) %>%
#   mutate(doy_stage_2 = day_of_year)
# 
# # Linear interpolation of stage 2
# stage_2_interpolated_gpt <- budburst_zh_all %>%
#   anti_join(direct_first_stage_2_gpt, by = "UID") %>%
#   group_by(UID) %>%
#   filter(budburst_score >= 2) %>%
#   filter(day_of_year == min(day_of_year)) %>%
#   rename(budburst_score_post_2 = budburst_score) %>%
#   rename(day_of_year_post_2 = day_of_year) %>%
#   select(UID, acorn_id, day_of_year_post_2, budburst_score_post_2) %>%
#   left_join(budburst_zh_all %>% filter(budburst_score <= 2) %>%
#               group_by(UID) %>%
#               filter(day_of_year == max(day_of_year)) %>%
#               rename(budburst_score_pre_2 = budburst_score) %>%
#               rename(day_of_year_pre_2 = day_of_year), by = "UID") %>%
#   mutate(difference_days = day_of_year_post_2 - day_of_year_pre_2) %>%
#   mutate(difference_budburst_score = budburst_score_post_2 - budburst_score_pre_2) %>%
#   mutate(days_per_score_step = difference_days / difference_budburst_score) %>%
#   mutate(amount_steps = 2 - budburst_score_pre_2) %>%
#   mutate(doy_stage_2 = day_of_year_pre_2 + (amount_steps * days_per_score_step))
# 
# # Combine calculated and interpolated stage 2 data frames
# stage_2_all_gpt <- bind_rows(direct_first_stage_2, stage_2_interpolated)
# 
# # Clean up data frame
# stage_2_gpt <- stage_2_all_gpt %>%
#   select(UID, acorn_id, mother_id, doy_stage_2, cohort, age, year)
# 
# # Combine stage 2 and mother info
# stage_2_combined_mother_gpt <- left_join(stage_2_gpt, mother_info, by = "mother_id") %>%
#   drop_na(species)
# 
# 
# identical(stage_2_combined_mother, stage_2_combined_mother_gpt)















### here not ####
# add cumulative temperature
stage_2_combined_mother <-  stage_2_combined_mother %>%
  mutate(day_of_year = round(doy_stage_2)) %>%
  mutate(year = as.numeric(year))

weather_zh_22_23_comb <- rbind(weather_zh_22, weather_zh_23)

by_add_weather <- join_by(day_of_year, year == year)
stage_2_combined_mother_weather <- left_join(stage_2_combined_mother, 
                                             weather_zh_22_23_comb%>% 
                                               select(day_of_year, year, gdd_above_5), 
                                             by = by_add_weather)

stage_2_for_analysis <- stage_2_combined_mother_weather %>%
  select(-day_of_year) %>%
  relocate(doy_stage_2, .before = gdd_above_5)

### export DF for stage 2
write_csv(stage_2_for_analysis, "~/budburst/data/processed/stage_2_for_analysis.csv")


#### Time 2 To 5 ####
### calculate how many growing degree days from stage 2 to stage 5

# find first_stage_5
direct_first_stage_5 <- budburst_zh_all %>%
  filter(budburst_score == 5) %>%
  group_by(UID) %>%
  slice_min(day_of_year) %>%
  mutate(doy_stage_5 = day_of_year) %>%
  mutate(year = as.numeric(year))
### 1134 observations: ok

# add gdd at time point of stage 5
by <- join_by(day_of_year, year == year)
direct_first_stage_5_weather_for_5 <- left_join(direct_first_stage_5, 
                                         weather_zh_22_23_comb %>% select(day_of_year, year, gdd_above_5),
                                         by = by) %>%
  rename(gdd_stage_5 = gdd_above_5) %>%
  select(UID, acorn_id, mother_id, cohort, age, year, doy_stage_5, gdd_stage_5)

# add doy stage 2

stage_2_and_5 <- left_join(direct_first_stage_5_weather_for_5,
                           stage_2_all %>% select(UID, doy_stage_2),
                           by = "UID") %>%
  mutate(doy_stage_2 = round(doy_stage_2))

# add gdd at time point of stage 2
by_weather_2 <- join_by(doy_stage_2 == day_of_year, year == year)
stage_2_and_5_weather <- left_join(stage_2_and_5, 
                                    weather_zh_22_23_comb %>% select(day_of_year, year, gdd_above_5),
                                                by = by_weather_2) %>%
  rename(gdd_stage_2 = gdd_above_5)


# take difference of gdd at stage 5 to gdd at stage 2
gdd_2_to_5 <- stage_2_and_5_weather %>%
  rowwise() %>%
  mutate(gdd_2_to_5 = gdd_stage_5 - gdd_stage_2)


# combine gdd_2_to_5 and mother info
gdd_2_to_5_combined_mother <- left_join(gdd_2_to_5, mother_info, by = "mother_id")
### 1134 observations
# for 24 acorns, no mother info found (these arrived late and must be excluded from analysis)
gdd_2_to_5_combined_mother <- gdd_2_to_5_combined_mother %>%
  drop_na(species)
### total: 1110 acorns for analysis

  

### export gdd_2_to_5
write_csv(gdd_2_to_5_combined_mother, "~/budburst/data/processed/gdd_2_to_5_for_analysis.csv")
  
