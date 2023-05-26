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


## make a new DF: Date of First Stage 2
## if not measured, make linear interpolation between the two neighbouring measurements

### find all for which stage 2 was measured
direct_first_stage_2 <- budburst_drop_na %>%
  filter(budburst_score == 2) %>%
  group_by(acorn_id) %>%
  slice_max(day_of_year) %>%
  mutate(doy_stage_2 = day_of_year)
### 639 observations
