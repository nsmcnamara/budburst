### Exploratory Data Analysis for Weather/Growth Stage Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-05
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)
library(RColorBrewer)

#### IDEA ####
# plot mean, min and max temp jan till may (weather graph)
# plot cumulative gdd
# include growth stage like here: https://www.arable.com/blog/a-guide-to-growing-degree-days-gdd-linking-temperature-and-crop-growth-stages/

# data needed
# weather
# need avg date for stage 1/2/3/4/5 overall

#### Data Import ####
weather_zh_2023_processed <- read.csv("~/budburst/data/processed/weather_zh_2023_processed.csv")

## Checking out the data
glimpse(weather_zh_2023_processed)
## Checking NA
weather_zh_2023_processed %>%
  summarise(across(everything(), ~ sum(is.na(.))))

## Change Date format ###
weather_zh_2023_processed <- weather_zh_2023_processed %>%
  mutate(date = as.character(date)) %>%
  mutate(date = clock::date_time_parse_RFC_3339(date)) %>%
  mutate(date = lubridate::as_date(date))


### Temp 2023 mean, min, max ####
ggplot(data = weather_zh_2023_processed, aes(x = date, y = temp_mean, group = 1)) +
  geom_line() +
  geom_ribbon(aes(x = date, ymax = temp_max, ymin = temp_min), alpha = 0.3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B")
