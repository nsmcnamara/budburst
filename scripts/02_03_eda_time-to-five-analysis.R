### Exploratory Data Analysis for Time to 5 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-06
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(GGally)
library(RColorBrewer)


#### Data Import ####
gdd_2_to_5 <- read.csv("~/budburst/data/processed/gdd_2_to_5.csv", stringsAsFactors=TRUE)

## Check out data
glimpse(gdd_2_to_5)
head(gdd_2_to_5)
summary(gdd_2_to_5)
str(gdd_2_to_5)

## check NAs
sapply(gdd_2_to_5, function(x) sum(is.na(x)))

### DF w/o NA (no weather data for 2022, ergo no gdd)
gdd_2_to_5_clean <- gdd_2_to_5 %>%
  drop_na(gdd_2_to_5)
  