### Exploratory Data Analysis for Stage 2 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-05-26
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)
library(GGally)

#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)

glimpse(stage_2_for_analysis)
head(stage_2_for_analysis)
summary(stage_2_for_analysis)
str(stage_2_for_analysis)

### check NAs
sapply(stage_2_for_analysis, function(x) sum(is.na(x)))


#### First Data Viz ####

### Histogram by species
ggplot(stage_2_for_analysis,
       mapping = aes(doy_stage_2, ..density.., fill = species, alpha = 0.5)) +
  geom_histogram() +
  geom_density() +
  facet_wrap( ~species)

### Automatic EDA by ggally
stage_2_for_analysis %>%
  select(doy_stage_2, species, altitude, latitude, longitude, age) %>%
  ggpairs(mapping = aes(color = species, alpha = 0.5))


## make a raincloudplot of doy by species
## make a plot of doy by site_name

