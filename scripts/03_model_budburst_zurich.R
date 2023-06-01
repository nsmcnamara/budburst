### Model for budburst
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-01
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(lme4)


#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)

glimpse(stage_2_for_analysis)
head(stage_2_for_analysis)
summary(stage_2_for_analysis)
str(stage_2_for_analysis)

### check NAs
sapply(stage_2_for_analysis, function(x) sum(is.na(x)))


#### LMM ####
# model_growing_degree_days <- lmer(cum_temp_above_5 ~ species * site_name + (1|mother_id) + (1 + mother_id|age), data = stage_2_for_analysis)
# not working: fixed-effect model matrix is rank deficient so dropping 50 columns / coefficients

model_growing_degree_days <- lmer(cum_temp_above_5 ~ species * site_name + (1|mother_id) + (1|age), data = stage_2_for_analysis)

summary(model_growing_degree_days)
