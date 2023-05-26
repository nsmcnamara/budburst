### Exploratory Data Analysis for Stage 2 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-05-26
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)
library(FactoMineR)

#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)

glimpse(stage_2_for_analysis)

#### First Data Viz ####

hist(stage_2_for_analysis$doy_stage_2)

# PCA
# reorder columns
stage_2_for_analysis <- stage_2_for_analysis %>%
  mutate(age = as.numeric(age)) %>%
  select(5:8, everything())

pca <- PCA(stage_2_for_analysis, quali.sup = 1:7)
