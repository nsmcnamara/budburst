### Model for chlorophyll (Apogee)
### This script is part of the ACORN budburst analysis project
### Last update:  2023-07-10
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)

# resolve conflicts
library(conflicted)


#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)
chelsa <- read.csv("~/budburst/data/processed/coordinates_chelsa_values.csv", stringsAsFactors = TRUE)
sumstat_gdd_120 <- read.csv("~/budburst/data/processed/sumstat_gdd-120-1980-2019.csv", stringsAsFactors = TRUE)
chlorophyll_zh <- read.csv("~/budburst/data/processed/chlorophyll_zurich-2023.csv", stringsAsFactors = TRUE)

str(chlorophyll_zh)
