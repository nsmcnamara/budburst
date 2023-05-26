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
str(budburst)
glimpse(budburst)

## Checking NAs
budburst %>%
  summarise(across(everything(), ~ sum(is.na(.))))


  
  
### Transforming the data
### Budburst

