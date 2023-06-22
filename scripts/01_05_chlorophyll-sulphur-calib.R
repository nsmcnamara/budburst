### Checking whether sulphur spots affect chlorophyll measurements
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-12
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)

#### Data Import ####
df_chlor_calib <- read.csv("~/budburst/data/processed/chlorophyll_sulphur_calib.csv", stringsAsFactors=TRUE)
df_clean <- df_chlor_calib %>%
  filter(status == "clean") %>%
  drop_na()
df_dirty <- df_chlor_calib %>%
  filter(status == "dirty") %>%
  drop_na()

one.way <- aov(Reading ~ status, data = df_chlor_calib)
summary(one.way)

var.test(df_clean$Reading, df_dirty$Reading, alternative = "two.sided")

mean(df_clean$Reading)
mean(df_chlor_calib$Reading[df_chlor_calib$status == "clean"], na.rm = TRUE)
var(df_clean$Reading)

mean(df_dirty$Reading)
var(df_dirty$Reading)

two.way <- aov(Reading ~ status + species, data = df_chlor_calib)
summary(two.way)
