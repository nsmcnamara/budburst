### Model for Time to 5 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-07
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(lme4)
library(lmerTest)

# Palette
my_pal <- c("#B14343", "#936E23","#6E8938","#0C420C","#24785C","#1D3643", "#436379", "#4E376C", "#B9679E","#ED6F6F")


#### Data Import ####
df_gdd_2_to_5 <- read.csv("~/budburst/data/processed/gdd_2_to_5_for_analysis.csv", stringsAsFactors=TRUE)

## Check out data
glimpse(df_gdd_2_to_5)
head(df_gdd_2_to_5)
summary(df_gdd_2_to_5)
str(df_gdd_2_to_5)

## check NAs
sapply(df_gdd_2_to_5, function(x) sum(is.na(x)))

### DF w/o NA (no weather data for 2022, ergo no gdd)
df_gdd_2_to_5_clean <- df_gdd_2_to_5 %>%
  drop_na(gdd_2_to_5)
# sanity check: 756 

# split by species
df_robur_only <- df_gdd_2_to_5_clean %>%
  filter(species == "Q.robur")

#### LMM ####
#### ROBUR ONLY ####
model_robur_gdd_2_to_5 <- lmer(gdd_2_to_5 ~ altitude + latitude + longitude + site_wet + cohort + (1|mother_id), 
                         data = df_robur_only)
summary(model_robur_gdd_2_to_5)
anova(model_robur_gdd_2_to_5, type = "3")

# check collinearity
car::vif(model_robur_gdd_2_to_5)
# altitude and latitude problematic

df_robur_alt_lat <- df_robur_only %>%
  select(altitude, latitude)

# run PCA
pca = prcomp(df_robur_alt_lat, scale = TRUE)
pc_robur <- pca$x

# latitude and altitude reduced to components pc1 and pc2. 
# check proortion of variance explained by each
# divide variance explained by each by variance explained by all
pca$sdev^2 / sum(pca$sdev^2)
# first component explains 855%, second component 15%

df_robur_pc <- cbind(df_robur_only, pc_robur)
ggplot(df_robur_pc,
       mapping = aes(x = PC1, y = PC2, color = site_name)) +
  geom_point(size = 3)

#### LMM 2 ####
model_robur_pc <- lmer(gdd_2_to_5 ~ PC1 + PC2 + longitude + site_wet + cohort + (1|mother_id), 
                               data = df_robur_pc)
summary(model_robur_pc)
anova(model_robur_pc, type = "3")

