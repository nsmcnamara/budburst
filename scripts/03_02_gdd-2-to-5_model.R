### Model for Time to 5 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-08
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(DHARMa)

# Palette
my_pal <- c(
  "#AF2020",
  "#A57C29",
  "#638323",
  "#0C420C",
  "#1E7B5C",
  "#1B3643",
  "#71C0E5",
  "#713478",
  "#DE439C",
  "#FF6767")



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
# Both cohorts
model_robur_gdd_2_to_5 <- lmer(gdd_2_to_5 ~ latitude + (1|cohort) + (1|mother_id), 
                         data = df_robur_only)
summary(model_robur_gdd_2_to_5)
anova(model_robur_gdd_2_to_5, type = "3")
# latitude significant, dropped wet, longitude, altitude as not significant

testDispersion(model_robur_gdd_2_to_5)




# 
# # check collinearity
# # car::vif(model_robur_gdd_2_to_5)
# # altitude and latitude problematic
# 
# #df_robur_alt_lat <- df_robur_only %>%
# #  select(altitude, latitude)
# 
# # run PCA
# pca = prcomp(df_robur_alt_lat, scale = TRUE)
# pc_robur <- pca$x
# 
# # latitude and altitude reduced to components pc1 and pc2. 
# # check proortion of variance explained by each
# # divide variance explained by each by variance explained by all
# pca$sdev^2 / sum(pca$sdev^2)
# # first component explains 855%, second component 15%
# 
# df_robur_pc <- cbind(df_robur_only, pc_robur)
# ggplot(df_robur_pc,
#        mapping = aes(x = PC1, y = PC2, color = site_name)) +
#   geom_point(size = 3)
# 
# #### LMM 2 ####
# model_robur_pc <- lmer(gdd_2_to_5 ~ PC1 + PC2 + longitude + site_wet + cohort + (1|mother_id), 
#                                data = df_robur_pc)
# summary(model_robur_pc)
# anova(model_robur_pc, type = "3")
# 
# 
# 
# #### Robur cohort 2 only ####
# model_robur_gdd_2_to_5_cohort202302 <- lmer(gdd_2_to_5 ~ altitude + latitude + longitude + site_wet + (1|mother_id), 
#                                data = df_robur_only %>%
#                                  filter(cohort == "2023_2"))
# summary(model_robur_gdd_2_to_5_cohort202302)
# anova(model_robur_gdd_2_to_5_cohort202302, type = "3")
# # both altitude and latitude show some significance 
# 
# #### Robur cohort 3 only ####
# model_robur_gdd_2_to_5_cohort202303 <- lmer(gdd_2_to_5 ~ altitude + latitude + longitude + site_wet + (1|mother_id), 
#                                             data = df_robur_only %>%
#                                               filter(cohort == "2023_3"))
# summary(model_robur_gdd_2_to_5_cohort202303)
# anova(model_robur_gdd_2_to_5_cohort202303, type = "3")
# # nothing shows any significance, but rank deficiency
# cor_matrix <- df_robur_only %>%
#   filter(cohort == "2023_03") %>%
#   select_if(is.numeric) %>%
#   as.matrix() %>%
#   cor()
# 
