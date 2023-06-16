### Model for budburst
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-16
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)


#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)
chelsa <- read.csv("~/budburst/data/processed/coordinates_chelsa_values.csv", stringsAsFactors = TRUE)

glimpse(stage_2_for_analysis)
head(stage_2_for_analysis)
summary(stage_2_for_analysis)
str(stage_2_for_analysis)

### check NAs
sapply(stage_2_for_analysis, function(x) sum(is.na(x)))

## add chelsa to DF
chelsa <- chelsa %>%
  dplyr::select(- c(Long, Lat, optional))

# rename
colnames(chelsa) <- gsub("CHELSA_|_1981.2010_V.2.1", "", colnames(chelsa))
chelsa <- chelsa %>%
  rename(temp_ann_mean = bio1) %>%
  rename(precip_ann = bio12) %>%
  rename(precip_seasonality = bio15)


df_s2 <- left_join(stage_2_for_analysis, chelsa, by = "mother_id")


### plot variables and transform if necessary
# as factor
df_s2 <- df_s2 %>%
  mutate(age = as.factor(age)) %>%
  mutate(year = as.factor(year))

# plot all numeric variables
numeric_columns <- df_s2[ , sapply(df_s2, is.numeric)]
par(mfrow = c(4, 6))  # Set up multiple plots in one row
lapply(colnames(numeric_columns), function(col_name) {
  hist(numeric_columns[[col_name]], main = col_name, xlab = "Value", ylab = "Frequency")
})

# transformations
hist(sqrt(df_s2$altitude)) # alt: sqrt (right-skewed)
hist(df_s2$latitude) # i think keep
hist(df_s2$longitude) # i think keep
hist(log(df_s2$gdd_above_5)) # gdd_above_5: log 
e1071::skewness(df_s2$temp_ann_mean)
hist(log(df_s2$temp_ann_mean)) # temp_ann_mean: log
hist(log(df_s2$precip_ann)) # precip: log
hist(df_s2$fcf) # i think keep
hist(log(df_s2$gdd5)) # gdd5: log
hist(df_s2$gsl) # keep
hist(df_s2$gsp) # keep

df_s2 <- df_s2 %>%
  mutate(alt_sqrt = sqrt(altitude)) %>%
  mutate(gdd_above_5_log = log(gdd_above_5)) %>%
  mutate(temp_ann_mean_log = log(temp_ann_mean)) %>%
  mutate(precip_ann_log = log(precip_ann)) %>%
  mutate(gdd5_log = log(gdd5))
    





#### LMM Stage 2 ####
# resp ~ FEexpr + (REexpr1 | factor1) + (REexpr2 | factor2) + ...
# resp = gdd_above_5
# fixed effects: 
  # photoperiod (latitude)
  # temperature at provenance 
  # precipitation at provenance
# random effects:
  # mother_id 

# resp ~ FEexpr + (1 | factor 1)
# global intercept. deviation from intercept for factor 1. global slope for FE

# resp ~ FEexpr + (1 | factor 1) + (0 + FEexpr | factor 1)
# as above, plus effect of FEexpr within each level of factor 1, 
# while enforcing a zero correlation between the intercept deviations and effect deviations across levels of V2. 

# resp ~ FEexpr + (1 + FEexpr | factor 1)
# allowing correlation between intercept and effect deviation


#### LMM Robur ####

### latitude only
m_gdd_s2_rob_lat <- stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  lmer(gdd_above_5 ~ latitude + (1 | mother_id), data = .)

# summary / anova
summary(m_gdd_s2_rob_lat)
# latitude not significant
# but mother_id explains a fair bit, but a fair bit of variance unexplained

# w/o Bosco
m_gdd_s2_rob_lat_bosco <- stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ latitude + (1 | mother_id), data = .)

summary(m_gdd_s2_rob_lat_bosco)
# now latitude v. significant
# mother_id effect less 

### altitude
m_gdd_s2_rob_alt <- stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  lmer(gdd_above_5 ~ latitude + (1 | mother_id), data = .)

# summary / anova
summary(m_gdd_s2_rob_alt)
# altitude not significant
# but mother_id explains a fair bit, but a fair bit of variance unexplained

# w/o Bosco
m_gdd_s2_rob_alt_bosco <- stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ altitude + (1 | mother_id), data = .)

summary(m_gdd_s2_rob_alt_bosco)
# now altitude a little significant


### latitude and altitude
### latitude only
m_gdd_s2_rob_lat_alt <- stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  lmer(gdd_above_5 ~ latitude + altitude + (1 | mother_id), data = .)

# summary / anova
summary(m_gdd_s2_rob_lat_alt)
# neither significant

# w/o Bosco
m_gdd_s2_rob_lat_alt_bosco <- stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ latitude + altitude + (1 | mother_id), data = .)

summary(m_gdd_s2_rob_lat_alt_bosco)
# now latitude significant, altitude not

### w interaction between latitude and altitude
m_gdd_s2_rob_lat_alt_bosco <- stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  mutate(latitude_scaled = scale(latitude),
         altitude_scaled = scale(altitude)) %>%
  lmer(gdd_above_5 ~ latitude_scaled * altitude_scaled + (1 | mother_id), data = .)

summary(m_gdd_s2_rob_lat_alt_bosco)
# latitude significant

## LMM robur lat + climate_zone
m_gdd_s2_rob_lat_clim_bosco <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ latitude + climate_zone + altitude + site_wet + (1 | mother_id) + (1 | age) + (1 | year), data =.)

summary(m_gdd_s2_rob_lat_clim_bosco)

## LMM robur lat + climate_zone
m_gdd_s2_rob_aust_alt_wet <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(country == "Austria") %>%
  lmer(gdd_above_5 ~ altitude + site_wet + (1 | mother_id) + (1 | age) + (1 | year), data =.)

summary(m_gdd_s2_rob_aust_alt_wet)


### LMM robur with many fun things
m_gdd_rob_many <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ latitude + altitude + temp_ann_mean + precip_ann +  
         (1 | mother_id) + (1 | age) + (1 | year), data = .)


summary(m_gdd_rob_many)


### check model assumptions

## Tukey Anscombe
par(mfrow = c(1,1))
plot(m_gdd_rob_many)

## QQ plot
qqnorm(resid(m_gdd_rob_many))
qqline(resid(m_gdd_rob_many))

## scale-location plot
plot(m_gdd_rob_many,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)

## leverage plot
plot(m_gdd_rob_many, rstudent(.) ~ hatvalues(.))
performance::check_model(m_gdd_rob_many)

# some kind of tukey test
emm <- emmeans(m_gdd_rob_many, ~ latitude + altitude + temp_ann_mean + precip_ann)
summary(emm)

# not sure how to interpret







### ### ### ### ### ### ###
### ### ### ### ### ### ###
# QQ of residuals
qqnorm(resid(model_growing_degree_days), main = "Residuals")
qqline(resid(model_growing_degree_days), col = "red") # looks ok to me

# QQ of Random effects of Line
qqnorm(ranef(model_growing_degree_days)$mother_id[,1], main = "Random effects of Mother ID")
qqline(ranef(model_growing_degree_days)$mother_id[,1], col = "red")

# QQ of Random effects of Date
qqnorm(ranef(model_growing_degree_days)$age[,1], main = "Random effects of Age")
qqline(ranef(model_growing_degree_days)$age[,1], col = "red")


#### POST HOC TEST ####

######NOTES
# some kind of tukey test
emmeans(model_growing_degree_days)

# split lmer by species
# try with latitude and altitude 

q_robur <- stage_2_for_analysis %>%
  filter(species == "Q.robur")
plot(q_robur$latitude,q_robur$altitude
     )
# correlated
# maybe just start with one or do a PCA of lat and alt
# some kind of site moisture index
# avg yearly precip from debbie 

