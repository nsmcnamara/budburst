### Model for budburst
### This script is part of the ACORN budburst analysis project
### Last update:  2023-07-07
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(MASS)
library(AICcmodavg)
library(DHARMa)
library(car)

# resolve conflicts
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(lmerTest::lmer)



#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)
chelsa <- read.csv("~/budburst/data/processed/coordinates_chelsa_values.csv", stringsAsFactors = TRUE)
sumstat_gdd_120 <- read.csv("~/budburst/data/processed/sumstat_gdd-120-1980-2019.csv", stringsAsFactors = TRUE)


#glimpse(stage_2_for_analysis)
#head(stage_2_for_analysis)
#summary(stage_2_for_analysis)
#str(stage_2_for_analysis)

### check NAs
sapply(stage_2_for_analysis, function(x) sum(is.na(x)))

## add chelsa to DF
# drop unnecessary columns
chelsa <- chelsa %>%
  select(- c(Long, Lat, optional))

# rename
colnames(chelsa) <- gsub("CHELSA_|_1981.2010_V.2.1", "", colnames(chelsa))
chelsa <- chelsa %>%
  rename(temp_ann_mean = bio1) %>%
  rename(precip_ann = bio12) %>%
  rename(precip_seasonality = bio15)


df_s2 <- left_join(stage_2_for_analysis, chelsa, by = "mother_id")

### add gdd at prov
df_s2 <- left_join(df_s2, sumstat_gdd_120, by = "site_name")

## don't use 2022: were in fridge and not reliable data in any way 
df_s2 <- df_s2 %>%
  filter(!year == "2022")



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
  # cohort

# resp ~ FEexpr + (1 | factor 1)
# global intercept. deviation from intercept for factor 1. global slope for FE

# resp ~ FEexpr + (1 | factor 1) + (0 + FEexpr | factor 1)
# as above, plus effect of FEexpr within each level of factor 1, 
# while enforcing a zero correlation between the intercept deviations and effect deviations across levels of V2. 

# resp ~ FEexpr + (1 + FEexpr | factor 1)
# allowing correlation between intercept and effect deviation


#### LMM Robur ####
## 07.07.
## Relationship with provenance temp and precip_seasonality is driven by Bosco Pantano

# minimal model: temperature at provenance
# with Bosco Pantano
m_gdd_s2_rob_gddprov <- df_s2 %>%
  filter(species == "Q.robur") %>%
  lmer(gdd_above_5 ~ mean + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov)

# plot
df_s2 %>%
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = mean, y = gdd_above_5)) +
  geom_point() +
  geom_smooth(method = lm)


### check model assumptions

## Tukey Anscombe
par(mfrow = c(1,1))
plot(m_gdd_s2_rob_gddprov)

## QQ plot
qqnorm(resid(m_gdd_s2_rob_gddprov))
qqline(resid(m_gdd_s2_rob_gddprov))

## scale-location plot
plot(m_gdd_s2_rob_gddprov,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)

## leverage plot
plot(m_gdd_s2_rob_gddprov, rstudent(.) ~ hatvalues(.))


# without Bosco Pantano
m_gdd_s2_rob_gddprov_b <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ mean + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov_b)

# plot
df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = mean, y = gdd_above_5)) +
  geom_point() +
  geom_smooth(method = lm)

## Tukey Anscombe
par(mfrow = c(1,1))
plot(m_gdd_s2_rob_gddprov)

## QQ plot
qqnorm(resid(m_gdd_s2_rob_gddprov))
qqline(resid(m_gdd_s2_rob_gddprov))

## scale-location plot
plot(m_gdd_s2_rob_gddprov,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)

## leverage plot
plot(m_gdd_s2_rob_gddprov, rstudent(.) ~ hatvalues(.))

# relationship is inverted but not significant.
# significant only if Bosco Pantano is in


# temperature and latitude
# neither significant, both with or without Bosco
# with Bosco Pantano
m_gdd_s2_rob_gddprov_2 <- df_s2 %>%
  filter(species == "Q.robur") %>%
  lmer(gdd_above_5 ~ mean + latitude + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov_2)

# without Bosco Pantano
m_gdd_s2_rob_gddprov_2_b <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ mean + latitude + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov_2_b)


# temp and precipitation
# again driven by Bosco Pantano
# with Bosco Pantano
m_gdd_s2_rob_gddprov_3 <- df_s2 %>%
  filter(species == "Q.robur") %>%
  lmer(gdd_above_5 ~ mean + precip_seasonality + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov_3)

# plot
df_s2 %>%
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = precip_seasonality, y = gdd_above_5)) +
  geom_point() +
  geom_smooth(method = lm)


# without Bosco Pantano
m_gdd_s2_rob_gddprov_3b <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ mean + precip_seasonality + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov_3b)

# plot
df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = mean, y = gdd_above_5)) +
  geom_point() +
  geom_smooth(method = lm)

# relationship is inverted but not significant.
# significant only if Bosco Pantano is in


# precipitation only
# not significant
m_gdd_s2_rob_gddprov_4 <- df_s2 %>%
  filter(species == "Q.robur") %>%
  lmer(gdd_above_5 ~ precip_seasonality + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov_4)

# plot
df_s2 %>%
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = precip_seasonality, y = gdd_above_5)) +
  geom_point() +
  geom_smooth(method = lm)


# without Bosco Pantano
m_gdd_s2_rob_gddprov_4b <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ precip_seasonality + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov_4b)

# plot
df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = precip_seasonality, y = gdd_above_5)) +
  geom_point() +
  geom_smooth(method = lm)



# full model with precip, temp and lat
# again, with Bosco some significant terms, without none

m_gdd_s2_rob_gddprov_5 <- df_s2 %>%
  filter(species == "Q.robur") %>%
  lmer(gdd_above_5 ~ mean + precip_seasonality + latitude + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov_5)

# without Bosco Pantano
m_gdd_s2_rob_gddprov_5b <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  lmer(gdd_above_5 ~ mean + precip_seasonality + latitude + (1 | cohort) + (1 | mother_id), data = .)
summary(m_gdd_s2_rob_gddprov_5b)














### Q petraea ###
m_gdd_pet_many <- df_s2 %>%
  filter(species == "Q.petraea") %>%
  filter(country != "Turkey") %>%
  lmer(gdd_above_5 ~ precip_ann + 
         (1 | mother_id) + (1 | age) + (1 | year), data = .)
# problem: precip_ann is same for staufen dry/wet and for schriesheim dry/wet

### Q pubescens ###
m_gdd_pub_many <- df_s2 %>%
  filter(species == "Q.pubescens") %>%
  drop_na() %>%
  lmer(gdd_above_5_log ~ latitude +
         (1 | mother_id) + (1 | age) + (1 | year), data = .)
pub <- df_s2 %>%
  filter(species == "Q.pubescens") %>%
  drop_na()

summary(m_gdd_pub_many)

performance::check_model(m_gdd_pub_many, check = "qq", show)






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


###### 19.6.2023 ####
# AIC etc: best model with the smallest value of information criterion
# backward selection (start w full)
# forward selection (start w empty)
df_robur <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano")

# drop columns with na values
df_robur <- df_robur %>% 
  select_if(~ !any(is.na(.)))

which(is.na(df_robur), arr.ind=TRUE)

df_robur <- df_robur %>%
  dplyr::select(!c(species))
                  
m_robur_full <- lmer(gdd_above_5_log ~ precip_ann_log * temp_ann_mean_log * gdd5_log * alt_sqrt * ngd5 * kg2 *
           gst * gsp * gsl * gdgfgd10 * gddlgd10 * gdd10 * precip_seasonality * latitude +
           (1 | mother_id ) + (1 | year) + (1 | age), data = df_robur)

r.AIC <- stepAIC(m_robur_full, direction = c("backward"), trace = FALSE, AICc = TRUE)


#######
robur_2023_2 <- df_s2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  filter(cohort == "2023_2") %>%
  replace(is.na(.), 0) %>%
  mutate(spring_tempo = gdgfgd10 - gdgfgd5)



m_robur_2023_2 <- lmer(gdd_above_5 ~ spring_tempo + 
                         (1 | site_name / mother_id),
                       data = robur_2023_2)
summary(m_robur_2023_2)

resid <- simulateResiduals(m_robur_2023_2)
plot(resid)
