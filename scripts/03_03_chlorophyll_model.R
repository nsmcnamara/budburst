### Model for chlorophyll (Apogee)
### This script is part of the ACORN budburst analysis project
### Last update:  2023-07-10
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)
library(plotly)
library(lme4)
library(lmerTest)

# resolve conflicts
library(conflicted)
conflict_scout()
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(lmerTest::lmer)
conflicts_prefer(lmerTest::step)


#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)
chelsa <- read.csv("~/budburst/data/processed/coordinates_chelsa_values.csv", stringsAsFactors = TRUE)
sumstat_gdd_120 <- read.csv("~/budburst/data/processed/sumstat_gdd-120-1980-2019.csv", stringsAsFactors = TRUE)
chlorophyll_zh <- read.csv("~/budburst/data/processed/chlorophyll_zurich-2023.csv", stringsAsFactors = TRUE)

str(chlorophyll_zh)

# check NAs
sapply(chlorophyll_zh, function(x) sum(is.na(x)))

#### Data Wrangling ####
# use only 2023 data
stage_2_2023 <- stage_2_for_analysis %>%
  filter(year == "2023") %>%
  mutate(age = as.factor(age))
# inner join drops where no complete information available 
chlorophyll <- inner_join(chlorophyll_zh, stage_2_2023, by = "acorn_id")

## add chelsa to DF
# drop unnecessary columns
chelsa <- chelsa %>%
  select(- c(Long, Lat, optional))

# rename
colnames(chelsa) <- gsub("CHELSA_|_1981.2010_V.2.1", "", colnames(chelsa))
chelsa <- chelsa %>%
  rename(temp_ann_mean = bio1) %>%
  rename(precip_ann = bio12) %>%
  rename(precip_seasonality = bio15) %>%
  mutate(kg2 = as.factor(kg2))

# join chelsa
chlorophyll <- left_join(chlorophyll, chelsa, by = "mother_id")

# add gdd sumstats to DF
# rename
sumstat_gdd_120 <- sumstat_gdd_120 %>%
  rename(mean_gdd_120 = mean) %>%
  rename(var_gdd_120 = var)

# join
chlorophyll <- left_join(chlorophyll, sumstat_gdd_120, by = "site_name")


#### Plot variables ####
numeric_columns <- chlorophyll[ , sapply(chlorophyll, is.numeric)]
par(mar = c(1, 1, 1, 1))
par(mfrow = c(4, 6))  # Set up multiple plots in one row
lapply(colnames(numeric_columns), function(col_name) {
  hist(numeric_columns[[col_name]], main = col_name, xlab = "Value", ylab = "Frequency")
})

# transformations
hist(chlorophyll$reading)
hist(sqrt(chlorophyll$altitude)) # alt: sqrt (right-skewed)
hist(chlorophyll$latitude) # i think keep
hist(chlorophyll$longitude) # i think keep
hist(log(chlorophyll$gdd_above_5)) # gdd_above_5: log 
hist(log(chlorophyll$temp_ann_mean)) # temp_ann_mean: log
hist(log(chlorophyll$precip_ann)) # precip: log
hist(chlorophyll$fcf) # i think keep
hist(log(chlorophyll$gdd5)) # gdd5: log
hist(chlorophyll$gsl) # keep
hist(chlorophyll$gsp) # keep
hist(log(chlorophyll$mean_gdd_120)) # log
hist(log(chlorophyll$var_gdd_120)) # log

chlorophyll <- chlorophyll %>%
  rename(chl_index = reading) %>%
  mutate(alt_sqrt = sqrt(altitude)) %>%
  mutate(gdd_above_5_log = log(gdd_above_5)) %>%
  mutate(temp_ann_mean_log = log(temp_ann_mean)) %>%
  mutate(precip_ann_log = log(precip_ann)) %>%
  mutate(gdd5_log = log(gdd5)) %>%
  mutate(mean_gdd_120_log = log(mean_gdd_120)) %>%
  mutate(var_gdd_120_log = log(var_gdd_120))

chlorophyll_scaled <- chlorophyll %>%
  mutate_if(is.numeric, scale)

#### First Data Viz ####
dev.off()

ggplot(data = chlorophyll, mapping = aes(x = species, y = chl_index, col = site_name)) +
  geom_jitter()

c_rob <- chlorophyll %>%
  filter(species == "Q.robur") %>%
  filter(age == "2") %>%
  ggplot(mapping = aes(x = fcf, y = chl_index, col = site_name)) +
  geom_jitter(alpha = 0.2) +
  geom_boxplot()

ggplotly(c_rob)

#### LMM ####
#### ROBUR ####
# something about Cestas and Laveyron: lighter 
robur <- chlorophyll_scaled %>%
  filter(species == "Q.robur")

# plot
robur %>%
  filter(cohort == "2023_2") %>%
  ggplot(mapping = aes(x = mother_id, y = chl_index, fill = site_name)) +
  geom_point(aes(col = site_name, alpha = 0.8)) +
  geom_boxplot(aes(alpha = 0.7)) + 
  facet_wrap(vars(site_name), scales = "free_x")

chlorophyll %>%
  filter(species == "Q.robur") %>%
  filter(cohort == "2023_2") %>%
  ggplot(mapping = aes(x = mother_id, y = chl_index, fill = site_name)) +
  geom_point(aes(col = site_name, alpha = 0.8)) +
  geom_boxplot(aes(alpha = 0.7)) + 
  facet_wrap(vars(site_name), scales = "free_x")

chlorophyll %>%
  filter(species == "Q.robur") %>%
  filter(cohort == "2023_2") %>%
  ggplot(mapping = aes(x = site_name, y = chl_index)) +
  geom_boxplot(aes(fill = site_name, alpha = 0.7))

# 0 model
m_chl_rob_0 <- robur %>%
  lmer(chl_index ~ 0 + (1 | cohort) + (1 | mother_id) + (1 | date), data = .)
summary(m_chl_rob_0)


# gdd 120 model 
m_chl_rob_mean_gdd <- robur %>%
  filter(!site_name == "Bosco_Pantano") %>%
  lmer(chl_index ~ mean_gdd_120_log + (1 | cohort) + (1 | mother_id) + (1 | date), data = .)
summary(m_chl_rob_mean_gdd)

chlorophyll %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = mean_gdd_120, y = chl_index)) +
  geom_jitter(aes(col = site_name, alpha = 0.2)) +
  geom_boxplot(aes(group = site_name, fill = site_name)) +
  geom_smooth(method = "loess")

# lat model 
m_chl_rob_lat <- robur %>%
  lmer(chl_index ~ latitude + (1 | cohort) + (1 | mother_id) + (1 | date), data = .)
summary(m_chl_rob_lat)



chlorophyll %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = latitude, y = chl_index)) +
  geom_jitter(aes(col = site_name, alpha = 0.2)) +
  geom_boxplot(aes(group = site_name, fill = site_name)) +
  geom_smooth(method = "loess")

# altitude model 
m_chl_rob_alt <- robur %>%
  lmer(chl_index ~ alt_sqrt + (1 | cohort) + (1 | mother_id) + (1 | date), data = .)
summary(m_chl_rob_alt)

chlorophyll %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = altitude, y = chl_index)) +
  geom_jitter(aes(col = site_name, alpha = 0.2)) +
  geom_boxplot(aes(group = site_name, fill = site_name)) +
  geom_smooth(method = "loess")

# temp ann mean
chlorophyll %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = temp_ann_mean, y = chl_index)) +
  geom_jitter(aes(col = site_name, alpha = 0.2)) +
  geom_boxplot(aes(group = site_name, fill = site_name)) +
  geom_smooth(method = "lm")

# precip ann 
chlorophyll %>%
  filter(species == "Q.robur") %>%
  filter(!site_name == "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = precip_ann, y = chl_index)) +
  geom_jitter(aes(col = site_name, alpha = 0.2)) +
  geom_boxplot(aes(group = site_name, fill = site_name)) +
  geom_smooth(method = "loess")

m_chl_rob_kg2 <- robur %>%
  lmer(chl_index ~ kg2 + (1 | cohort) + (1 | mother_id) + (1 | date), data = .)
summary(m_chl_rob_kg2)
# something about laveyron and cestas but not guca


chlorophyll %>%
  ggplot(mapping = aes(x = species, y = chl_index)) +
  geom_boxplot(aes(fill = species, alpha = 0.7))

chlorophyll %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = site_name, y = chl_index)) +
  geom_jitter(aes(col = site_name, alpha = 0.2)) +
  geom_boxplot(aes(group = site_name, fill = site_name)) +
  geom_smooth(method = "loess")

m_chl_rob_many <- robur %>%
  lmer(chl_index ~   mean_gdd_120_log + (1 | cohort) + (1 | mother_id) + (1 | date), data = ., REML=F)
summary(m_chl_rob_many)
vif(m_chl_rob_many)
