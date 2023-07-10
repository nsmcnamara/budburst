### Model for chlorophyll (Apogee)
### This script is part of the ACORN budburst analysis project
### Last update:  2023-07-10
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)
library(plotly)

# resolve conflicts
library(conflicted)


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
  filter(year == "2023")
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

ggplot(data = chlorophyll, mapping = aes(x = species, y = reading, col = site_name)) +
  geom_jitter()

c_rob <- chlorophyll %>%
  filter(species == "Q.robur") %>%
  filter(age == "2") %>%
  ggplot(mapping = aes(x = fcf, y = reading, col = site_name)) +
  geom_jitter(alpha = 0.2) +
  geom_boxplot()

ggplotly(c_rob)

#### LMM ####
#### ROBUR ####
robur <- chlorophyll_scaled %>%
  filter(species == "Q.robur")

# with Bosco Pantano
m_chl_rob_0 <- robur %>%
  lmer(chl_index ~ 0 + (1 | cohort) + (1 | mother_id), data = .)
summary(m_chl_rob_0)

# plot
robur %>%
  ggplot(mapping = aes(x = cohort, y = chl_index)) +
  geom_point() +
  geom_smooth(method = lm)

