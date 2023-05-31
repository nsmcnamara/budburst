### Exploratory Data Analysis for Stage 2 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-05-26
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)
library(GGally)

#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)

glimpse(stage_2_for_analysis)
head(stage_2_for_analysis)
summary(stage_2_for_analysis)
str(stage_2_for_analysis)

### check NAs
sapply(stage_2_for_analysis, function(x) sum(is.na(x)))


#### First Data Viz ####

### Histogram by species
ggplot(stage_2_for_analysis,
       mapping = aes(cum_temp_above_5, ..density.., fill = species, alpha = 0.5)) +
  geom_histogram(bins = 12) +
#  geom_density() +
  facet_wrap( ~species, ncol = 1)

### QQ Plot
qqnorm(stage_2_for_analysis$cum_temp_above_5)
qqline(stage_2_for_analysis$cum_temp_above_5)

### Automatic EDA by ggally
stage_2_for_analysis %>%
  select(cum_temp_above_5, species, altitude, latitude, longitude, age) %>%
  ggpairs(mapping = aes(color = species, alpha = 0.5))

## make a raincloudplot of doy by species
ggplot(stage_2_for_analysis, aes(x = species, y = cum_temp_above_5, 
                                 colour = species, fill = species, alpha = 0.5)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    justification = -.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = .12,
    show.legend = FALSE
  ) +

#  ggdist::stat_dots(
#    side = "left",
#    justification = 1.1,
#    position = position_dodge(),
#    binwidth = 7,
#    dotsize = 0.5,
#    scale = 2/10,
#    show.legend = FALSE
#  ) +

  coord_cartesian(xlim = c(1.2, NA)) +
#  scale_y_continuous(breaks = seq(100, 150, 10)) +
  theme_bw() +
  labs(x = "Species",
       y = "DOY of Stage 2",
       colour = "Species") +
  scale_alpha(guide = "none") +
  labs(fill = "Species", colour = "Species")





## make a plot of doy by site_name

