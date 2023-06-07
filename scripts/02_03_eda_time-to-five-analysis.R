### Exploratory Data Analysis for Time to 5 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-07
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(GGally)
library(RColorBrewer)

# Palette
Set2palette <- brewer.pal(8, "Set2")
Dark2palette <- brewer.pal(8, "Dark2")


#### Data Import ####
gdd_2_to_5 <- read.csv("~/budburst/data/processed/gdd_2_to_5_for_analysis.csv", stringsAsFactors=TRUE)

## Check out data
glimpse(gdd_2_to_5)
head(gdd_2_to_5)
summary(gdd_2_to_5)
str(gdd_2_to_5)

## check NAs
sapply(gdd_2_to_5, function(x) sum(is.na(x)))

### DF w/o NA (no weather data for 2022, ergo no gdd)
gdd_2_to_5_clean <- gdd_2_to_5 %>%
  drop_na(gdd_2_to_5)

# calculate means and counts  
means <- gdd_2_to_5_clean %>%
  group_by(species) %>%
  summarize(m = mean(gdd_2_to_5))
### very close, no diff by species

counts <- gdd_2_to_5_clean %>%
  group_by(species) %>%
  summarise(n = n())

sum_stats <- gdd_2_to_5_clean %>%
  group_by(species, site_name, age, latitude, longitude, altitude) %>%
  summarize(mean = mean(gdd_2_to_5))

#### GDD 2 to 5 Q. petraea ####
gdd_2_to_5_clean %>%
  filter(species == "Q.petraea") %>%
  filter(age == "2") %>%
  ggplot(aes(x = reorder(site_name, altitude), 
            y = gdd_2_to_5, 
            colour = site_name, fill = site_name)) +
  ggdist::stat_halfeye(
    breaks = 14,
    adjust = 0.5,
    width = 0.6,
    justification = -.2,
    .width = 0,
    point_colour = NA,
    alpha = 0.6,
    show.legend = FALSE
  ) +
  geom_boxplot(
    width = .12,
    alpha = 0.6,
    show.legend = FALSE
  ) +
  ggdist::stat_dots(
    position = "dodge",
    scale = 0.4,
    side = "left",
    dotsize = 1,
    justification = 1.2,
    alpha = 0.6,
    show.legend = FALSE
  ) +
  coord_flip(xlim = c(1, NA), ylim = c(20, 100), expand = TRUE, clip = "on") +
#  scale_x_discrete(breaks = seq(100, 400, 50)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.2)) +
  labs(x = "Site",
       y = "GDD 2 to 5",
       colour = "Site") +
  scale_alpha(guide = "none") +
  labs(fill = "Site", colour = "Site") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")


gdd_2_to_5_clean %>%
  select(gdd_2_to_5, species, altitude, latitude, longitude, cohort, site_wet) %>%
  ggpairs(mapping = aes(color = species, alpha = 0.5))
## altitude, latitude, longitude, correlated but also to gdd_2_to_5