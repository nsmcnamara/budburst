### Exploratory Data Analysis for Stage 2 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-02
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)
library(GGally)
library(RColorBrewer)

#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)

glimpse(stage_2_for_analysis)
head(stage_2_for_analysis)
summary(stage_2_for_analysis)
str(stage_2_for_analysis)

### check NAs
sapply(stage_2_for_analysis, function(x) sum(is.na(x)))


#### First Data Viz ####
#### Distributions of Variable ####

### DOY by Species
doy_stage_2_by_species <- ggplot(stage_2_for_analysis, 
      mapping = aes(doy_stage_2, ..density.., 
                    fill = species)) +
  geom_histogram(bins = 14) +
  facet_wrap(~species, ncol = 1) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(title = "DOY Stage 2, split by Species",
       x = "DOY Stage 2",
       y = "Frequency",
       fill = "Species") +
  scale_alpha(guide = "none")

doy_stage_2_by_species
### +/- 120 days for stage 2

ggsave(filename = "doy_stage_2_by_species.png", device = png, plot = doy_stage_2_by_species, path = "output/figs")


### GDD above 5 by Species
gdd_above_5_by_species <- ggplot(stage_2_for_analysis,
       mapping = aes(gdd_above_5, ..density.., 
                     fill = species)) +
  geom_histogram(bins = 14) +
  facet_wrap( ~species, ncol = 1) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(title = "GDD above 5, split by Species",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Species") +
  scale_alpha(guide = "none")

gdd_above_5_by_species
### pubescens first, petraea second, robur third, all around 200

ggsave(filename = "gdd_above_5_by_species.png", device = png, plot = gdd_above_5_by_species, path = "output/figs")


### GDD above 5 for Q.petraea by site
# all sites for all species individually
# ggplot(data = stage_2_for_analysis,
#       mapping = aes(x = gdd_above_5, y = ..density..,
#                     fill = species, alpha = 0.5)) +
#  geom_histogram(bins = 14) +
#  facet_wrap(~site_name)

# all sites for petraea 
gdd_above_5_petraea_by_site <- stage_2_for_analysis %>% 
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                    fill = site_name, alpha = 0.5)) +
  geom_histogram(bins = 14) +
  facet_wrap(~site_name, ncol = 1)

gdd_above_5_petraea_by_site
ggsave(filename = "gdd_above_5_petraea_by_site.png", device = png, plot = gdd_above_5_petraea_by_site, path = "output/figs")


# all sites for petraea, by age cohort
stage_2_for_analysis %>% 
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = as.factor(age), alpha = 0.5)) +
  geom_histogram(bins = 14) +
  facet_wrap(~site_name, ncol = 1)


### QQ Plot
qqnorm(stage_2_for_analysis$cum_temp_above_5)
qqline(stage_2_for_analysis$cum_temp_above_5)

### Automatic EDA by ggally
stage_2_for_analysis %>%
  select(cum_temp_above_5, species, altitude, latitude, longitude, age) %>%
  ggpairs(mapping = aes(color = species, alpha = 0.5))


### Growing Degree Days by Species ####
## make a raincloudplot of gdd by species
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
  ggdist::stat_dots(
    position = "dodge",
    scale = 0.5,
    side = "left",
    dotsize = 1,
    justification = 1.2,
    show.legend = FALSE
  ) +
  coord_cartesian(xlim = c(1.2, NA)) +
  scale_y_continuous(breaks = seq(100, 400, 50)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.9)) +
  labs(x = "Species",
       y = "Growing Degree Days",
       colour = "Species") +
  scale_alpha(guide = "none") +
  labs(fill = "Species", colour = "Species")


### Growing Degree Days for Q. Robur by Site
## make a plot of doy by site_name
q_robur <- filter(stage_2_for_analysis, species == "Q.robur")

ggplot(q_robur, aes(x = reorder(site_name, site_wet), y = cum_temp_above_5, 
                                 colour = site_name, fill = site_name, alpha = 0.5)) +
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
  ggdist::stat_dots(
    position = "dodge",
    scale = 0.5,
    side = "left",
    dotsize = 1,
    justification = 1.2,
    show.legend = FALSE
  ) +

  coord_cartesian(xlim = c(1.2, NA)) +
#  scale_y_continuous(breaks = seq(100, 150, 10)) +
  theme_bw() +
  labs(x = "Site name",
       y = "Growing Degree Days") +
  scale_alpha(guide = "none") +
  labs(fill = "Site", colour = "Site") +
  facet_wrap( ~ age)


acorns_only <- subset(stage_2_for_analysis, stage_2_for_analysis$age == 2)


ggplot(q_robur, aes(x = reorder(site_name, altitude), y = cum_temp_above_5, 
                                 colour = site_name, fill = site_name, alpha = 0.5)) +
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
  ggdist::stat_dots(
    position = "dodge",
    scale = 0.5,
    side = "left",
    dotsize = 1,
    justification = 1.2,
    show.legend = FALSE
  ) +
  coord_cartesian(xlim = c(1.2, NA)) +
  scale_y_continuous(breaks = seq(100, 400, 50)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.9)) +
  theme(axis.text = element_text(angle = 90)) +
  labs(x = "Species",
       y = "Growing Degree Days",
       colour = "Species") +
  scale_alpha(guide = "none") +
  labs(fill = "Species", colour = "Species") +
  facet_wrap( ~ age)
