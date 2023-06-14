### Exploratory Data Analysis for Time to 5 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-14
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(GGally)
library(ggrepel)

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

my_pal_species <- c( "#1E7B5C",  "#71C0E5",  "#713478")
my_pal_cohorts <- c(   "#638323", "#1B3643","#FF6767")

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

# calculate means and counts  
means <- df_gdd_2_to_5_clean %>%
  group_by(species) %>%
  summarize(m = mean(gdd_2_to_5))
### very close, no diff by species

counts <- df_gdd_2_to_5_clean %>%
  group_by(species) %>%
  summarise(n = n())

sum_stats <- df_gdd_2_to_5_clean %>%
  group_by(species, site_name, age, latitude, longitude, altitude) %>%
  summarize(mean = mean(gdd_2_to_5))

#### PLOT THE DATA ####
#### ALL SPECIES ####
# ALL SPECIES BY AlTITUDE
ggplot(data = df_gdd_2_to_5_clean,
       mapping = aes(x = altitude, y = gdd_2_to_5,
                     color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# slightly longer gdd with increasing altitude, but doubt significance

# ALL SPECIES BY LATITUDE
ggplot(data = df_gdd_2_to_5_clean,
       mapping = aes(x = latitude, y = gdd_2_to_5,
                     color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# shorter gdd with increasing latitude
# ie the further north we go, the less warming is needed

# ALL SPECIES BY LONGITUDE
ggplot(data = df_gdd_2_to_5_clean,
       mapping = aes(x = longitude, y = gdd_2_to_5,
                     color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# increasing gdd with increasing longitude
# ie the further east we go, the more warming required



#### Q. PETRAEA ####
# ALL COHORTS BY LATITUDE
df_gdd_2_to_5_clean %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# shorter gdd with increasing latitude
# ie the further north we go, the less warming is needed
# same pattern as for all species

# COHORTS SPLIT BY LATITUDE
df_gdd_2_to_5_clean %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  facet_wrap(~ cohort)
# shorter gdd with increasing latitude
# ie the further north we go, the less warming is needed
# same pattern as for all species
# BUT: kizilcahamam is at 1454
# but pattern also holds for 3yo withou kizilcahamam

# ALL COHORTS BY ALTITUDE
df_gdd_2_to_5_clean %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# longer gdd with increasing altitude
# ie the further up we go, the more warming is needed
# same pattern as for all species
# BUT: SEE ABOVE 

# COHORTS SPLIT BY ALTITUDE
df_gdd_2_to_5_clean %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  facet_wrap(~ cohort)
# shorter gdd with increasing latitude
# ie the further north we go, the less warming is needed
# same pattern as for all species
# BUT: kizilcahamam is at 1454
# but pattern also holds for 3yo withou kizilcahamam

# this matches findings by Ducousso et al 1995
# https://www.afs-journal.org/articles/forest/pdf/1996/02/AFS_0003-4312_1996_53_2-3_ART0053.pdf
# Their conclusion: Therefore the latest origins are more tolerant to frost by avoidance and resistance. 


# wilkinson et al 2017
# https://link.springer.com/article/10.1007/s10342-016-0998-z
# Variation with latitude, altitude and continentality
# 
# The date of budburst was linearly related to latitude of origin with provenances 
# from southern latitudes consistently leafing out earlier than those from more northerly latitudes, 
# but there was significant variation across years. 
# The relationship with the highest sensitivity (1.28 days later per degree north) occurred in 2006, 
# whilst the lowest (0.44 days per degree north) was in 2013 (Fig. 3). 
# Including altitude or continentality (either singularly or combined) 
# did not significantly improve the fit of the GLM (data not shown).
# --> they have the opposite pattern in terms of latitude

# These results demonstrate that spring temperature during the heat accumulation period 
# was the dominant environmental driver of budburst date; 
# mean daily air temperature during the winter chilling temperature had a much smaller, 
# yet still significant effect.





#### Q. ROBUR ####
# ALL COHORTS BY LATITUDE
df_gdd_2_to_5_clean %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# less gdd with increasing latitude
# ie the further north we go, the less warming is needed to complete budburst
# same pattern as for all species

# COHORTS SPLIT BY LATITUDE
df_gdd_2_to_5_clean %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%  
  ggplot(mapping = aes(x = latitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  facet_wrap(~ cohort)
# shorter gdd with increasing latitude
# ie the further north we go, the less warming is needed
# same pattern as for all species
# BUT: kizilcahamam is at 1454
# but pattern also holds for 3yo withou kizilcahamam

# ALL COHORTS BY ALTITUDE
df_gdd_2_to_5_clean %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%  
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# longer gdd with increasing altitude
# ie the further up we go, the more warming is needed
# same pattern as for all species
# BUT: SEE ABOVE 

# COHORTS SPLIT BY ALTITUDE
df_gdd_2_to_5_clean %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%  
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  facet_wrap(~ cohort)




#### GDD 2 to 5 Q. petraea ###
df_gdd_2_to_5_clean %>%
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



#### Q. robur only ####
df_robur_only <- df_gdd_2_to_5_clean %>%
  filter(species == "Q.robur")

robur_means <- aggregate(gdd_2_to_5 ~ site_name + altitude + longitude + latitude + site_wet, df_robur_only, mean)

## ALTITUDE
ggplot(data = df_robur_only, 
       mapping = aes(x = altitude, y = gdd_2_to_5)) +
  geom_point() +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(x = altitude, y = gdd_2_to_5, label = round(..y.., 2)),
    size = 3,
    color = "red",
    show.legend = FALSE,
    vjust = -1
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )

## LATITUDE
df_site_name_unique_robur <- distinct(df_robur_only, site_name, .keep_all = TRUE) %>%
  filter(species == "Q.robur")

ggplot(data = df_robur_only, 
       mapping = aes(x = latitude, y = gdd_2_to_5, colour = site_name)) +
  geom_point(size = 5, alpha = 0.5, position = position_dodge(width = 0.05)) +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(x = latitude, y = gdd_2_to_5, label = round(..y.., 2)),
    size = 5,
    color = "red",
    show.legend = FALSE,
    vjust = -9
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  scale_color_manual(values = my_pal) +
  geom_label_repel(data = df_site_name_unique_robur, aes(label = site_name))

ggplot(data = df_robur_only, 
       mapping = aes(x = reorder(site_name, latitude), y = gdd_2_to_5, colour = site_name)) +
  geom_point(size = 5, alpha = 0.5, position = position_dodge(width = 0.05)) +
  stat_summary(
    fun = mean,
    geom = "label",
    aes(x = site_name, y = gdd_2_to_5, label = round(..y.., 2)),
    size = 5,
    color = "red",
    show.legend = FALSE,
    vjust = 0
  ) +
  scale_color_manual(values = my_pal) +
#+ geom_label_repel(data = df_site_name_unique_robur, aes(label = site_name))
  theme(
  axis.text = element_text(angle = 45, hjust = 1)
)

## LONGITUDE
ggplot(data = df_robur_only, 
       mapping = aes(x = longitude, y = gdd_2_to_5, colour = site_name)) +
  geom_point(size = 3) +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(x = longitude, y = gdd_2_to_5, label = round(..y.., 2)),
    size = 3,
    color = "red",
    show.legend = FALSE,
    vjust = -1
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  scale_color_manual(values = my_pal)

ggplot(data = df_robur_only, 
       mapping = aes(x = site_wet, y = gdd_2_to_5)) +
  geom_point() +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(x = latitude, y = gdd_2_to_5, label = round(..y.., 2)),
    size = 3,
    color = "red",
    show.legend = FALSE,
    vjust = -1
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
