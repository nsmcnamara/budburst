### Exploratory Data Analysis for Time to 5 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-15
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

# calculate means and counts  
means <- df_gdd_2_to_5 %>%
  group_by(species) %>%
  summarize(m = mean(gdd_2_to_5))
### robur seems faster

counts <- df_gdd_2_to_5 %>%
  group_by(species) %>%
  summarise(n = n())

sum_stats <- df_gdd_2_to_5 %>%
  group_by(species, site_name, age, latitude, longitude, altitude) %>%
  summarize(mean = mean(gdd_2_to_5))

#### PLOT THE DATA ####
#### ALL SPECIES ####
sumstat_gdd_2to5_all <- df_gdd_2_to_5 %>%
  summarize(n = n(),
                   m = mean(gdd_2_to_5), 
                   var = var(gdd_2_to_5),
                   sd = sd(gdd_2_to_5))

# histogram
h_gdd_2to5_all <- ggplot(data = df_gdd_2_to_5, 
                     mapping = aes(gdd_2_to_5, after_stat(density), alpha = 0.8)) +
  geom_histogram(bins = 20, color = "black") +
  geom_vline(xintercept = sumstat_gdd_2to5_all$m, color = "red") +
  theme_bw() +
  labs(title = "GDD 2 to 5, all",
       x = "GDD 2 to 5",
       y = "Frequency") +
  guides(alpha = "none")

# print
h_gdd_2to5_all
### +/- 70 gdd

# save
ggsave(filename = "h_gdd_2to5_all.png", device = png, plot = h_gdd_2to5_all, path = "output/figs")

## raincloud
# means by species
means <- df_gdd_2_to_5 %>%
  group_by(species) %>%
  summarize(m = mean(gdd_2_to_5))
counts <- df_gdd_2_to_5 %>%
  group_by(species) %>%
  summarise(n = n())

# plot
ggplot(df_gdd_2_to_5, aes(x = forcats::fct_relevel(species, "Q.robur", "Q.pubescens", "Q.petraea"), 
                      y = gdd_2_to_5 , 
                      colour = species, fill = species)) +
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
  # annotate mean and n
  annotate(
    "text",
    x = 3.5, 
    y = 350,
    label = paste("Q. petraea, ", "n = ", counts[1,2], "mean =", round(means[1,2],2)),
    colour = my_pal_species[1]
  ) +
  annotate(
    "text",
    x = 2.5, 
    y = 350,
    label = paste("Q. pubescens, ","n = ", counts[2,2], "mean =", round(means[2,2],2)),
    colour = my_pal_species[2]
  ) +
  annotate(
    "text",
    x = 1.5, 
    y = 350,
    label = paste("Q. robur, ","n = ", counts[3,2], "mean =", round(means[3,2],2)),
    colour = my_pal_species[3]
  ) +
  coord_flip(xlim = c(1, NA), ylim = c(0, 400), expand = TRUE, clip = "on") +
  scale_x_discrete(breaks = seq(100, 400, 50)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.2)) +
  labs(x = "Species",
       y = "GDD 2 to 5",
       colour = "Species") +
  scale_alpha(guide = "none") +
  labs(fill = "Species", colour = "Species") +
  scale_fill_manual(values = my_pal_species) +
  scale_color_manual(values = my_pal_species) 





#### CORR w EXPL ####
## ALL SPECIES BY AlTITUDE ##

ggplot(data = df_gdd_2_to_5,
       mapping = aes(x = altitude, y = gdd_2_to_5,
                     color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# seems pretty even

# ALL SPECIES BY LATITUDE
ggplot(data = df_gdd_2_to_5,
       mapping = aes(x = latitude, y = gdd_2_to_5,
                     color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# seems pretty even

# ALL SPECIES BY LONGITUDE
ggplot(data = df_gdd_2_to_5,
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
df_gdd_2_to_5 %>%
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
df_gdd_2_to_5 %>%
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

# contrasting patterns

# ALL COHORTS BY ALTITUDE
df_gdd_2_to_5 %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )

# no pattern really

# COHORTS SPLIT BY ALTITUDE
df_gdd_2_to_5 %>%
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
# contrasting patterns
## in 2022 much larger variation than in 2023 for altitude and latitude






#### Q. ROBUR ####
# ALL COHORTS BY LATITUDE
df_gdd_2_to_5 %>%
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
# further north more warming required

# COHORTS SPLIT BY LATITUDE
df_gdd_2_to_5 %>%
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
# opposite pattern.. so other pattern is created by differences in cohorts


# ALL COHORTS BY ALTITUDE
df_gdd_2_to_5 %>%
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
df_gdd_2_to_5 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%  
  filter(site_name != "Cestas") %>%  
  filter(site_name != "Groane") %>%  
  filter(site_name != "Laveyron(Tarbes/landouc)") %>%  
  filter(site_name != "Locarno") %>%  
  filter(site_name != "Guca") %>%  
  
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  facet_wrap(~ cohort)
# now opposite pattern
# ie the further up we go, the less warming is needed
# pattern created by cohorts 


#### GDD 2 to 5 Q. petraea ###
df_gdd_2_to_5 %>%
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


df_gdd_2_to_5 %>%
  select(gdd_2_to_5, species, altitude, latitude, longitude, cohort, site_wet) %>%
  ggpairs(mapping = aes(color = species, alpha = 0.5))
## altitude, latitude, longitude, correlated but also to gdd_2_to_5



#### Q. robur only ####
df_robur_only <- df_gdd_2_to_5 %>%
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




####### Copy #####


#### Plot the Data ####


## Time to 5 by ALL ##
# summary stats
sumstat_gdd_25_all <- df_gdd_2_to_5 %>%
  dplyr::summarize(n = n(),
                   m = mean(gdd_2_to_5), 
                   var = var(gdd_2_to_5),
                   sd = sd(gdd_2_to_5))

# histogram
gdd_25_all <- ggplot(data = df_gdd_2_to_5, 
                     mapping = aes(gdd_2_to_5, after_stat(density), alpha = 0.8)) +
  geom_histogram(bins = 20, color = "black") +
  geom_vline(xintercept = sumstat_gdd_25_all$m, color = "red") +
  theme_bw() +
  labs(title = "DOY Stage 2, all",
       x = "DOY Stage 2",
       y = "Frequency") +
  guides(alpha = "none")

# print
gdd_25_all
### +/- 70 days for stage 2

# save
ggsave(filename = "gdd_25_all.png", device = png, plot = gdd_25_all, path = "output/figs")

## raincloud
# means by species
means <- df_gdd_2_to_5 %>%
  group_by(species) %>%
  summarize(m = mean(gdd_2_to_5))
counts <- df_gdd_2_to_5 %>%
  group_by(species) %>%
  summarise(n = n())

# plot
ggplot(df_gdd_2_to_5, aes(x = forcats::fct_relevel(species, "Q.robur", "Q.pubescens", "Q.petraea"), 
                      y = gdd_2_to_5, 
                      colour = species, fill = species)) +
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
  # annotate mean and n
  annotate(
    "text",
    x = 3.5, 
    y = 350,
    label = paste("Q. petraea, ", "n = ", counts[1,2], "mean =", round(means[1,2],2)),
    colour = my_pal_species[1]
  ) +
  annotate(
    "text",
    x = 2.5, 
    y = 350,
    label = paste("Q. pubescens, ","n = ", counts[2,2], "mean =", round(means[2,2],2)),
    colour = my_pal_species[2]
  ) +
  annotate(
    "text",
    x = 1.5, 
    y = 350,
    label = paste("Q. robur, ","n = ", counts[3,2], "mean =", round(means[3,2],2)),
    colour = my_pal_species[3]
  ) +
  coord_flip(xlim = c(1, NA), ylim = c(0, 400), expand = TRUE, clip = "on") +
  scale_x_discrete(breaks = seq(100, 400, 50)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.2)) +
  labs(x = "Species",
       y = "GDD 2 to 5",
       colour = "Species") +
  scale_alpha(guide = "none") +
  labs(fill = "Species", colour = "Species") +
  scale_fill_manual(values = my_pal_species) +
  scale_color_manual(values = my_pal_species) 



## DOY by Species ##
# summary stats
sumstat_gdd_25_by_species <- df_gdd_2_to_5 %>%
  group_by(species) %>%
  summarize(n = n(),
            m = mean(gdd_2_to_5), 
            var = var(gdd_2_to_5),
            sd = sd(gdd_2_to_5))
# histogram
gdd_25_by_species <- ggplot(df_gdd_2_to_5, 
                            mapping = aes(gdd_2_to_5, 
                                          fill = species, alpha = 0.7)) +
  geom_histogram(bins = 20, color = "black") +
  geom_vline(data = sumstat_gdd_25_by_species, aes(xintercept = m, color = species)) +
  facet_wrap(~species, ncol = 1) +
  scale_fill_manual(values = my_pal_species) +
  scale_color_manual(values = my_pal_species) +
  theme_bw() +
  labs(title = "GDD 2 to 5, split by Species, all cohorts combined",
       x = "GDD to 5",
       y = "Frequency"
       ) +
  guides(alpha = "none", color = "none", fill = "none")

# print
gdd_25_by_species

# save
ggsave(filename = "gdd_25_by_species.png", device = png, plot = gdd_25_by_species, path = "output/figs")

# robur, pubescens, petraea






#### CORR w EXPL ####
## ALL SPECIES BY AlTITUDE ##
ggplot(data = df_gdd_2_to_5 %>%
         filter(site_name != "Bosco_Pantano"),
       mapping = aes(x = altitude, y = gdd_2_to_5,
                     color = reorder(site_name, altitude))) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )

# no pattern

# ALL SPECIES BY LATITUDE
ggplot(data = df_gdd_2_to_5,
       mapping = aes(x = latitude, y = gdd_2_to_5,
                     color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# no pattern




#### Q. ROBUR ####
### GDD 2 to 5 for Q. Robur by site
# all sites for robur 
gdd_2_to_5_robur_by_site <- df_gdd_2_to_5 %>% 
  filter(species == "Q.robur") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_2_to_5, y = after_stat(density),
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 40, colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal) +
  labs(title = "GDD 2 to 5 for Q. robur", 
       subtitle = "split by Site, all Cohorts",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Site name") +
  xlim(0, 150) +
  theme(legend.position = "none") +
  guides(alpha = "none", fill = "none") +
  theme_bw()

# print
gdd_2_to_5_robur_by_site

# save
ggsave(filename = "gdd_2_to_5_robur_by_site.png", 
       device = png, width = 5,
       plot = gdd_2_to_5_robur_by_site, 
       path = "output/figs")


# all sites for robur, by site and cohort 
gdd_2_to_5_robur_by_site_and_cohort <- df_gdd_2_to_5 %>% 
  filter(species == "Q.robur") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_2_to_5, y = after_stat(density),
                       fill = cohort, alpha = 0.5)) +
  geom_histogram(bins = 40, position = "dodge", colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 2) +
  scale_fill_manual(values = my_pal_cohorts) +
  labs(title = "GDD above 5 until Stage 2 for Q. robur", 
       subtitle = "split by Site and Cohort, ordered by Latitude",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Cohort") +
  xlim(0, 150) +
  ylim(0, 0.1) +
  guides(alpha = "none", fill = guide_legend(override.aes = list(alpha = 0.5))) +
  theme_bw()


# print
gdd_2_to_5_robur_by_site_and_cohort

ggsave(filename = "gdd_2_to_5_robur_by_site_and_cohort.png", 
       device = png, width = 10, height = 10,
       plot = gdd_2_to_5_robur_by_site_and_cohort,
       path = "output/figs")



# ALL COHORTS BY LATITUDE
robur_gdd_25_lat <- df_gdd_2_to_5 %>%
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_2_to_5,
                       color = reorder(site_name, latitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "Growing Degree Days for Q.robur",
       x = "Latitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

# print
robur_gdd_25_lat

# save
ggsave(filename = "robur_gdd_25-lat.png", device = png, plot = robur_gdd_25_lat, path = "output/figs")

# slight decrease


# without Bosco Pantano
robur_gdd_25_lat_no_bosco <- df_gdd_2_to_5 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_2_to_5,
                       color = reorder(site_name, latitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "GDD 2 to 5 for Q.robur, without Bosco Pantano",
       x = "Latitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal[c(2:10)]) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

robur_gdd_25_lat_no_bosco
ggsave(filename = "robur_gdd_25-lat_no-bosco.png", device = png, plot = robur_gdd_25_lat_no_bosco, path = "output/figs")

# increase


# ALL COHORTS BY ALTITUDE
robur_gdd_25_alt <- df_gdd_2_to_5 %>%
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = reorder(site_name, altitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "GDD 2 to 5 for Q.robur",
       x = "Altitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

robur_gdd_25_alt
ggsave(filename = "robur_gdd_25-alt.png", device = png, plot = robur_gdd_25_alt, path = "output/figs")
# no clear pattern

# without Bosco Pantano
robur_gdd_25_alt_no_bosco <- df_gdd_2_to_5 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = reorder(site_name, altitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "Growing Degree Days for Q.robur, without Bosco Pantano",
       x = "Altitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal[c(2:10)]) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

robur_gdd_25_alt_no_bosco
ggsave(filename = "robur_gdd_25-alt_no-bosco.png", device = png, plot = robur_gdd_25_alt_no_bosco, path = "output/figs")
# increase with altitude

## The Austrians ##
# sumstats

sumstat_robur_gdd_25_austrians <- df_gdd_2_to_5 %>%
  filter(species == "Q.robur") %>%
  filter(country == "Austria") %>%
  group_by(site_name, cohort) %>%
  summarize(n = n(),
            m = mean(gdd_2_to_5), 
            var = var(gdd_2_to_5),
            sd = sd(gdd_2_to_5))

site_order <- sumstat_robur_gdd_25_austrians %>%
  arrange(m) %>%
  pull(site_name)

df_robur_gdd_25_austrians <- df_gdd_2_to_5 %>%
  filter(species == "Q.robur") %>%
  filter(country == "Austria")

robur_gdd_25_austrians <- df_robur_gdd_25_austrians %>%
  ggplot(mapping = aes(gdd_2_to_5, 
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 10, color = "black") +
  geom_vline(data = sumstat_robur_gdd_25_austrians, aes(xintercept = m, color = site_name)) +  
  facet_grid(site_name ~ cohort) +
  scale_fill_manual(values = my_pal[c(8,7,5,9)]) +
  scale_color_manual(values = my_pal[c(8,7,5,9)]) +  
  theme_bw() +
  labs(title = "GDD 2 to 5: The Austrians",
       subtitle = "split by Collection Site, all Cohorts split",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Collection Site") +
  guides(fill = "none", alpha = "none", color = "none")

# print
robur_gdd_25_austrians

# save
ggsave(filename = "robur_gdd_25_austrians.png", device = png, plot = robur_gdd_25_austrians, path = "output/figs")

write.csv(sumstat_robur_gdd_25_austrians, file = "output/tables/robur_gdd_25_austrians.csv", row.names = FALSE)




#### Q. PETRAEA ####
### GDD 2 to 5 for Q. Petraea by site
# all sites for petraea 
gdd_2_to_5_pet_by_site <- df_gdd_2_to_5 %>% 
  filter(species == "Q.petraea") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_2_to_5, y = after_stat(density),
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 40, colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal) +
  labs(title = "GDD 2 to 5 for Q. petraea", 
       subtitle = "split by Site, all Cohorts",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Site name") +
  xlim(0, 150) +
  theme(legend.position = "none") +
  guides(alpha = "none", fill = "none") +
  theme_bw()

# print
gdd_2_to_5_pet_by_site

# save
ggsave(filename = "gdd_2_to_5_pet_by_site.png", 
       device = png, width = 5,
       plot = gdd_2_to_5_pet_by_site, 
       path = "output/figs")


# all sites for petraea, by site and cohort 
gdd_2_to_5_pet_by_site_and_cohort <- df_gdd_2_to_5 %>% 
  filter(species == "Q.petraea") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_2_to_5, y = after_stat(density),
                       fill = cohort, alpha = 0.5)) +
  geom_histogram(bins = 40, position = "dodge", colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal_cohorts) +
  labs(title = "GDD 2 to 5 for Q. petraea", 
       subtitle = "split by Site and Cohort, ordered by Latitude",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Cohort") +
  xlim(0, 150) +
  ylim(0, 0.1) +
  guides(alpha = "none", fill = guide_legend(override.aes = list(alpha = 0.5))) +
  theme_bw()


# print
gdd_2_to_5_pet_by_site_and_cohort

ggsave(filename = "gdd_2_to_5_pet_by_site_and_cohort.png", 
       device = png, width = 5, height = 10,
       plot = gdd_2_to_5_pet_by_site_and_cohort,
       path = "output/figs")



# ALL COHORTS BY LATITUDE
pet_gdd_lat <- df_gdd_2_to_5 %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_2_to_5,
                       color = reorder(site_name, latitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "Growing Degree Days for Q.petraea",
       x = "Latitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

# print
pet_gdd_lat

# save
ggsave(filename = "petraea_gdd_25-lat.png", device = png, plot = pet_gdd_lat, path = "output/figs")

# not meaningful


# ALL COHORTS BY ALTITUDE
pet_gdd_alt <- df_gdd_2_to_5 %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = reorder(site_name, altitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "Growing Degree Days for Q.petraea",
       x = "Altitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

# print
pet_gdd_alt

# save
ggsave(filename = "pet_gdd_25-alt.png", device = png, plot = pet_gdd_alt, path = "output/figs")
# no meaningful pattern


## The Germans ##
# sumstats

sumstat_pet_gdd_25_germans <- df_gdd_2_to_5 %>%
  filter(species == "Q.petraea") %>%
  filter(country == "Germany") %>%
  group_by(site_name, cohort) %>%
  summarize(n = n(),
            m = mean(gdd_2_to_5), 
            var = var(gdd_2_to_5),
            sd = sd(gdd_2_to_5))


pet_gdd_25_germans <- df_gdd_2_to_5 %>%
  filter(species == "Q.petraea") %>%
  filter(country == "Germany") %>%
  ggplot(mapping = aes(gdd_2_to_5, 
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 10, color = "black") +
  geom_vline(data = sumstat_pet_gdd_25_germans, aes(xintercept = m, color = site_name)) +  
  facet_grid(site_name ~ cohort) +
  scale_fill_manual(values = my_pal[c(4,3,6,7)]) +
  scale_color_manual(values = my_pal[c(4,3,6,7)]) +  
  theme_bw() +
  labs(title = "GDD 2 to 5: The Germans",
       subtitle = "split by Collection Site, all Cohorts split",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Collection Site") +
  guides(fill = "none", alpha = "none", color = "none")

# print
pet_gdd_25_germans

# save
ggsave(filename = "pet_gdd_25_germans.png", device = png, plot = pet_gdd_25_germans, path = "output/figs")

write.csv(sumstat_pet_gdd_25_germans, file = "output/tables/pet_gdd_25_germans.csv", row.names = FALSE)

##  there's nothing here




### Q. PUBESCENS ####
### GDD 2 to 5 for Q. pubescens by site
# all sites for pubescens 
gdd_2_to_5_pub_by_site <- df_gdd_2_to_5 %>% 
  filter(species == "Q.pubescens") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_2_to_5, y = after_stat(density),
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 40, colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal) +
  labs(title = "GDD above 5 until Stage 2 for Q. pubescens", 
       subtitle = "split by Site, all Cohorts",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Site name") +
  xlim(0, 150) +
  theme(legend.position = "none") +
  guides(alpha = "none", fill = "none") +
  theme_bw()

# print
gdd_2_to_5_pub_by_site

# save
ggsave(filename = "gdd_2_to_5_pub_by_site.png", 
       device = png, width = 5,
       plot = gdd_2_to_5_pub_by_site, 
       path = "output/figs")


# all sites for pubescens, by site and cohort 
gdd_2_to_5_pub_by_site_and_cohort <- df_gdd_2_to_5 %>% 
  filter(species == "Q.pubescens") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_2_to_5, y = after_stat(density),
                       fill = cohort, alpha = 0.5)) +
  geom_histogram(bins = 40, position = "dodge", colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal_cohorts) +
  labs(title = "GDD 2 to 5 for Q. pubescens", 
       subtitle = "split by Site and Cohort, ordered by Latitude",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Cohort") +
  xlim(0, 200) +
  ylim(0, 0.1) +
  guides(alpha = "none", fill = guide_legend(override.aes = list(alpha = 0.5))) +
  theme_bw()


# print
gdd_2_to_5_pub_by_site_and_cohort

ggsave(filename = "gdd_2_to_5_pub_by_site_and_cohort.png", 
       device = png, width = 5, height = 10,
       plot = gdd_2_to_5_pub_by_site_and_cohort,
       path = "output/figs")



# ALL COHORTS BY LATITUDE
pub_gdd_25_lat <- df_gdd_2_to_5 %>%
  filter(species == "Q.pubescens") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_2_to_5,
                       color = reorder(site_name, latitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "GDD 2 to 5 for Q.pubescens",
       x = "Latitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

# print
pub_gdd_25_lat

# save
ggsave(filename = "pubescens_gdd_25-lat.png", device = png, plot = pub_gdd_25_lat, path = "output/figs")

# meaningful ?


# ALL COHORTS BY ALTITUDE
pub_gdd_25_alt <- df_gdd_2_to_5 %>%
  filter(species == "Q.pubescens") %>%
  filter(year == "2023") %>%
  filter(age == "2") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_2_to_5,
                       color = reorder(site_name, altitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "GDD 2 to 5 for Q.pubescens",
       x = "Altitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

# print
pub_gdd_25_alt

# save
ggsave(filename = "pub_gdd_25-alt.png", device = png, plot = pub_gdd_25_alt, path = "output/figs")
# opposite patterns if both years or year 2023 only


## The Northerns ##
# sumstats

sumstat_pub_gdd_25_northern <- df_gdd_2_to_5 %>%
  filter(species == "Q.pubescens") %>%
  filter(latitude >= 45) %>%
  group_by(site_name, cohort) %>%
  summarize(n = n(),
            m = mean(gdd_2_to_5), 
            var = var(gdd_2_to_5),
            sd = sd(gdd_2_to_5))


pub_gdd_25_northern <- df_gdd_2_to_5 %>%
  filter(species == "Q.pubescens") %>%
  filter(latitude >= 45) %>%
  ggplot(mapping = aes(gdd_2_to_5, 
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 10, color = "black") +
  geom_vline(data = sumstat_pub_gdd_25_northern, aes(xintercept = m, color = site_name)) +  
  facet_grid(site_name ~ cohort) +
  scale_fill_manual(values = my_pal[c(4,3,6,7)]) +
  scale_color_manual(values = my_pal[c(4,3,6,7)]) +  
  theme_bw() +
  labs(title = "GDD 2 to 5: The Northerns",
       subtitle = "split by Collection Site, all Cohorts split",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Collection Site") +
  guides(fill = "none", alpha = "none", color = "none")

# print
pub_gdd_25_northern

# save
ggsave(filename = "pub_gdd_25_northern.png", device = png, plot = pub_gdd_25_northern, path = "output/figs")

write.csv(sumstat_pub_gdd_25_northern, file = "output/tables/pub_gdd_25_northern.csv", row.names = FALSE)



## The Southerns ##
# sumstats

sumstat_pub_gdd_25_southern <- df_gdd_2_to_5 %>%
  filter(species == "Q.pubescens") %>%
  filter(latitude <= 45) %>%
  group_by(site_name, cohort) %>%
  summarize(n = n(),
            m = mean(gdd_2_to_5), 
            var = var(gdd_2_to_5),
            sd = sd(gdd_2_to_5))



pub_gdd_25_southern <- df_gdd_2_to_5 %>%
  filter(species == "Q.pubescens") %>%
  filter(latitude <= 45) %>%
  ggplot(mapping = aes(gdd_2_to_5, 
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 10, color = "black") +
  #  geom_vline(data = sumstat_pub_gdd_southern, aes(xintercept = m, color = site_name)) +  
  facet_wrap(~ reorder(site_name, latitude), ncol = 1) +
  scale_fill_manual(values = my_pal[c(4,3,6,7)]) +
  scale_color_manual(values = my_pal[c(4,3,6,7)]) +  
  theme_bw() +
  labs(title = "GDD 2 to 5: The Southerns",
       subtitle = "split by Collection Site, all Cohorts split",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Collection Site") +
  guides(fill = "none", alpha = "none", color = "none")

# print
pub_gdd_25_southern

# save
ggsave(filename = "pub_gdd_25_southern.png", device = png, plot = pub_gdd_25_southern, path = "output/figs")

write.csv(sumstat_pub_gdd_25_southern, file = "output/tables/pub_gdd_25_southern.csv", row.names = FALSE)









#### OTHER STUFF ####




### QQ Plot
qqnorm(df_gdd_2_to_5$cum_temp_above_5)
qqline(df_gdd_2_to_5$cum_temp_above_5)

### Automatic EDA by ggally
df_gdd_2_to_5 %>%
  select(cum_temp_above_5, species, altitude, latitude, longitude, age) %>%
  ggpairs(mapping = aes(color = species, alpha = 0.5))




### robur
KG_robur %>%
  dplyr::select(site_name, longitude, latitude, climate)








