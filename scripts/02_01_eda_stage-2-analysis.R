### Exploratory Data Analysis for Stage 2 Analysis
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-14
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(GGally)
library(ggfortify)
library(plotly)

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
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)

glimpse(stage_2_for_analysis)
head(stage_2_for_analysis)
summary(stage_2_for_analysis)
str(stage_2_for_analysis)

### check NAs
sapply(stage_2_for_analysis, function(x) sum(is.na(x)))

df_doy_s2 <- stage_2_for_analysis
df_gdd_s2 <- stage_2_for_analysis %>%
  drop_na(gdd_above_5)



#### Plot the Data ####
#### DOY ####


## DOY by ALL ##
# summary stats
sumstat_doy_s2_all <- df_doy_s2 %>%
  summarize(n = n(),
            m = mean(doy_stage_2), 
            var = var(doy_stage_2),
            sd = sd(doy_stage_2))

# histogram
doy_s2_all <- ggplot(data = df_doy_s2, 
                          mapping = aes(doy_stage_2, after_stat(density), alpha = 0.8)) +
  geom_histogram(bins = 20, color = "black") +
  geom_vline(xintercept = sumstat_doy_s2_all$m, color = "red") +
  theme_bw() +
  labs(title = "DOY Stage 2, all",
       x = "DOY Stage 2",
       y = "Frequency") +
  guides(alpha = "none")

# print
doy_s2_all
### +/- 120 days for stage 2

# save
ggsave(filename = "doy_s2_all.png", device = png, plot = doy_s2_all, path = "output/figs")

## raincloud
# means by species
means <- df_doy_s2 %>%
  group_by(species) %>%
  summarize(m = mean(doy_stage_2))
counts <- df_doy_s2 %>%
  group_by(species) %>%
  summarise(n = n())

# plot
ggplot(df_doy_s2, aes(x = forcats::fct_relevel(species, "Q.robur", "Q.pubescens", "Q.petraea"), 
                      y = doy_stage_2, 
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
    y = 170,
    label = paste("n = ", counts[1,2], "mean =", round(means[1,2],2)),
    colour = my_pal_species[1]
  ) +
  annotate(
    "text",
    x = 2.5, 
    y = 170,
    label = paste("n = ", counts[2,2], "mean =", round(means[2,2],2)),
    colour = my_pal_species[2]
  ) +
  annotate(
    "text",
    x = 1.5, 
    y = 170,
    label = paste("n = ", counts[3,2], "mean =", round(means[3,2],2)),
    colour = my_pal_species[3]
  ) +
  coord_flip(xlim = c(1, NA), ylim = c(90, 180), expand = TRUE, clip = "on") +
  scale_x_discrete(breaks = seq(100, 400, 50)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.2)) +
  labs(x = "Species",
       y = "DOY Stage 2",
       colour = "Species") +
  scale_alpha(guide = "none") +
  labs(fill = "Species", colour = "Species") +
  scale_fill_manual(values = my_pal_species) +
  scale_color_manual(values = my_pal_species) 



## DOY by Species ##
# summary stats
sumstat_doy_s2_by_species <- df_doy_s2 %>%
  group_by(species) %>%
  summarize(n = n(),
            m = mean(doy_stage_2), 
            var = var(doy_stage_2),
            sd = sd(doy_stage_2))
# histogram
doy_s2_by_species <- ggplot(df_doy_s2, 
                                 mapping = aes(doy_stage_2, 
                                               fill = species, alpha = 0.7)) +
  geom_histogram(bins = 20, color = "black") +
  geom_vline(data = sumstat_doy_s2_by_species, aes(xintercept = m, color = species)) +
  facet_wrap(~species, ncol = 1) +
  scale_fill_manual(values = my_pal_species) +
  scale_color_manual(values = my_pal_species) +
  theme_bw() +
  labs(title = "DOY Stage 2, split by Species, all cohorts combined",
       x = "DOY Stage 2",
       y = "Frequency",
       caption = "All three species show a very similar distribution. 
       Mean values are 118.6, 117.6, 117.4 for petraea, pubescens and robur, respectively.") +
  guides(alpha = "none", color = "none", fill = "none")

# print
doy_s2_by_species

# save
ggsave(filename = "doy_s2_by_species.png", device = png, plot = doy_s2_by_species, path = "output/figs")

### very similar distributions.
### +/- 120 days for stage 2

# 
# 
# #### DOY 2023 ###
# # since so far I only have weather data for 2023 and I want to compare doy and gdd,
# # I should have the same plot as above but for 2023 only.
# # when we have weather data for 2022, can be removed.
# 
# ## DOY by ALL ##
# # summary stats
# sumstat_doy_s2_23 <- df_gdd_s2 %>%
#   summarize(n = n(),
#             m = mean(doy_stage_2), 
#             var = var(doy_stage_2),
#             sd = sd(doy_stage_2))
# 
# # histogram
# doy_s2_23_all <- ggplot(data = df_gdd_s2, 
#                           mapping = aes(doy_stage_2, after_stat(density), alpha = 0.8)) +
#   geom_histogram(bins = 20) +
#   geom_vline(xintercept = sumstat_doy_s2_23$m, color = "red") +
#   theme_bw() +
#   labs(title = "DOY Stage 2, 2023 only",
#        x = "DOY Stage 2",
#        y = "Frequency") +
#   guides(alpha = "none")
# 
# # print
# doy_s2_23_all
# ### +/- 120 days for stage 2
# 
# # save
# ggsave(filename = "doy_s2_23_all.png", device = png, plot = doy_s2_23_all, path = "output/figs")
# 
# 
# 
# 
# ## DOY by Species ##
# # summary stats
# sumstat_doy_s2_23_by_species <- df_gdd_s2 %>%
#   group_by(species) %>%
#   summarize(n = n(),
#             m = mean(doy_stage_2), 
#             var = var(doy_stage_2),
#             sd = sd(doy_stage_2))
# # histogram
# doy_s2_23_by_species <- ggplot(df_gdd_s2, 
#                             mapping = aes(doy_stage_2, 
#                                           fill = species, alpha = 0.7)) +
#   geom_histogram(bins = 20) +
#   geom_vline(data = sumstat_doy_s2_23_by_species, aes(xintercept = m, color = species)) +
#   facet_wrap(~species, ncol = 1) +
#   scale_fill_manual(values = my_pal) +
#   scale_color_manual(values = my_pal) +
#   theme_bw() +
#   labs(title = "DOY Stage 2, split by Species, 2023 only",
#        x = "DOY Stage 2",
#        y = "Frequency") +
#   guides(alpha = "none", color = "none", fill = "none")
# 
# # print
# doy_s2_23_by_species
# 
# # save
# ggsave(filename = "doy_s2_23_by_species.png", device = png, plot = doy_s2_23_by_species, path = "output/figs")

### pubescens, petraea, robur
### same pattern as in GDD


#### GDD ####
## GDD by ALL ##

# summary stats
sumstat_gdd_s2_all <- df_gdd_s2 %>%
  summarize(n = n(),
            m = mean(gdd_above_5), 
            var = var(gdd_above_5),
            sd = sd(gdd_above_5))


# histogram
gdd_s2_all <- ggplot(data = df_gdd_s2, 
                          mapping = aes(gdd_above_5, after_stat(density), alpha = 0.8)) +
  geom_histogram(bins = 20, color = "black") +
  geom_vline(xintercept = sumstat_gdd_s2_all$m, color = "red") +
  theme_bw() +
  labs(title = "GDD Stage 2, all",
       x = "GDD Stage 2",
       y = "Frequency") +
  guides(alpha = "none")

# print
gdd_s2_all

# save 
ggsave(filename = "gdd_s2_all.png", device = png, plot = gdd_s2_all, path = "output/figs")
### +/- 226 gdd for stage 2


  
  
## GDD by Species ##

# summary stats
sumstat_gdd_s2_by_species <- df_gdd_s2 %>%
  group_by(species) %>%
  summarize(n = n(),
            m = mean(gdd_above_5), 
            med = median(gdd_above_5),
            var = var(gdd_above_5),
            sd = sd(gdd_above_5))


# histogram
gdd_s2_by_species <- ggplot(df_gdd_s2,
                                 mapping = aes(gdd_above_5, 
                                               fill = species, alpha = 0.7)) +
  geom_histogram(bins = 20, color = "black") +
  geom_vline(data = sumstat_gdd_s2_by_species, aes(xintercept = m, color = species)) +
  facet_wrap( ~species, ncol = 1) +
  scale_fill_manual(values = my_pal_species) +
  scale_color_manual(values = my_pal_species) +
  theme_bw() +
  labs(title = "GDD Stage 2",
       subtitle = "split by Species, all Cohorts combined",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Species") +
  guides(fill = "none", alpha = "none", color = "none")

# print
gdd_s2_by_species

# save
ggsave(filename = "gdd_s2_by_species.png", device = png, plot = gdd_s2_by_species, path = "output/figs")

### petraea quite later, but peak at same time. median same for all three

## Raincloud by species
ggplot(df_gdd_s2, aes(x = species, y = gdd_above_5, 
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
  scale_fill_manual(values = my_pal_species) +
  scale_color_manual(values = my_pal_species) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.9)) +
  labs(x = "Species",
       y = "Growing Degree Days",
       colour = "Species") +
  scale_alpha(guide = "none") +
  labs(fill = "Species", colour = "Species")



# obviously if you put in altitude, latitude etc, it will cluster by these...
# #### PCA ##
# # Filter only numeric columns
# numeric_gdd_s2 <- df_gdd_s2 %>% 
#   select(altitude, latitude, doy_stage_2, gdd_above_5, age)
# 
# # Perform PCA
# # color by species
# pca_res <- prcomp(numeric_gdd_s2, scale. = TRUE)
# p <- autoplot(pca_res, data = df_gdd_s2, colour = "species", alpha = 0.5,
#               loadings = TRUE, loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 3)
# ggplotly(p)
# # species: no clear clustering
# 
# p <- autoplot(pca_res, data = df_gdd_s2, colour = "site_wet")
# ggplotly(p)
# # site_wet: no clear clustering
# 
# p <- autoplot(pca_res, data = df_gdd_s2, colour = "cohort")
# ggplotly(p)
# # cohort: clear clustering
# 
# p <- autoplot(pca_res, data = df_gdd_s2, colour = "site_name")
# ggplotly(p)
# # site_name: clear clustering: seems turkish ones are different
# 
# p <- autoplot(pca_res, data = df_gdd_s2, colour = "country")
# ggplotly(p)
# # country: confirms, turkish ones are different
# 
# p <- autoplot(pca_res, data = df_gdd_s2, colour = "altitude")
# ggplotly(p)
# # altitude: clear clustering --> explains turkish ones?
# 
# p <- autoplot(pca_res, data = df_gdd_s2, colour = "latitude")
# ggplotly(p)
# # latitude: clear clustering --> explains turkish ones?
# 
# p <- autoplot(pca_res, data = df_gdd_s2, colour = "doy_stage_2")
# ggplotly(p)
# # doy_stage_2: pattern left to right (bc its part of the PCA)
# 
# p <- autoplot(pca_res, data = df_gdd_s2, colour = "gdd_above_5")
# ggplotly(p)
# # gdd_above_5: pattern left to right (bc its part of the PCA)
# 
# ### turkish ones cluster together. altitude and latitude quite different here
# ### separate and repeat


# df_gdd_s2_tk <- df_gdd_s2 %>%
#   filter(country == "Turkey ")
# 
# df_gdd_s2_tk_num<- df_gdd_s2_tk %>%
#   select(altitude, latitude, doy_stage_2, gdd_above_5, age)
# 
# 
# # Perform PCA
# # color by species
# pca_tk_res <- prcomp(df_gdd_s2_tk_num)
# 
# p <- autoplot(pca_tk_res, data = df_gdd_s2_tk, colour = "site_name", alpha = 0.5,
#               loadings = TRUE, loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 3)
# ggplotly(p)
# # within turkey, altitude is the biggest factor.
# yes obviously... it varies more by altitude than by latitude..


# df_gdd_s2_isik <- df_gdd_s2 %>%
#   filter(site_name == "Işık Dağı")
# df_gdd_s2_isik_num <- df_gdd_s2_isik %>%
#   select(altitude, latitude, doy_stage_2, gdd_above_5, age)
# pca_isik_res <- prcomp(df_gdd_s2_isik_num)
# p <- autoplot(pca_isik_res, data = df_gdd_s2_isik, colour = "mother_id",
#               loadings = TRUE, loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 3)
# ggplotly(p)
# 
# scores <- pca_isik_res$x
# pca_isik_df <- cbind(df_gdd_s2_isik, scores)
# 
# ggplot(pca_isik_df, aes(PC1, PC2, colour = mother_id)) +
#   geom_point() +
#   stat_ellipse(geom = "polygon", aes(fill = mother_id),
#                alpha = 0.2,
#                show.legend = FALSE,
#                level = 0.95)

### revealed nothing really, other than that Turkish ones are a bit different
# because their distribution in terms of altitude is really quite different
# PC1 maybe too strong.




#### CORR w EXPL ####
## ALL SPECIES BY AlTITUDE ##
ggplot(data = df_gdd_s2 %>%
         filter(site_name != "Bosco_Pantano"),
       mapping = aes(x = altitude, y = gdd_above_5,
                     color = reorder(site_name, altitude))) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )

# slightly earlier gdd by stage 2 with increasing altitude, but doubt significance
# ie the further up, the less warming required to start budburst
# much more pronounced w/o Bosco Pantano
# compare w time to 5:
# slightly longer gdd with increasing altitude, but doubt significance

# ALL SPECIES BY LATITUDE
ggplot(data = df_gdd_s2,
       mapping = aes(x = latitude, y = gdd_above_5,
                     color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# slightly later gdd by stage 2 with increasing latitude,
# ie the further north, the more warming required
# compare w time to 5:
# shorter gdd with increasing latitude
# ie the further north we go, the less warming is needed to complete budburst




#### Q. ROBUR ####
### GDD above 5 for Q. Robur by site
# all sites for robur 
gdd_above_5_robur_by_site <- stage_2_for_analysis %>% 
  filter(species == "Q.robur") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 40, colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal) +
  labs(title = "GDD above 5 until Stage 2 for Q. robur", 
       subtitle = "split by Site, all Cohorts",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Site name") +
  xlim(100, 400) +
  theme(legend.position = "none") +
  guides(alpha = "none", fill = "none") +
  theme_bw()

# print
gdd_above_5_robur_by_site

# save
ggsave(filename = "gdd_above_5_robur_by_site.png", 
       device = png, width = 5,
       plot = gdd_above_5_robur_by_site, 
       path = "output/figs")


# all sites for robur, by site and cohort 
gdd_above_5_robur_by_site_and_cohort <- df_gdd_s2 %>% 
  filter(species == "Q.robur") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = cohort, alpha = 0.5)) +
  geom_histogram(bins = 40, position = "dodge", colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal_cohorts) +
  labs(title = "GDD above 5 until Stage 2 for Q. robur", 
       subtitle = "split by Site and Cohort, ordered by Latitude",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Cohort") +
  xlim(100, 350) +
  ylim(0, 0.1) +
  guides(alpha = "none") +
  theme_bw()


# print
gdd_above_5_robur_by_site_and_cohort

ggsave(filename = "gdd_above_5_robur_by_site_and_cohort.png", 
       device = png, width = 5, height = 10,
       plot = gdd_above_5_robur_by_site_and_cohort,
       path = "output/figs")



# ALL COHORTS BY LATITUDE
robur_gdd_lat <- df_gdd_s2 %>%
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_above_5,
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
robur_gdd_lat

# save
ggsave(filename = "robur_gdd-lat.png", device = png, plot = robur_gdd_lat, path = "output/figs")

# no pattern


# without Bosco Pantano
robur_gdd_lat_no_bosco <- df_gdd_s2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_above_5,
                       color = reorder(site_name, latitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "Growing Degree Days for Q.robur, without Bosco Pantano",
       x = "Latitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal[c(2:10)]) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

robur_gdd_lat_no_bosco
ggsave(filename = "robur_gdd-lat_no-bosco.png", device = png, plot = robur_gdd_lat_no_bosco, path = "output/figs")

# clear pattern
# now pattern matches with TIME TO 5 Q. petraea
# the further north, the less warming is required



# # COHORTS SPLIT BY LATITUDE
# # bosco pantano removed
# df_gdd_s2 %>%
#   filter(species == "Q.robur") %>%
#   filter(site_name != "Bosco_Pantano") %>%
# # filter(age == "2") %>%
#   ggplot(mapping = aes(x = latitude, y = gdd_above_5,
#                        color = site_name)) +
#   geom_point() +
#   geom_smooth(
#     method = "lm",
#     se = FALSE,
#     color = "blue"
#   ) +
#   facet_wrap(~cohort)
# # can't really say anything about the 3 year 

# ALL COHORTS BY ALTITUDE
robur_gdd_alt <- df_gdd_s2 %>%
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_above_5,
                       color = reorder(site_name, altitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "Growing Degree Days for Q.robur",
       x = "Altitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

robur_gdd_alt
# no pattern
ggsave(filename = "robur_gdd-alt.png", device = png, plot = robur_gdd_alt, path = "output/figs")


# without Bosco Pantano
robur_gdd_alt_no_bosco <- df_gdd_s2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_above_5,
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

robur_gdd_alt_no_bosco
ggsave(filename = "robur_gdd-alt_no-bosco.png", device = png, plot = robur_gdd_alt_no_bosco, path = "output/figs")


## The Austrians ##
# sumstats

sumstat_robur_gdd_austrians <- df_gdd_s2 %>%
  filter(species == "Q.robur") %>%
  filter(country == "Austria") %>%
  group_by(site_name, cohort) %>%
  summarize(n = n(),
            m = mean(gdd_above_5), 
            var = var(gdd_above_5),
            sd = sd(gdd_above_5))

site_order <- sumstat_robur_gdd_austrians %>%
  arrange(m) %>%
  pull(site_name)

df_robur_gdd_austrians <- df_gdd_s2 %>%
  filter(species == "Q.robur") %>%
  filter(country == "Austria") %>%
  mutate(site_name = factor(site_name, levels = site_order))


robur_gdd_austrians <- df_robur_gdd_austrians %>%
  ggplot(mapping = aes(gdd_above_5, 
                      fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 10, color = "black") +
#  geom_vline(data = sumstat_robur_gdd_austrians, aes(xintercept = m, color = site_name)) +
  geom_vline(data = sumstat_robur_gdd_austrians, aes(xintercept = m, color = site_name)) +  
  facet_grid(site_name ~ cohort) +
  scale_fill_manual(values = my_pal[c(8,7,5,9)]) +
  scale_color_manual(values = my_pal[c(7,9,5,8)]) +  
  theme_bw() +
  labs(title = "GDD Stage 2: The Austrians",
       subtitle = "split by Collection Site, all Cohorts split, ordered by mean GDD",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Collection Site") +
  guides(fill = "none", alpha = "none", color = "none")

# print
robur_gdd_austrians

# save
ggsave(filename = "robur_gdd_austrians.png", device = png, plot = robur_gdd_austrians, path = "output/figs")

write.csv(sumstat_robur_gdd_austrians, file = "output/tables/robur_gdd_austrians.csv", row.names = FALSE)




# ### Growing Degree Days for Q. Robur by Site
# ## make a plot of doy by site_name
# q_robur <- filter(df_doy_s2, species == "Q.robur")
# 
# ggplot(q_robur, aes(x = reorder(site_name, site_wet), y = gdd_above_5, 
#                                  colour = site_name, fill = site_name, alpha = 0.5)) +
#   ggdist::stat_halfeye(
#     adjust = 0.5,
#     width = 0.6,
#     justification = -.2,
#     .width = 0,
#     point_colour = NA
#   ) +
#   geom_boxplot(
#     width = .12,
#     show.legend = FALSE
#   ) +
#   ggdist::stat_dots(
#     position = "dodge",
#     scale = 0.5,
#     side = "left",
#     dotsize = 1,
#     justification = 1.2,
#     show.legend = FALSE
#   ) +
# 
#   coord_cartesian(xlim = c(1.2, NA)) +
# #  scale_y_continuous(breaks = seq(100, 150, 10)) +
#   theme_bw() +
#   labs(x = "Site name",
#        y = "Growing Degree Days") +
#   scale_alpha(guide = "none") +
#   labs(fill = "Site", colour = "Site") +
#   facet_wrap( ~ age)
# acorns_only <- subset(stage_2_for_analysis, stage_2_for_analysis$age == 2)
# 
# 
# ggplot(q_robur, aes(x = reorder(site_name, altitude), y = gdd_above_5, 
#                                  colour = site_name, fill = site_name, alpha = 0.5)) +
#   ggdist::stat_halfeye(
#     adjust = 0.5,
#     width = 0.6,
#     justification = -.2,
#     .width = 0,
#     point_colour = NA
#   ) +
#   geom_boxplot(
#     width = .12,
#     show.legend = FALSE
#   ) +
#   ggdist::stat_dots(
#     position = "dodge",
#     scale = 0.5,
#     side = "left",
#     dotsize = 1,
#     justification = 1.2,
#     show.legend = FALSE
#   ) +
#   coord_cartesian(xlim = c(1.2, NA)) +
#   scale_y_continuous(breaks = seq(100, 400, 50)) +
#   theme_bw() +
#   theme(legend.position = c(0.9, 0.9)) +
#   theme(axis.text = element_text(angle = 90)) +
#   labs(x = "Species",
#        y = "Growing Degree Days",
#        colour = "Species") +
#   scale_alpha(guide = "none") +
#   labs(fill = "Species", colour = "Species") +
#   facet_wrap( ~ age)














#### Q. PETRAEA ####
### GDD above 5 for Q. Petraea by site
# all sites for petraea 
gdd_above_5_pet_by_site <- df_gdd_s2 %>% 
  filter(species == "Q.petraea") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 40, colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal) +
  labs(title = "GDD above 5 until Stage 2 for Q. petraea", 
       subtitle = "split by Site, all Cohorts",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Site name") +
  xlim(100, 400) +
  theme(legend.position = "none") +
  guides(alpha = "none", fill = "none") +
  theme_bw()

# print
gdd_above_5_pet_by_site

# save
ggsave(filename = "gdd_above_5_pet_by_site.png", 
       device = png, width = 5,
       plot = gdd_above_5_pet_by_site, 
       path = "output/figs")


# all sites for robur, by site and cohort 
gdd_above_5_pet_by_site_and_cohort <- df_gdd_s2 %>% 
  filter(species == "Q.petraea") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = cohort, alpha = 0.5)) +
  geom_histogram(bins = 40, position = "dodge", colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal_cohorts) +
  labs(title = "GDD above 5 until Stage 2 for Q. petraea", 
       subtitle = "split by Site and Cohort, ordered by Latitude",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Cohort") +
  xlim(100, 350) +
  ylim(0, 0.1) +
  guides(alpha = "none", fill = guide_legend(override.aes = list(alpha = 0.5))) +
  theme_bw()


# print
gdd_above_5_pet_by_site_and_cohort

ggsave(filename = "gdd_above_5_pet_by_site_and_cohort.png", 
       device = png, width = 5, height = 10,
       plot = gdd_above_5_pet_by_site_and_cohort,
       path = "output/figs")



# ALL COHORTS BY LATITUDE
pet_gdd_lat <- df_gdd_s2 %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_above_5,
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
ggsave(filename = "petraea_gdd-lat.png", device = png, plot = pet_gdd_lat, path = "output/figs")

# not meaningful


# ALL COHORTS BY ALTITUDE
pet_gdd_alt <- df_gdd_s2 %>%
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_above_5,
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
ggsave(filename = "pet_gdd-alt.png", device = png, plot = pet_gdd_alt, path = "output/figs")
# no meaningful pattern


## The Germans ##
# sumstats

sumstat_pet_gdd_germans <- df_gdd_s2 %>%
  filter(species == "Q.petraea") %>%
  filter(country == "Germany") %>%
  group_by(site_name, cohort) %>%
  summarize(n = n(),
            m = mean(gdd_above_5), 
            var = var(gdd_above_5),
            sd = sd(gdd_above_5))

# site_order_pet <- sumstat_pet_gdd_germans %>%
#   arrange(m) %>%
#   pull(site_name)
# 
# df_pet_gdd_germans <- df_gdd_s2 %>%
#   filter(species == "Q.petraea") %>%
#   filter(country == "Germany") %>%
#   mutate(site_name = factor(site_name, levels = site_order_pet))


pet_gdd_germans <- df_gdd_s2 %>%
  filter(species == "Q.petraea") %>%
  filter(country == "Germany") %>%
  ggplot(mapping = aes(gdd_above_5, 
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 10, color = "black") +
  geom_vline(data = sumstat_pet_gdd_germans, aes(xintercept = m, color = site_name)) +  
  facet_grid(site_name ~ cohort) +
  scale_fill_manual(values = my_pal[c(4,3,6,7)]) +
  scale_color_manual(values = my_pal[c(4,3,6,7)]) +  
  theme_bw() +
  labs(title = "GDD Stage 2: The Germans",
       subtitle = "split by Collection Site, all Cohorts split, ordered by mean GDD",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Collection Site") +
  guides(fill = "none", alpha = "none", color = "none")

# print
pet_gdd_germans

# save
ggsave(filename = "pet_gdd_germans.png", device = png, plot = pet_gdd_germans, path = "output/figs")

write.csv(sumstat_pet_gdd_germans, file = "output/tables/pet_gdd_germans.csv", row.names = FALSE)

##  there's nothing here




# ### GDD above 5 for Q.petraea by site
# # all sites for all species individually
# # ggplot(data = stage_2_for_analysis,
# #       mapping = aes(x = gdd_above_5, y = ..density..,
# #                     fill = species, alpha = 0.5)) +
# #  geom_histogram(bins = 14) +
# #  facet_wrap(~site_name)
# 
# # all sites for petraea 
# gdd_above_5_petraea_by_site <- stage_2_for_analysis %>% 
#   filter(species == "Q.petraea") %>%
#   ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
#                     fill = site_name)) +
#   geom_histogram(bins = 40, colour = "black") +
#   geom_density(alpha = 0.5) +
#   facet_wrap(~site_name, ncol = 1) +
#   scale_fill_brewer(palette = "Set2") +
#   labs(title = "GDD above 5 until Stage 2 for Q. petraea",
#        subtitle = "split by Site, all Cohorts combined",
#        x = "Growing Degree Days",
#        y = "Frequency",
#        fill = "Site name") +
#   xlim(100, 400) +
#   theme(legend.position = "none")
# 
# 
# gdd_above_5_petraea_by_site
# 
# ggsave(filename = "gdd_above_5_petraea_by_site.png", 
#        device = png, width = 5,
#        plot = gdd_above_5_petraea_by_site, 
#        path = "output/figs")
# 
# 
# # all sites for petraea, by cohort
# gdd_above_5_petraea_by_site_and_cohort <- stage_2_for_analysis %>% 
#   filter(species == "Q.petraea") %>%
#   ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
#                        fill = cohort)) +
#   geom_histogram(bins = 40, position = "dodge", colour = "black") +
#   geom_density(alpha = 0.5) +
#   facet_wrap(~site_name, ncol = 1) +
#   scale_fill_brewer(palette = "Set2") +
#   labs(title = "GDD above 5 until Stage 2 for Q. petraea",
#        subtitle = "split by Site, coloured by Cohort",
#        x = "Growing Degree Days",
#        y = "Frequency",
#        fill = "Cohort") +
#   xlim(100, 400) +
#   ylim(0, 0.1)
# 
# gdd_above_5_petraea_by_site_and_cohort
# ggsave(filename = "gdd_above_5_petraea_by_site_and_cohort.png", 
#        device = png, width = 5, height = 5,
#        plot = gdd_above_5_petraea_by_site_and_cohort,
#        path = "output/figs")
# 
# 
# 

### GDD above 5 for Q. pubescens by site
# all sites for pubescens 
gdd_above_5_pub_by_site <- df_gdd_s2 %>% 
  filter(species == "Q.pubescens") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
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
  xlim(100, 400) +
  theme(legend.position = "none") +
  guides(alpha = "none", fill = "none") +
  theme_bw()

# print
gdd_above_5_pub_by_site

# save
ggsave(filename = "gdd_above_5_pub_by_site.png", 
       device = png, width = 5,
       plot = gdd_above_5_pub_by_site, 
       path = "output/figs")


# all sites for pubescens, by site and cohort 
gdd_above_5_pub_by_site_and_cohort <- df_gdd_s2 %>% 
  filter(species == "Q.pubescens") %>%
  mutate(site_name = reorder(site_name, latitude)) %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = cohort, alpha = 0.5)) +
  geom_histogram(bins = 40, position = "dodge", colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal_cohorts) +
  labs(title = "GDD above 5 until Stage 2 for Q. pubescens", 
       subtitle = "split by Site and Cohort, ordered by Latitude",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Cohort") +
  xlim(100, 350) +
  ylim(0, 0.1) +
  guides(alpha = "none", fill = guide_legend(override.aes = list(alpha = 0.5))) +
  theme_bw()


# print
gdd_above_5_pub_by_site_and_cohort

ggsave(filename = "gdd_above_5_pub_by_site_and_cohort.png", 
       device = png, width = 5, height = 10,
       plot = gdd_above_5_pub_by_site_and_cohort,
       path = "output/figs")



# ALL COHORTS BY LATITUDE
pub_gdd_lat <- df_gdd_s2 %>%
  filter(species == "Q.pubescens") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_above_5,
                       color = reorder(site_name, latitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "Growing Degree Days for Q.pubescens",
       x = "Latitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

# print
pub_gdd_lat

# save
ggsave(filename = "pubescens_gdd-lat.png", device = png, plot = pub_gdd_lat, path = "output/figs")

# meaningful ?


# ALL COHORTS BY ALTITUDE
pub_gdd_alt <- df_gdd_s2 %>%
  filter(species == "Q.pubescens") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_above_5,
                       color = reorder(site_name, altitude), size = 0.5, alpha = 0.7)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue", size = 1
  ) +
  theme_bw() +
  labs(title = "Growing Degree Days for Q.pubescens",
       x = "Altitude",
       y = "Growing Degree Days",
       colour = "Collection Site") +
  scale_alpha(guide = "none") +
  scale_color_manual(values = my_pal) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))

# print
pub_gdd_alt

# save
ggsave(filename = "pub_gdd-alt.png", device = png, plot = pub_gdd_alt, path = "output/figs")
# good altitude pattern


## The Northerns ##
# sumstats

sumstat_pub_gdd_northern <- df_gdd_s2 %>%
  filter(species == "Q.pubescens") %>%
  filter(latitude >= 45) %>%
  group_by(site_name, cohort) %>%
  summarize(n = n(),
            m = mean(gdd_above_5), 
            var = var(gdd_above_5),
            sd = sd(gdd_above_5))

# site_order_pet <- sumstat_pet_gdd_germans %>%
#   arrange(m) %>%
#   pull(site_name)
# 
# df_pet_gdd_germans <- df_gdd_s2 %>%
#   filter(species == "Q.petraea") %>%
#   filter(country == "Germany") %>%
#   mutate(site_name = factor(site_name, levels = site_order_pet))


pub_gdd_northern <- df_gdd_s2 %>%
  filter(species == "Q.pubescens") %>%
  filter(latitude >= 45) %>%
  ggplot(mapping = aes(gdd_above_5, 
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 10, color = "black") +
  geom_vline(data = sumstat_pub_gdd_northern, aes(xintercept = m, color = site_name)) +  
  facet_grid(site_name ~ cohort) +
  scale_fill_manual(values = my_pal[c(4,3,6,7)]) +
  scale_color_manual(values = my_pal[c(4,3,6,7)]) +  
  theme_bw() +
  labs(title = "GDD Stage 2: The Northerns",
       subtitle = "split by Collection Site, all Cohorts split",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Collection Site") +
  guides(fill = "none", alpha = "none", color = "none")

# print
pub_gdd_northern

# save
ggsave(filename = "pub_gdd_northern.png", device = png, plot = pub_gdd_northern, path = "output/figs")

write.csv(sumstat_pub_gdd_northern, file = "output/tables/pub_gdd_northern.csv", row.names = FALSE)

##  maybe a slight tendency


## The Southerns ##
# sumstats

sumstat_pub_gdd_southern <- df_gdd_s2 %>%
  filter(species == "Q.pubescens") %>%
  filter(latitude <= 45) %>%
  group_by(site_name, cohort) %>%
  summarize(n = n(),
            m = mean(gdd_above_5), 
            var = var(gdd_above_5),
            sd = sd(gdd_above_5))

# site_order_pet <- sumstat_pet_gdd_germans %>%
#   arrange(m) %>%
#   pull(site_name)
# 
# df_pet_gdd_germans <- df_gdd_s2 %>%
#   filter(species == "Q.petraea") %>%
#   filter(country == "Germany") %>%
#   mutate(site_name = factor(site_name, levels = site_order_pet))


pub_gdd_southern <- df_gdd_s2 %>%
  filter(species == "Q.pubescens") %>%
  filter(latitude <= 45) %>%
  ggplot(mapping = aes(gdd_above_5, 
                       fill = site_name, alpha = 0.7)) +
  geom_histogram(bins = 10, color = "black") +
#  geom_vline(data = sumstat_pub_gdd_southern, aes(xintercept = m, color = site_name)) +  
  facet_wrap(~ reorder(site_name, latitude), ncol = 1) +
  scale_fill_manual(values = my_pal[c(4,3,6,7)]) +
  scale_color_manual(values = my_pal[c(4,3,6,7)]) +  
  theme_bw() +
  labs(title = "GDD Stage 2: The Southerns",
       subtitle = "split by Collection Site, all Cohorts split",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Collection Site") +
  guides(fill = "none", alpha = "none", color = "none")

# print
pub_gdd_southern

# save
ggsave(filename = "pub_gdd_southern.png", device = png, plot = pub_gdd_southern, path = "output/figs")

write.csv(sumstat_pub_gdd_northern, file = "output/tables/pub_gdd_northern.csv", row.names = FALSE)

##  maybe a slight tendency








#### OTHER STUFF ####




### QQ Plot
qqnorm(stage_2_for_analysis$cum_temp_above_5)
qqline(stage_2_for_analysis$cum_temp_above_5)

### Automatic EDA by ggally
stage_2_for_analysis %>%
  select(cum_temp_above_5, species, altitude, latitude, longitude, age) %>%
  ggpairs(mapping = aes(color = species, alpha = 0.5))





### pubescens ###
# 4 northern provenances of clim10, 4 southern of clim 13
# 4 northern by altitude: B-moist 236, B-dry 315, /// E-dry 560, E-wet 630
# 4 southern by altitude: Kurt-wet 920, Konya-wet(Ilgin) 1180, Konya-dry 1200, Isik-dry 1420
# matched pairs Kurt-Isik // Konya-Konya
# by latitude: Konya, Konya, Kurt, Isik, E, E, B, B
stage_2_for_analysis %>%
  filter(species == "Q.pubescens") %>%
  filter(age == 3) %>%
  group_by(site_name) %>%
  summarise(mean_doy = mean(doy_stage_2))

Konya-O 112. dry. 1200 south-med
Konya-I 115. wet. 1180 south-med
Eger-m. 115. wet. 630. north-low
Bitz_d  115. dry. 315  north-low 3y 2023
Kurt    116. wet. 920. South-med
büchs-m 116. wet. 236. north-low 3y 2023
eger-m  116. wet. 630. north-low 3y 2023
eger-d  117. dry. 560. north-low 3y 2023
Eger-d. 119. dry. 560. north-low
Büchs-m 119. wet. 236. north-low
Isik    120. dry. 1400 south-high
Bitz-d. 123. dry. 315  north-low



stage_2_for_analysis %>%
  filter(species == "Q.pubescens") %>%
  filter(age == 3) %>%
  group_by(site_name) %>%
  summarise(mean_gdd = mean(gdd_above_5))

konya-0 169
konya-i 182
kurt.   189
isik    212

at age 3
b-dry 185
b-wet 187
e-wet 189
e-dry 190

stage_2_for_analysis %>%
  filter(species == "Q.pubescens") %>%
  filter(age == 2) %>%
  group_by(site_name) %>%
  summarise(n = n())


### robur
KG_robur %>%
  dplyr::select(site_name, longitude, latitude, climate)







