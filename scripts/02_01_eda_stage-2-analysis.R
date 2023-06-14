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
doy_stage_2_all <- ggplot(data = df_doy_s2, 
                          mapping = aes(doy_stage_2, after_stat(density), alpha = 0.8)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = mean_doy_s2, color = "red") +
  theme_bw() +
  labs(title = "DOY Stage 2, all",
       x = "DOY Stage 2",
       y = "Frequency") +
  guides(alpha = "none")

# print
doy_stage_2_all
### +/- 120 days for stage 2

# save
ggsave(filename = "doy_s2_all.png", device = png, plot = doy_stage_2_all, path = "output/figs")




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
  geom_histogram(bins = 20) +
  geom_vline(data = sumstat_doy_s2_by_species, aes(xintercept = m, color = species)) +
  facet_wrap(~species, ncol = 1) +
  scale_fill_manual(values = my_pal) +
  scale_color_manual(values = my_pal) +
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



#### DOY 2023 ####
# since so far I only have weather data for 2023 and I want to compare doy and gdd,
# I should have the same plot as above but for 2023 only.
# when we have weather data for 2022, can be removed.

## DOY by ALL ##
# summary stats
sumstat_doy_s2_23 <- df_gdd_s2 %>%
  summarize(n = n(),
            m = mean(doy_stage_2), 
            var = var(doy_stage_2),
            sd = sd(doy_stage_2))

# histogram
doy_s2_23_all <- ggplot(data = df_gdd_s2, 
                          mapping = aes(doy_stage_2, after_stat(density), alpha = 0.8)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = sumstat_doy_s2_23$m, color = "red") +
  theme_bw() +
  labs(title = "DOY Stage 2, all",
       x = "DOY Stage 2",
       y = "Frequency") +
  guides(alpha = "none")

# print
doy_s2_23_all
### +/- 120 days for stage 2

# save
ggsave(filename = "doy_s2_23_all.png", device = png, plot = doy_s2_23_all, path = "output/figs")




## DOY by Species ##
# summary stats
sumstat_doy_s2_23_by_species <- df_gdd_s2 %>%
  group_by(species) %>%
  summarize(n = n(),
            m = mean(doy_stage_2), 
            var = var(doy_stage_2),
            sd = sd(doy_stage_2))
# histogram
doy_s2_23_by_species <- ggplot(df_gdd_s2, 
                            mapping = aes(doy_stage_2, 
                                          fill = species, alpha = 0.7)) +
  geom_histogram(bins = 20) +
  geom_vline(data = sumstat_doy_s2_23_by_species, aes(xintercept = m, color = species)) +
  facet_wrap(~species, ncol = 1) +
  scale_fill_manual(values = my_pal) +
  scale_color_manual(values = my_pal) +
  theme_bw() +
  labs(title = "DOY Stage 2, split by Species, all cohorts combined",
       x = "DOY Stage 2",
       y = "Frequency") +
  guides(alpha = "none", color = "none", fill = "none")

# print
doy_s2_23_by_species

# save
ggsave(filename = "doy_s2_23_by_species.png", device = png, plot = doy_s2_23_by_species, path = "output/figs")

### very similar distributions.
### +/- 120 days for stage 2


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
  geom_histogram(bins = 14) +
  geom_vline(xintercept = mean_gdd_s2_all, color = "red") +
  theme_bw() +
  labs(title = "GDD Stage 2, all",
       x = "GDD Stage 2",
       y = "Frequency") +
  guides(alpha = "none")

# print
gdd_s2_all

# save 
ggsave(filename = "gdd_s2_all.png", device = png, plot = gdd_s2_all, path = "output/figs")
### +/- 200 gdd for stage 2


  
  
## GDD by Species ##

# summary stats
sumstat_gdd_s2_by_species <- df_gdd_s2 %>%
  group_by(species) %>%
  summarize(n = n(),
            m = mean(gdd_above_5), 
            var = var(gdd_above_5),
            sd = sd(gdd_above_5))

# histogram
gdd_s2_by_species <- ggplot(df_gdd_s2,
                                 mapping = aes(gdd_above_5, 
                                               fill = species, alpha = 0.7)) +
  geom_histogram(bins = 14) +
  geom_vline(data = sumstat_gdd_s2_by_species, aes(xintercept = m, color = species)) +
  facet_wrap( ~species, ncol = 1) +
  scale_fill_manual(values = my_pal) +
  scale_color_manual(values = my_pal) +
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

### all around 200, but seem quite different
### pubescens early, petraea, then robur





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
# ALL COHORTS BY LATITUDE
df_gdd_s2 %>%
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_above_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# slightly later gdd by stage 2 with increasing latitude
# ie the further north, the more warming required until budburst starts
# same pattern as with all species
# 

# without Bosco Pantano
df_gdd_s2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  filter(site_name != "Schönberg_am_Kamp") %>%
  filter(site_name != "Diendorf_am_Walde") %>%
  filter(site_name != "Planck_am_Kamp") %>%
  filter(site_name != "Laveyron(Tarbes/landouc)") %>%
  
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
  scale_color_manual(values = my_pal[c(1,2,5,6,8)]) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 5, alpha = 0.7)))


# pattern is reversed without Bosco Pantano
# now pattern matches with TIME TO 5 Q. petraea
# the further north, the less warming is required

# COHORTS SPLIT BY LATITUDE
# bosco pantano removed
df_gdd_s2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
# filter(age == "2") %>%
  ggplot(mapping = aes(x = latitude, y = gdd_above_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  facet_wrap(~cohort)
# can't really say anything about the 3 year 

# ALL COHORTS BY ALTITUDE
df_gdd_to_stage_2 %>%
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_above_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# without Bosco Pantano
df_gdd_to_stage_2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_above_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  )
# again, the pattern changes w/o Bosco Pantano
# now, less warming required for higher altitudes

# COHORTS SPLIT BY ALTITUDE
# bosco pantano removed
df_gdd_to_stage_2 %>%
  filter(species == "Q.robur") %>%
  filter(site_name != "Bosco_Pantano") %>%
  # filter(age == "2") %>%
  ggplot(mapping = aes(x = altitude, y = gdd_above_5,
                       color = site_name)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  facet_wrap(~cohort)






















#### First Data Viz ####



## raincloud
# means by species
means <- stage_2_for_analysis %>%
  group_by(species) %>%
  summarize(m = mean(doy_stage_2))
counts <- stage_2_for_analysis %>%
  group_by(species) %>%
  summarise(n = n())

# plot
ggplot(stage_2_for_analysis, aes(x = forcats::fct_relevel(species, "Q.robur", "Q.pubescens", "Q.petraea"), 
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
    colour = Dark2palette[1]
  ) +
  annotate(
    "text",
    x = 2.5, 
    y = 170,
    label = paste("n = ", counts[2,2], "mean =", round(means[2,2],2)),
    colour = Dark2palette[2]
  ) +
  annotate(
    "text",
    x = 1.5, 
    y = 170,
    label = paste("n = ", counts[3,2], "mean =", round(means[3,2],2)),
    colour = Dark2palette[3]
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
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")








 #####
 #####













#### Petraea ####
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
                    fill = site_name)) +
  geom_histogram(bins = 40, colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "GDD above 5 until Stage 2 for Q. petraea",
       subtitle = "split by Site, all Cohorts combined",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Site name") +
  xlim(100, 400) +
  theme(legend.position = "none")


gdd_above_5_petraea_by_site

ggsave(filename = "gdd_above_5_petraea_by_site.png", 
       device = png, width = 5,
       plot = gdd_above_5_petraea_by_site, 
       path = "output/figs")


# all sites for petraea, by cohort
gdd_above_5_petraea_by_site_and_cohort <- stage_2_for_analysis %>% 
  filter(species == "Q.petraea") %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = cohort)) +
  geom_histogram(bins = 40, position = "dodge", colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "GDD above 5 until Stage 2 for Q. petraea",
       subtitle = "split by Site, coloured by Cohort",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Cohort") +
  xlim(100, 400) +
  ylim(0, 0.1)

gdd_above_5_petraea_by_site_and_cohort
ggsave(filename = "gdd_above_5_petraea_by_site_and_cohort.png", 
       device = png, width = 5, height = 5,
       plot = gdd_above_5_petraea_by_site_and_cohort,
       path = "output/figs")




#### Pubescens ####
### GDD above 5 for Q.pubescens by site

# all sites for pubescens 
gdd_above_5_pubescens_by_site <- stage_2_for_analysis %>% 
  filter(species == "Q.pubescens") %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = site_name)) +
  geom_histogram(bins = 40, colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "GDD above 5 until Stage 2 for Q. pubescens",
      subtitle = "split by Site, all Cohorts",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Site name") +
  xlim(100, 400) +
  theme(legend.position = "none")
  
  
gdd_above_5_pubescens_by_site

ggsave(filename = "gdd_above_5_pubescens_by_site.png", 
       device = png, width = 5,
       plot = gdd_above_5_pubescens_by_site, 
       path = "output/figs")


# all sites for pubescens, by site and cohort 
gdd_above_5_pubescens_by_site_and_cohort <- stage_2_for_analysis %>% 
  filter(species == "Q.pubescens") %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = cohort, alpha = 0.7)) +
  geom_histogram(bins = 40, position = "dodge", colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = my_pal[c(1,7)]) +
  labs(title = "GDD above 5 until Stage 2 for Q. pubescens", 
       subtitle = "split by Site and Cohort",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Cohort") +
  xlim(100, 400) +
  ylim(0, 0.1) +
  scale_alpha(guide = "none") +
  theme_bw()

gdd_above_5_pubescens_by_site_and_cohort

ggsave(filename = "gdd_above_5_pubescens_by_site_and_cohort.png", 
       device = png, width = 5, height = 8,
       plot = gdd_above_5_pubescens_by_site_and_cohort,
       path = "output/figs")

#### Robur ####
### GDD above 5 for Q. Robur by site

# all sites for robur 
gdd_above_5_robur_by_site <- stage_2_for_analysis %>% 
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = site_name)) +
  geom_histogram(bins = 40, colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "GDD above 5 until Stage 2 for Q. robur", 
       subtitle = "split by Site, all Cohorts",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Site name") +
  xlim(100, 400) +
  theme(legend.position = "none")


gdd_above_5_robur_by_site

ggsave(filename = "gdd_above_5_robur_by_site.png", 
       device = png, width = 5,
       plot = gdd_above_5_robur_by_site, 
       path = "output/figs")


# all sites for robur, by site and cohort 
gdd_above_5_robur_by_site_and_cohort <- stage_2_for_analysis %>% 
  filter(species == "Q.robur") %>%
  ggplot(mapping = aes(x = gdd_above_5, y = ..density..,
                       fill = cohort)) +
  geom_histogram(bins = 40, position = "dodge", colour = "black") +
  geom_density(alpha = 0.5) +
  facet_wrap(~site_name, ncol = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "GDD above 5 until Stage 2 for Q. robur", 
       subtitle = "split by Site and Cohort",
       x = "Growing Degree Days",
       y = "Frequency",
       fill = "Cohort") +
  xlim(100, 400) +
  ylim(0, 0.1)


gdd_above_5_robur_by_site_and_cohort
ggsave(filename = "gdd_above_5_robur_by_site_and_cohort.png", 
       device = png, width = 5, height = 10,
       plot = gdd_above_5_robur_by_site_and_cohort,
       path = "output/figs")












#### OTHER STUFF ####




### QQ Plot
qqnorm(stage_2_for_analysis$cum_temp_above_5)
qqline(stage_2_for_analysis$cum_temp_above_5)

### Automatic EDA by ggally
stage_2_for_analysis %>%
  select(cum_temp_above_5, species, altitude, latitude, longitude, age) %>%
  ggpairs(mapping = aes(color = species, alpha = 0.5))


### Rainclouds ####
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

stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  filter(cohort == "2023_3") %>%
  group_by(site_name) %>%
  summarise(mean_doy = mean(doy_stage_2))


4 Schönberg_am_Kamp     108.  2022 2        350
2 Bosco_Pantano         109.  2023 2  Csa   10
4 Schönberg_am_Kamp     111.  2023_3        350
1 Altenhof_am_Kamp      112.  2022 2        250  
2 Diendorf_am_Walde     115.  2022 2        350
3 Planck_am_Kamp        115.  2022 2        250
1 Altenhof_am_Kamp      116.  2023_3        250
10 Schönberg_am_Kamp    118.  2023 2        350
5 Groane                119.  2023 2  Cfa   250
8 Locarno               120.  2023 2        200
2 Diendorf_am_Walde     120.  2023_3        350
3 Planck_am_Kamp        120.  2023_3        250
1 Altenhof_am_Kamp      121.  2023 2        250
3 Cestas                122.  2023 2        50
4 Diendorf_am_Walde     122.  2023 2        350
6 Guca                  122.  2023 2        400
7 Laveyron              122.  2023 2  Cfa   150
9 Planck_am_Kamp        122.  2023 2        250


4 Schönberg_am_Kamp     108.  2022 2        350
4 Schönberg_am_Kamp     111.  2023_3        350
1 Altenhof_am_Kamp      112.  2022 2        250  
2 Diendorf_am_Walde     115.  2022 2        350
3 Planck_am_Kamp        115.  2022 2        250
1 Altenhof_am_Kamp      116.  2023_3        250
10 Schönberg_am_Kamp    118.  2023 2        350
2 Diendorf_am_Walde     120.  2023_3        350
3 Planck_am_Kamp        120.  2023_3        250
1 Altenhof_am_Kamp      121.  2023 2        250
4 Diendorf_am_Walde     122.  2023 2        350
9 Planck_am_Kamp        122.  2023 2        250


4 Schönberg_am_Kamp     108.  2022 2        350
1 Altenhof_am_Kamp      112.  2022 2        250  
2 Diendorf_am_Walde     115.  2022 2        350
3 Planck_am_Kamp        115.  2022 2        250

4 Schönberg_am_Kamp     111.  2023_3        350
1 Altenhof_am_Kamp      116.  2023_3        250
2 Diendorf_am_Walde     120.  2023_3        350
3 Planck_am_Kamp        120.  2023_3        250

10 Schönberg_am_Kamp    118.  2023 2        350
1 Altenhof_am_Kamp      121.  2023 2        250
4 Diendorf_am_Walde     122.  2023 2        350
9 Planck_am_Kamp        122.  2023 2        250


stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  filter(cohort == "2023_3") %>%
  group_by(site_name) %>%
  summarise(mean_gdd = mean(gdd_above_5))

10 Schönberg_am_Kamp            198.
1 Altenhof_am_Kamp             212.
9 Planck_am_Kamp               226.
4 Diendorf_am_Walde            230.

4 Schönberg_am_Kamp     171.
1 Altenhof_am_Kamp      188.
3 Planck_am_Kamp        208.
2 Diendorf_am_Walde     214.


DOY
2 Bosco_Pantano         109.  2023 2  Csa   10
5 Groane                119.  2023 2  Cfa   250
8 Locarno               120.  2023 2        200
3 Cestas                122.  2023 2        50
6 Guca                  122.  2023 2        400
7 Laveyron              122.  2023 2  Cfa   150

stage_2_for_analysis %>%
  filter(species == "Q.robur") %>%
  filter(cohort == "2023_2") %>%
  group_by(site_name) %>%
  summarise(mean_gdd = mean(gdd_above_5))

2 Bosco_Pantano                164.
5 Groane                       202.
8 Locarno                      212.
3 Cestas                       224.
6 Guca                         224.
7 Laveyron(Tarbes/landouc)     224.
