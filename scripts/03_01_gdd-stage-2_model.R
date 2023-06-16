### Model for budburst
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-16
### Simone McNamara


#### Setup ####
# libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)


#### Data Import ####
stage_2_for_analysis <- read.csv("~/budburst/data/processed/stage_2_for_analysis.csv", stringsAsFactors=TRUE)

glimpse(stage_2_for_analysis)
head(stage_2_for_analysis)
summary(stage_2_for_analysis)
str(stage_2_for_analysis)

### check NAs
sapply(stage_2_for_analysis, function(x) sum(is.na(x)))


#### LMM Stage 2 ####
# resp ~ FEexpr + (REexpr1 | factor1) + (REexpr2 | factor2) + ...
# resp = gdd_above_5
# fixed effects: 
  # photoperiod (latitude)
  # temperature at provenance 
  # precipitation at provenance
# random effects:
  # mother_id 


# LMM Robur #
# latitude only
m_gdd_s2_rob <- lmer(gdd_above_5 ~ latitude + (1 | mother_id), 
                     data = stage_2_for_analysis, subset = species == "Q.robur")

# summary / anova
summary(m_gdd_s2_rob)
anova(m_gdd_s2_rob, type = "3")
# latitude not significant
# but mother_id explains a fair bit, but a fair bit of variance unexplained






### check model assumptions
### Tukey Anscombe
plot(model_growing_degree_days) # welll.... 

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

