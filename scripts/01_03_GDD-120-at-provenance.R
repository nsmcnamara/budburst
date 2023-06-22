### GDD-5 in Spring at Provenance Sites
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-22
### Simone McNamara

#### Info ####
# info about chelsa climate variables
# https://chelsa-climate.org/bioclim/
# get the download link with website https://envicloud.wsl.ch/#/?bucket=https%3A%2F%2Fos.zhdk.cloud.switch.ch%2Fenvicloud%2F&prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2Fclimatologies%2F1981-2010%2F
# download in terminal with "wget LINK"
# make sure you are downloading the correct month or "mean"

#### Setup ####
# libraries
library(tidyverse)
library(sf)
library(raster)


#### Data Import ####
# get coordinates of provenance sites (id, long, lat)
df_mother_info <- read.csv("~/budburst/data/processed/mother-info.csv") 

summary(df_mother_info)
sapply(df_mother_info, function(x) sum(is.na(x)))

# get unique coordinates
unique_coordinates <- df_mother_info %>%
  distinct(longitude, latitude, .keep_all = FALSE)

# Kelvin to °C
kelvin <- 273.15

# base temperature
tbase <- 5

# get bioclim data
# 152 files following the pattern /Users/simonemcnamara/budburst/data/ISIMIP3a/CHELSA-W5E5v1.0/chelsa-w5e5v1.0_obsclim_tas_1800arcsec_global_daily_201003.nc
# the 152 files cover the years 1979 - 2016, and for each year the months January until April
# each file has daily mean temperatures for each day of the month
# the temperatures are recorded in Kelvin

# do it for one file
files <- list.files(path = "data/ISIMIP3a/CHELSA-W5E5v1.0", full.names = TRUE)
stack <- stack(files[149]) 

# extract values for coordinates
temp_month <- extract(stack, unique_coordinates)

# convert to °C
temp_month_celsius <- temp_month - kelvin

# set to 0 if below 5
temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius)

# cumulate for month
temp_month_base_cum <- t(apply(temp_month_base, 1, cumsum))

# create a df that has an entry with growing degree days from january until april for each year in the dataset (1979 - 2016)
# where growing degree days is the cumulative temperature above 5°C, and temperatures below that are discarded and not considered when calculating the growing degree days





combinePointValue=cbind(pointCoordinates,rasValue)




### later
files <- list.files(path = "data/ISIMIP3a/CHELSA-W5E5v1.0", full.names = TRUE)




