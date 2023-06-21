### GDD-5 in Spring at Provenance Sites
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-21
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

coordinates <- df_mother_info %>%
  dplyr::select(mother_id, longitude, latitude) %>%
  rename(Long = longitude) %>%
  rename(Lat = latitude)


## whatever file to add more files you'd seperate the file names with a comma
rasStack = stack("~/budburst/data/raw/CHELSA_tas_01_1980_V.2.1.tif") 


pointCoordinates=coordinates

## match with column headers
coordinates(pointCoordinates)= ~ Long + Lat 

rasValue=extract(rasStack, pointCoordinates)

combinePointValue=cbind(pointCoordinates,rasValue)

## table
write.table(combinePointValue,file="~/budburst/data/processed/tas.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

