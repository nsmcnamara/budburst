### GDD-5 in Spring at Provenance Sites
### This script is part of the ACORN budburst analysis project
### Last update:  2023-06-21
### Simone McNamara

#### Setup ####
# libraries
library(tidyverse)
library(sf)
library(raster)



# info about chelsa climate variables
# https://chelsa-climate.org/bioclim/
# get the download link with website https://envicloud.wsl.ch/#/?bucket=https%3A%2F%2Fos.zhdk.cloud.switch.ch%2Fenvicloud%2F&prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2Fclimatologies%2F1981-2010%2F
# download in terminal with "wget LINK"
# make sure you are downloading the correct month or "mean"

## your coordinate file (ID Long Lat )
coordinates.testing <- read.csv("~/budburst/data/processed/mother-info.csv") 
coordinates <- coordinates.testing %>%
  dplyr::select(mother_id, longitude, latitude) %>%
  rename(Long = longitude) %>%
  rename(Lat = latitude)

## whatever file to add more files you'd seperate the file names with a comma
rasStack = stack("~/budburst/data/raw/CHELSA_bio1_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_bio12_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_bio15_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_fcf_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_gdd5_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_gdd10_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_gddlgd10_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_gdgfgd5_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_gdgfgd10_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_gsl_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_gsp_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_gst_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_kg2_1981-2010_V.2.1.tif",
                 "~/budburst/data/raw/CHELSA_ngd5_1981-2010_V.2.1.tif"
) 


pointCoordinates=coordinates

## match with column headers
coordinates(pointCoordinates)= ~ Long + Lat 

rasValue=extract(rasStack, pointCoordinates)

combinePointValue=cbind(pointCoordinates,rasValue)

## table
write.table(combinePointValue,file="~/budburst/data/processed/tas.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

