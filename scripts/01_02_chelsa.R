
library(tidyverse)
library(sf)

library(raster)

# info about chelsa climate variables
# https://chelsa-climate.org/bioclim/
# get the download link with website https://envicloud.wsl.ch/#/?bucket=https%3A%2F%2Fos.zhdk.cloud.switch.ch%2Fenvicloud%2F&prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2Fclimatologies%2F1981-2010%2F
# download in terminal with "wget LINK"
# make sure you are downloading the correct month or "mean"

coordinates.testing <- read.csv("~/budburst/data/processed/mother-info.csv") ## your coordinate file (ID Long Lat )
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

coordinates(pointCoordinates)= ~ Long + Lat ## match with column headers

rasValue=extract(rasStack, pointCoordinates)

combinePointValue=cbind(pointCoordinates,rasValue)

write.table(combinePointValue,file="~/budburst/data/processed/coordinates_chelsa_values.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE) ## table


### debbie's original code
# coordinates.testing <- read.delim("/Users/simonemcnamara/Desktop/UZH/MSc Thesis/30 Data Analysis/mother.coords.no.spp.txt") ## your coordinate file (ID Long Lat )
# 
# rasStack = stack("/Users/simonemcnamara/CHELSA_gdgfgd5_1981-2010_V.2.1.tif") ## whatever file to add more files you'd seperate the file names with a comma
# 
# pointCoordinates=coordinates.testing
# 
# coordinates(pointCoordinates)= ~ Long + Lat ## match with column headers
# 
# rasValue=extract(rasStack, pointCoordinates)
# #years <- 2010-1980
# #rasValue_pa = rasValue/years # calculate gdd per year
# 
# combinePointValue=cbind(pointCoordinates,rasValue)
# 
# write.table(combinePointValue,file="~/combinedPointValue.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE) ## table