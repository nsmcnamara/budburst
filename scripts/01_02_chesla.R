
library(sf)

library(raster)

coordinates.testing <- read.delim("/Users/simonemcnamara/Desktop/UZH/MSc Thesis/30 Data Analysis/mother.coords.no.spp.txt") ## your coordinate file (ID Long Lat )

rasStack = stack("/Users/simonemcnamara/CHELSA_gdgfgd5_1981-2010_V.2.1.tif") ## whatever file to add more files you'd seperate the file names with a comma

pointCoordinates=coordinates.testing

coordinates(pointCoordinates)= ~ Long + Lat ## match with column headers

rasValue=extract(rasStack, pointCoordinates)
#years <- 2010-1980
#rasValue_pa = rasValue/years # calculate gdd per year

combinePointValue=cbind(pointCoordinates,rasValue)

write.table(combinePointValue,file="~/combinedPointValue.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE) ## table
