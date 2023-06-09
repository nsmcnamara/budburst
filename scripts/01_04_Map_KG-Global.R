###########################################################################################
##
## R source code to read and visualize Köppen-Geiger fields (Version of 19 August 2022)                                                                                    
##
## Climate classification after Kottek et al. (2006), downscaling after Rubel et al. (2017)
##
## Kottek, M., J. Grieser, C. Beck, B. Rudolf, and F. Rubel, 2006: World Map of the  
## Köppen-Geiger climate classification updated. Meteorol. Z., 15, 259-263.
##
## Rubel, F., K. Brugger, K. Haslinger, and I. Auer, 2017: The climate of the 
## European Alps: Shift of very high resolution Köppen-Geiger climate zones 1800-2100. 
## Meteorol. Z., DOI 10.1127/metz/2016/0816.
##
## (C) Climate Change & Infectious Diseases Group, University of Veterinary Medicine Vienna
##     Vetmeduni Vienna, Austria
##
###########################################################################################

# required packages 
library(raster); library(rasterVis); library(rworldxtra); data(countriesHigh)
library(latticeExtra)


# Read raster files
period='1986-2010'
r <- raster(paste('KG_', period, '.grd', sep=''))

# Color palette for climate classification
climate.colors=c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", "#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64", "#007800", "#005000", "#003200", "#96FF00", "#00D700", "#00AA00", "#BEBE00", "#8C8C00", "#5A5A00", "#550055", "#820082", "#C800C8", "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6", "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF", "#6496FF", "#64FFFF", "#F5FFFF")

# Legend must correspond to all climate classes, insert placeholders
r0 <- r[1:32]; r[1:32] <- seq(1,32,1)

# Converts raster field to categorical data
r <- ratify(r); rat <- levels(r)[[1]]

# Legend is always drawn in alphabetic order
rat$climate <- c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk', 'BWh', 'BWk', 'Cfa', 'Cfb','Cfc', 'Csa', 'Csb', 'Csc', 'Cwa','Cwb', 'Cwc', 'Dfa', 'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc', 'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET', 'Ocean')

# Remove the placeholders
r[1:32] <- r0; levels(r) <- rat

# Select region (Australia)
# x1=80; x2=180; y1=-50; y2=20; xat=5; yat=5	
# Select region (Europe)
 x1=-20; x2=80; y1=30; y2=75; xat=10; yat=5		
# Select region (US)
# x1=-130; x2=-60; y1=20; y2=60; xat=5; yat=5
# Select region (Global)
#  x1=-180; x2=180; y1=-90; y2=90; xat=20; yat=10

r <- crop(r, extent(x1, x2, y1, y2))

# Visualization		
if(.Platform$OS.type=="windows") {quartz<-function(...) windows(...)}
quartz(width=13, height=10, dpi=100)

print(levelplot(r, col.regions=climate.colors, xlab="", ylab="", 
		scales=list(x=list(limits=c(xmin(r), xmax(r)), at=seq(xmin(r), xmax(r), xat)), 
		y=list(limits=c(ymin(r), ymax(r)), at=seq(ymin(r), ymax(r), yat))), colorkey=list(space="top", tck=0, maxpixels=ncell(r)))
		+latticeExtra::layer(sp.polygons(countriesHigh, lwd=0.25)))

out=paste('KG_', period,'_5m.pdf', sep='')
dev.copy2pdf(file=out)

# Find the climate class for Vienna, Europe (or another location)
lon=15.6930000; lat=48.54420
KG=r[cellFromXY(r, c(lon, lat))]
print(KG)

df_site_name_unique_pet <- df_site_name_unique_pet %>%
  relocate(longitude, .before = latitude)
long_lat_pet <- df_site_name_unique_pet %>%
  dplyr::select(longitude, latitude) %>%
  rename(lon = longitude) %>%
  rename(lat = latitude)

long_lat_pet %>% 
  mutate(KG = r[cellFromXY(r, c(lon, lat))])

KG_values_pet <- numeric(length(df_site_name_unique_pet$site_name))
for (i in seq_along(df_site_name_unique_pet$site_name)) {
  lon <- df_site_name_unique_pet$longitude[i]
  lat <- df_site_name_unique_pet$latitude[i]
  KG <- r[cellFromXY(r, c(lon, lat))]
  KG_values[i] <- KG
}
KG_values_pub <- numeric(length(df_site_name_unique_pub$site_name))
for (i in seq_along(df_site_name_unique_pub$site_name)) {
  lon <- df_site_name_unique_pub$longitude[i]
  lat <- df_site_name_unique_pub$latitude[i]
  KG <- r[cellFromXY(r, c(lon, lat))]
  KG_values_pub[i] <- KG
}
KG_values_robur <- numeric(length(df_site_name_unique_robur$site_name))
for (i in seq_along(df_site_name_unique_robur$site_name)) {
  lon <- df_site_name_unique_robur$longitude[i]
  lat <- df_site_name_unique_robur$latitude[i]
  KG <- r[cellFromXY(r, c(lon, lat))]
  KG_values_robur[i] <- KG
}

KG_robur <- cbind(df_site_name_unique_robur, KG_values_robur)
KG_robur <- KG_robur %>%
  rename(ID = KG_values_robur)
KG_robur <- left_join(KG_robur, rat, by = "ID")

# Output of ASCCI-Data (numbers correspond to the climate classes of the legend)
r <- crop(r, extent(x1, x1+100, y1, y1+100)) # extent must be within the selected region
z <- rasterToPoints(r, spatial=T); z <- spTransform(z, CRS=projection(r))
z <- as.data.frame(z); print(length(t(z[1]))); z = subset(z, z[1]!=32); print(length(t(z[1])))
names(z)=c('KG', 'lon', 'lat')
pts <- data.frame(lat=format(z[2], digits=4), lon=format(z[3], digits=7), KG=format(z[1], digits=3))
write.csv(pts, file=paste('KG_', period,'_5m.csv', sep=''), row.names=F)
