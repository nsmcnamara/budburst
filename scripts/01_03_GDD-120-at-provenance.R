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
  distinct(site_name, longitude, latitude, .keep_all = FALSE)

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
#files <- list.files(path = "data/ISIMIP3a/CHELSA-W5E5v1.0", full.names = TRUE)
#stack <- stack(files[147]) 

# extract values for coordinates
#temp_month <- extract(stack, unique_coordinates[2:3])

# convert to °C
#temp_month_celsius <- temp_month - kelvin

# set to 0 if below 5
#temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius)

# cumulate for month
#temp_month_base_cum <- t(apply(temp_month_base, 1, cumsum))


# do it for one year

# Define the base directory and file pattern
base_dir <- "data/ISIMIP3a/CHELSA-W5E5v1.0/"
file_pattern <- "chelsa-w5e5v1.0_obsclim_tas_1800arcsec_global_daily_2016%02d.nc"

# Define the start and end indices based on file endings
start_index <- 1  # Assuming the first file ends with "01"
end_index <- 4    # Assuming the last file ends with "04"

# Create an empty data frame
cumulative_temps_df <- data.frame(unique_coordinates)

# Initialize a vector to store the cumulative sum values
total_values <- numeric(nrow(unique_coordinates))


# Loop through the files
for (i in start_index:end_index) {
  # Construct the file path
  file_path <- list.files(path = base_dir, pattern = sprintf(file_pattern, i), full.names = TRUE)
  
  # Extract the year from the file name
  year <- substr(file_path, nchar(file_path) - 8, nchar(file_path) - 5)
  
  # Read the temperature data from the file
  stack <- stack(file_path)
  
  # Extract values for coordinates
  temp_month <- extract(stack, unique_coordinates[2:3])
  
  # Convert to °C
  temp_month_celsius <- temp_month - kelvin
  
  # Set to 0 if below 5, otherwise subtract 5 to get gdd_5
  temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius - tbase)
  
  # Calculate the cumulative sum for each month
  temp_month_base_cum <- t(apply(temp_month_base, 1, cumsum))
  
  last_column <- temp_month_base_cum[, ncol(temp_month_base_cum)]
  
  # Accumulate the values for each month
  total_values <- total_values + temp_month_base_cum[, ncol(temp_month_base_cum)]
}

# Create a data frame with the "Total" column
cumulative_temps_df[[year]] <- total_values



# create a df that has an entry with growing degree days from january until april for each year in the dataset (1979 - 2016)
# where growing degree days is the cumulative temperature above 5°C, and temperatures below that are discarded and not considered when calculating the growing degree days



# Define the base directory and file pattern
base_dir <- "data/ISIMIP3a/CHELSA-W5E5v1.0/"
file_pattern <- "chelsa-w5e5v1.0_obsclim_tas_1800arcsec_global_daily_%s%02d.nc"

# Define the start and end years
start_year <- 1979
end_year <- 2016

# Create an empty data frame
cumulative_temps_df <- data.frame(unique_coordinates)

# Loop through the years
for (year in start_year:end_year) {
  # Initialize a vector to store the cumulative sum values
  total_values <- numeric(nrow(unique_coordinates))
  
  # Loop through the months
  for (month in 1:4) {
    # Construct the file path
    file_path <- sprintf(file_pattern, year, month)
    
    # Read the temperature data from the file
    stack <- stack(file.path(base_dir, file_path))
    
    # Extract values for coordinates
    temp_month <- extract(stack, unique_coordinates[2:3])
    
    # Convert to °C
    temp_month_celsius <- temp_month - kelvin
    
    # Set to 0 if below 5, otherwise subtract 5 to get gdd_5
    temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius - tbase)
    
    # Calculate the cumulative sum for each month
    temp_month_base_cum <- t(apply(temp_month_base, 1, cumsum))
    
    last_column <- temp_month_base_cum[, ncol(temp_month_base_cum)]
    
    # Accumulate the values for each month
    total_values <- total_values + last_column
  }
  
  # Create a column name using the year
  column_name <- as.character(year)
  
  # Add the accumulated sum values as a new column in the data frame
  cumulative_temps_df[[column_name]] <- total_values
}






# Create an empty data frame
cumulative_temps_jan_mar_df <- data.frame(unique_coordinates)

# Loop through the years
for (year in start_year:end_year) {
  # Initialize a vector to store the cumulative sum values
  total_values <- numeric(nrow(unique_coordinates))
  
  # Loop through the months january till march
  for (month in 1:3) {
    # Construct the file path
    file_path <- sprintf(file_pattern, year, month)
    
    # Read the temperature data from the file
    stack <- stack(file.path(base_dir, file_path))
    
    # Extract values for coordinates
    temp_month <- extract(stack, unique_coordinates[2:3])
    
    # Convert to °C
    temp_month_celsius <- temp_month - kelvin
    
    # Set to 0 if below 5, otherwise subtract 5 to get gdd_5
    temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius - tbase)
    
    # Calculate the cumulative sum for each month
    temp_month_base_cum <- t(apply(temp_month_base, 1, cumsum))
    
    last_column <- temp_month_base_cum[, ncol(temp_month_base_cum)]
    
    # Accumulate the values for each month
    total_values <- total_values + last_column
  }
  
  # Create a column name using the year
  column_name <- as.character(year)
  
  # Add the accumulated sum values as a new column in the data frame
  cumulative_temps_df[[column_name]] <- total_values
}


