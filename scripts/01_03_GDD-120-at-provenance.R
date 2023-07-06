### GDD-5 in Spring at Provenance Sites
### This script is part of the ACORN budburst analysis project
### Last update:  2023-07-05
### Simone McNamara


#### Setup ####
library(tidyverse)
library(conflicted)

conflicts_prefer(dplyr::filter)


#### Data Import ####

# set up to import entire folder
base_dir <- "/Users/simonemcnamara/budburst/data/chelsa/"
file_pattern <- "chelsa_extracted_\\d{4}-\\d{2}-\\d{2}_121.txt"

# Get the list of file paths
file_paths <- list.files(path = base_dir, pattern = file_pattern, full.names = TRUE)

# Create an empty list to store the data frames
data_list <- list()

# create empty vector with years
years <- vector()

# Iterate over each file path and read into a data frame
for (file_path in file_paths) {
  # Extract the year from the file name
  year <- substr(file_path, nchar(file_path) - 17, nchar(file_path) - 14)
  # save in vector
  years[[year]] <- year
  
  # Read the file into a data frame
  data <- read.table(file_path, quote="\"", comment.char="", stringsAsFactors=TRUE)
  
  # Store the data frame in the list with the year as the name
  data_list[[year]] <- data
}



#### Data Wrangling ####
# make vector with years


# make vector with column names
col_names <- c("id", "tas", "tasmax", "tasmin", "pr", "rsds", "day", "month", "year", "lat", "lon", "site_name")
# apply column dames to data frame
colnames(chelsa_extracted_1999.12.31_121) <- col_names

# Iterate over each file path and make readable
for (file_path in file_paths) {
  # Extract the year from the file name
  year <- substr(file_path, nchar(file_path) - 17, nchar(file_path) - 14)
  
  # Read the file into a data frame
  data <- read.table(file_path, quote="\"", comment.char="", stringsAsFactors=TRUE)
  
  # Store the data frame in the list with the year as the name
  data_list[[year]] <- data
}


# tas is in kelvin *10, so to get gdd above 5°C need to transform
# variables
# Kelvin to °C
kelvin <- 273.15
# base temperature
tbase <- 5

# calculate gdd_5
chelsa_extracted_1999.12.31_121 <- chelsa_extracted_1999.12.31_121 %>%
  mutate(tas_C = (tas/10) - kelvin) %>%
  mutate(gdd_5 = if_else(tas_C >= tbase, tas_C - tbase, 0))

# calculate cumulative
ch <- chelsa_extracted_1999.12.31_121 %>%
  group_by(site_name, year) %>%
  arrange(month, day) %>%
  mutate(cum_gdd_5 = cumsum(gdd_5)) %>%
  summarise(gdd_120 = max(cum_gdd_5))

# extract year and make this the column name
ch <- ch %>%
  rename_with(~as.character(ch$year[1]), .cols = gdd_120) %>%
  select(!year)









# get bioclim data
# 152 files following the pattern /Users/simonemcnamara/budburst/data/ISIMIP3a/CHELSA-W5E5v1.0/chelsa-w5e5v1.0_obsclim_tas_1800arcsec_global_daily_201003.nc
# the 152 files cover the years 1979 - 2016, and for each year the months January until April
# each file has daily mean temperatures for each day of the month
# the temperatures are recorded in Kelvin

# do it for one file
files <- list.files(path = "data/ISIMIP3a/CHELSA-W5E5v1.0", full.names = TRUE)
stack <- stack(files[49]) 

# do it for one year

# # Define the base directory and file pattern
# base_dir <- "data/ISIMIP3a/CHELSA-W5E5v1.0/"
# file_pattern <- "chelsa-w5e5v1.0_obsclim_tas_30arcsec_global_daily_1979%02d.nc"
# 
# # Define the start and end indices based on file endings
# start_index <- 1  # Assuming the first file ends with "01"
# end_index <- 4    # Assuming the last file ends with "04"
# 
# # Create an empty data frame
# cumulative_temps_df <- data.frame(unique_coordinates)
# 
# # Initialize a vector to store the cumulative sum values
# total_values <- numeric(nrow(unique_coordinates))
# 
# 
# # Loop through the files
# for (i in start_index:end_index) {
#   # Construct the file path
#   file_path <- list.files(path = base_dir, pattern = sprintf(file_pattern, i), full.names = TRUE)
# 
#   # Extract the year from the file name
#   year <- substr(file_path, nchar(file_path) - 8, nchar(file_path) - 5)
# 
#   # Read the temperature data from the file
#   stack <- stack(file_path)
# 
#   # Extract values for coordinates
#   temp_month <- extract(stack, unique_coordinates[2:3])
# 
#   # Convert to °C
#   temp_month_celsius <- temp_month - kelvin
# 
#   # Set to 0 if below 5, otherwise subtract 5 to get gdd_5
#   temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius - tbase)
# 
#   # Calculate the cumulative sum for each month
#   temp_month_base_cum <- t(apply(temp_month_base, 1, cumsum))
# 
#   last_column <- temp_month_base_cum[, ncol(temp_month_base_cum)]
# 
#   # Accumulate the values for each month
#   total_values <- total_values + temp_month_base_cum[, ncol(temp_month_base_cum)]
# }
# 
# # Create a data frame with the "Total" column
# cumulative_temps_df[[year]] <- total_values
# 
# 
# 
# # create a df that has an entry with growing degree days from january until april for each year in the dataset (1979 - 2016)
# # where growing degree days is the cumulative temperature above 5°C, and temperatures below that are discarded and not considered when calculating the growing degree days

# 
# 
# # Define the base directory and file pattern
# base_dir <- "data/ISIMIP3a/CHELSA-W5E5v1.0"
# file_pattern <- "chelsa-w5e5v1.0_obsclim_tas_30arcsec_global_daily_%s%02d.nc"
# 
# # Define the start and end years
# start_year <- 1979
# end_year <- 1979
# 
# # Create an empty data frame
# cumulative_temps_df <- data.frame(unique_coordinates)
# 
# # Loop through the years
# for (year in start_year:end_year) {
#   # Initialize a vector to store the cumulative sum values
#   total_values <- numeric(nrow(unique_coordinates))
#   
#   # Loop through the months
#   for (month in 1:4) {
#     # Construct the file path
#     file_path <- sprintf(file_pattern, year, month)
#     
#     # Read the temperature data from the file
#     stack <- stack(file.path(base_dir, file_path))
#     
#     # Extract values for coordinates
#     temp_month <- raster::extract(stack, unique_coordinates[2:3])
#     
#     # Convert to °C
#     temp_month_celsius <- temp_month - kelvin
#     
#     # Set to 0 if below 5, otherwise subtract 5 to get gdd_5
#     temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius - tbase)
#     
#     # Calculate the cumulative sum for each month
#     temp_month_base_cum <- t(apply(temp_month_base, 1, cumsum))
#     
#     last_column <- temp_month_base_cum[, ncol(temp_month_base_cum)]
#     
#     # Accumulate the values for each month
#     total_values <- total_values + last_column
#   }
#   
#   # Create a column name using the year
#   column_name <- as.character(year)
#   
#   # Add the accumulated sum values as a new column in the data frame
#   cumulative_temps_df[[column_name]] <- total_values
# }
# 


# 
# 
# ## Jan Mar ###
# # Create an empty data frame
# cumulative_temps_jan_mar_df <- data.frame(unique_coordinates)
# 
# # Loop through the years
# for (year in start_year:end_year) {
#   # Initialize a vector to store the cumulative sum values
#   total_values <- numeric(nrow(unique_coordinates))
#   
#   # Loop through the months january till march
#   for (month in 1:3) {
#     # Construct the file path
#     file_path <- sprintf(file_pattern, year, month)
#     
#     # Read the temperature data from the file
#     stack <- stack(file.path(base_dir, file_path))
#     
#     # Extract values for coordinates
#     temp_month <- extract(stack, unique_coordinates[2:3])
#     
#     # Convert to °C
#     temp_month_celsius <- temp_month - kelvin
#     
#     # Set to 0 if below 5, otherwise subtract 5 to get gdd_5
#     temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius - tbase)
#     
#     # Calculate the cumulative sum for each month
#     temp_month_base_cum <- t(apply(temp_month_base, 1, cumsum))
#     
#     last_column <- temp_month_base_cum[, ncol(temp_month_base_cum)]
#     
#     # Accumulate the values for each month
#     total_values <- total_values + last_column
#   }
#   
#   # Create a column name using the year
#   column_name <- as.character(year)
#   
#   # Add the accumulated sum values as a new column in the data frame
#   cumulative_temps_df[[column_name]] <- total_values
# }


# # slightly faster per year?
# # Preallocate output variables
# cumulative_temps_df <- list()
# total_values <- numeric(length(unique_coordinates[2:3]))
# 
# # Extract unique coordinates outside the loop
# coords <- unique_coordinates[2:3]
# 
# # Loop through the files
# for (i in start_index:end_index) {
#   # Construct the file path
#   file_path <- list.files(path = base_dir, pattern = sprintf(file_pattern, i), full.names = TRUE)
#   
#   # Extract the year from the file name
#   year <- substr(file_path, nchar(file_path) - 8, nchar(file_path) - 5)
#   
#   # Read the temperature data from the file
#   stack <- stack(file_path)
#   
#   # Extract values for coordinates
#   temp_month <- extract(stack, coords)
#   
#   # Convert to °C
#   temp_month_celsius <- temp_month - kelvin
#   
#   # Set to 0 if below 5, otherwise subtract 5 to get gdd_5
#   temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius - tbase)
#   
#   # Calculate the cumulative sum for each month
#   temp_month_base_cum <- cumsum(temp_month_base, 2)
#   
#   # Accumulate the values for each month
#   total_values <- total_values + temp_month_base_cum[, ncol(temp_month_base_cum)]
#   
#   # Store the results in the data frame
#   cumulative_temps_df[[year]] <- total_values
# }
# 
# 
# ########## without loop #########
# library(purrr)
# 
# # Construct a sequence of file indices
# file_indices <- seq(start_index, end_index)
# 
# # Function to process each file
# process_file <- function(i) {
#   # Construct the file path
#   file_path <- list.files(path = base_dir, pattern = sprintf(file_pattern, i), full.names = TRUE)
#   
#   # Extract the year from the file name
#   year <- substr(file_path, nchar(file_path) - 8, nchar(file_path) - 5)
#   
#   # Read the temperature data from the file
#   stack <- stack(file_path)
#   
#   # Extract values for coordinates
#   temp_month <- raster::extract(stack, unique_coordinates[2:3])
#   
#   # Convert to °C
#   temp_month_celsius <- temp_month - kelvin
#   
#   # Set to 0 if below 5, otherwise subtract 5 to get gdd_5
#   temp_month_base <- ifelse(temp_month_celsius < tbase, 0, temp_month_celsius - tbase)
#   
#   # Calculate the cumulative sum for each month
#   temp_month_base_cum <- t(apply(temp_month_base, 1, cumsum))
#   
#   # Get the last column
#   last_column <- temp_month_base_cum[, ncol(temp_month_base_cum)]
#   
#   return(last_column)
# }
# 
# # Apply the function to process each file and get the last column
# cumulative_columns <- map(file_indices, process_file)
# 
# # Combine the columns into a matrix
# cumulative_matrix <- do.call(cbind, cumulative_columns)
# 
# # Create a data frame with the years as columns
# cumulative_temps_df <- as.data.frame(cumulative_matrix)
# names(cumulative_temps_df) <- substr(file_indices, nchar(file_indices) - 8, nchar(file_indices) - 5)


