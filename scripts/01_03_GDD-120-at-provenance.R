### GDD-5 at DOY 120 at Provenance Sites
### This script is part of the ACORN budburst analysis project
### Last update:  2023-07-06
### Simone McNamara


#### Setup ####
library(tidyverse)
library(conflicted)

conflicts_prefer(dplyr::filter)


#### Data Import ####

# set up base directory and file pattern to import entire folder
base_dir <- "/Users/simonemcnamara/budburst/data/chelsa/"
file_pattern <- "chelsa_extracted_\\d{4}-\\d{2}-\\d{2}_121.txt"

# Get the list of file paths
file_paths <- list.files(path = base_dir, pattern = file_pattern, full.names = TRUE)

# Create an empty list to store the data frames
data_list <- list()

# Create empty vector with years
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

# make vector with column names
col_names <- c("id", "tas", "tasmax", "tasmin", "pr", "rsds", "day", "month", "year", "lat", "lon", "site_name")

# replace col_names
data_list <- lapply(data_list, function(df) {
  colnames(df) <- col_names
  return(df)
})

# tas is in kelvin *10, so to get gdd above 5°C need to transform
# variables
# Kelvin to °C
kelvin <- 273.15
# base temperature
tbase <- 5

# Make transformation a function
tas_to_gdd_5 <- function(df) {
  # calculate gdd_5
  df <- df %>%
    mutate(tas_C = (tas/10) - kelvin) %>%
    mutate(gdd_5 = if_else(tas_C >= tbase, tas_C - tbase, 0))
  
  # calculate cumulative
  df <- df %>%
    group_by(site_name, year) %>%
    arrange(month, day) %>%
    mutate(cum_gdd_5 = cumsum(gdd_5)) %>%
    summarise(gdd_120 = max(cum_gdd_5))
  
  # extract year and make this the column name
  df <- df %>%
    rename_with(~ as.character(df$year[1]), .cols = gdd_120) %>%
    select(!year)
  
  return(df)
}

# Apply the code to each data frame in data_list
data_list <- lapply(data_list, tas_to_gdd_5)


# Make one single data frame with the entries
# Extract the site names from the first data frame
site_names <- data_list[[1]]$site_name

# Create a data frame with the site names as the first column
df_gdd_120_1980_2019 <- data.frame(site_name = site_names)

# Bind the columns from each data frame to the combined data frame
for (df in data_list) {
  combined_df <- bind_cols(combined_df, df[, -1])
}



