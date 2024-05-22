## ---------------------------
##
## Script name: Retrieving_IDs
##
## Purpose of script: Retrieving all sample IDs that have been registered.
##
## Author: Ruby Rose Bader
##
## Date Created: 2024-04-04
##
## Copyright (c) Ruby Bader, 2024
## Email: ruby.bader@me.com
##
## ---------------------------
## Notes: This requires access to the hard drive that stores all registered data.
## And the path has to be manually changed to each consecutive time period inorder to read the samples.
## ---------------------------

library(readxl)
library("xlsx")
library(lubridate)
library(skimr)
library(reshape2)
library(tidyverse)


# Tank Sample IDs ----

directory_path <- "/Volumes/T7 Shield/CORAL_T3"

# List all files in the last folder of the directory
file_list <- list.files(directory_path, recursive = FALSE)
# Print the list of file names
print(file_list)

# Convert the list into a data frame
file_df <- data.frame(sample_ID = file_list)
write.csv(file_df,"/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Dissertation/TANK_PARAMETERS/Sample_Key(T3).csv")

# Generate a list of 10 random samples at T0 --> for threshold caluclation in ImageJ

tank_samples <- read_excel("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Dissertation/TANK_PARAMETERS/Tank-Parameters.xlsx", sheet = "Tank_Samples")

(sample_ids <- sample(tank_samples$Sample_ID, 10))

# Creating List of Registered Samples per T ----

# Path to the directory containing folders
path <- "/Volumes/T7 Shield"

# List all directories within the specified path
all_directories <- list.dirs(path, recursive = FALSE, full.names = TRUE)
# Filter directories ending with "_Registered"
target_directories <- all_directories[grep("_Registered$", all_directories)]

# Initialize an empty data frame to store results
file_list <- data.frame(Time_Period = character(), Sample_ID = character(), stringsAsFactors = FALSE)

# Loop over each directory
for (dir in target_directories) {
  # List all files within the directory
  files <- list.files(dir, full.names = TRUE)
  
  # Extract file names
  file_names <- basename(files)
  
  # Create a data frame with directory and file names
  dir_df <- data.frame(Time_Period = basename(dir), Sample_ID = file_names, stringsAsFactors = FALSE)
  
  # Append to the file_list data frame
  file_list <- bind_rows(file_list, dir_df)
  
  file_list <- file_list |> 
    mutate(Time_Period = gsub("_Registered", "", Time_Period)) |>
    mutate(Sample_ID = gsub("_", "-", Sample_ID)) |>
    mutate(Sample_ID = gsub("SAMPLE-", "", Sample_ID)) |>
    mutate(Sample_ID = gsub("-T[0-9]", "", Sample_ID)) |> 
    filter(!grepl("\\.zip", Sample_ID))
}

# Filter out rows where 'Time_Period' contains 'T0'
sample_ids_with_T0 <- file_list %>%
  filter(grepl("T0", Time_Period)) %>%
  select(Sample_ID)





