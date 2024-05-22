# Load required libraries
library(stringr)
library(dplyr)

# Changing file names ---

# Set working directory to the main folder
setwd("/Volumes/T7 Shield/T2_Registered")

# List all directories within T3_Registered folder
directories <- list.dirs()

# Iterate over each directory
for (dir in directories) {
  # Set working directory to the current directory
  setwd(dir)
  
  print(dir)
  
  # Check if "_T" files exist in the current directory
  resampled_files <- list.files(pattern = "\\.view")
  
  print(resampled_files)
  
  if (length(resampled_files) > 0) {
    # Rename files
    new_names <- str_remove(resampled_files, pattern = "\\.view")
    file.rename(resampled_files, new_names)
  }
  
  setwd("/Volumes/T7 Shield/T2_Registered")
}


# oops renamed T0 directory 

# Iterate over each directory and rename it
for (dir in directories) {
  # Extract the directory name
  dir_name <- basename(dir)
  
  # New directory name with "SAMPLE-" prefix
  new_dir_name <- paste0("SAMPLE-", dir_name)
  
  # Construct the new directory path
  new_dir_path <- file.path(dirname(dir), new_dir_name)
  
  # Rename the directory
  file.rename(dir, new_dir_path)
  
  # Print the renaming information
  cat("Renamed", dir, "to", new_dir_path, "\n")
}

# Function: extract ps from .info file ----
extract_ps_from_info <- function(info_file_path) {
  # Read lines from the .info file
  lines <- readLines(info_file_path)
  
  # Extract the third line
  third_line <- lines[3]
  
  # Extract substring from the 10th to 20th character
  ps <- substr(third_line, start = 10, stop = 30)
  
  return(ps)
}

# Path to the directory containing the directories to process ----
setwd("/Volumes/T7 Shield/T0_Registered")

base_directory <- "/Volumes/T7 Shield/T0_Registered"

# List all directories within the specified base directory
directories <- list.dirs(base_directory, recursive = FALSE)

data_list <- list()

# Iterate over each directory
for (dir in directories) {
  # Construct the path to the info file in the current directory
  info_path <- substr(dir, start = 41 , stop = 46)
  info_file_path <- file.path(dir, paste0(info_path, ".info"), fsep = "/")
  
  # Check if the info file exists
  if (file.exists(info_file_path)) {
    # Extract PS values from the info file
    ps <- extract_ps_from_info(info_file_path)
    
    # Remove spaces and split the string into individual characters
    ps_num <- na.omit(as.numeric(strsplit(ps, "\\s+")[[1]]))
    
    # Check if there are exactly two numbers
    if (length(ps_num) == 2) {
      # Convert the numbers to numeric
      ps1 <- as.numeric(ps_num[1])
      ps2 <- as.numeric(ps_num[2])
      
      
      # Create a data frame including PS1, PS2, and SampleNumber columns
      data <- data.frame(PS1 = ps1, PS2 = ps2, SampleNumber = info_path)
      
      # Append the data frame to the list
      data_list[[length(data_list) + 1]] <- data
    } else {
      cat("Error: Input format is not correct for directory:", dir, "\n")
    }
  }else {
    cat("Error: Info file not found for directory:", dir, "\n")
  }
}

# Combine all data frames into a single data frame
T0_combined_data <- do.call(rbind, data_list)

# Combine all data objects using rbind()
all_ps <- rbind(T4_combined_data, T3_combined_data, T2_combined_data, T1_combined_data, T0_combined_data)

# Write data to a CSV file
write.csv(all_ps, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Python-scripts-microCT-analysis/ps_values.csv", row.names = FALSE)


# Inspection of pixel data sheet ----

filtered <- T0_combined_data |>
  filter(PS1 >= 1) |> 
  filter(PS2 >= 1)


filtered <- all_ps %>%
  filter(grepl("^003", SampleNumber))

all_ps <- all_ps |>
  mutate(FragmentType = ifelse(grepl("^(042|032|033|028|029|013)", SampleNumber), "Large", "Small"))


# Overwriting .info files ----

# Read the content of the .info file
file_path <- "path_to_your_file.info"  # Replace "path_to_your_file.info" with the actual file path
file_content <- readLines(file_path)

# Modify the third line
new_third_line <- 'pixelsize 10.0 10.0'  # Replace this with the new content for the third line

# Replace the third line in the content
file_content[3] <- new_third_line

# Write the modified content back to the file
writeLines(file_content, file_path)


