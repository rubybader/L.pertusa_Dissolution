## ---------------------------
##
## Script name: Tank-Parameters
##
## Purpose of script: Making sense of the environmental tank parameters that have been continously samples over the duration of the study.
##
## Author: Ruby Rose Bader
##
## Date Created: 2024-03-15
##
## Copyright (c) Ruby Bader, 2024
## Email: ruby.bader@me.com
##
## ---------------------------
## Notes: 
## ---------------------------

install.packages("readxl")
install.packages("xlsx")
install.packages("lubridate")
install.packages("skimr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("tidyverse")
install.packages("cowplot")
install.packages("viridis")

library(readxl)
library("xlsx")
library(lubridate)
library(skimr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(cowplot)
library(viridis)


setwd("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS")

## Loading Parameter Data of Each Tank ----

excel_file <- "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Tank-Parameters.xlsx"

sheet_names <- excel_sheets(excel_file)

exclude_sheets <- c("Months", "Tank_Samples", "Tank_Key", "Scanning_Dates")
sheet_names <- sheet_names[!sheet_names %in% exclude_sheets]

# Initialize an empty list to store filtered data frames
filtered_data_list <- list()

# Loop through each sheet
for (sheet_name in sheet_names) {
  # Read data from the sheet
  sheet_data <- read_excel(excel_file, sheet = sheet_name)
  
  # Apply filtering and NA removal to the sheet data
  filtered_data <- sheet_data %>%
    select(Treatment, Time_Period, Date, Day, pHT, pH, Salinity, Temperature, Oxygen_mgL, Oxygen_per) %>%
    filter(!is.na(pH), !is.na(pHT), !is.na(Salinity), !is.na(Temperature), !is.na(Oxygen_mgL)) %>%  # Remove NA values
    mutate(Tank = sheet_name)
  
  # Store the filtered data in the list
  filtered_data_list[[sheet_name]] <- filtered_data
  
  # Assign the filtered data to an object named after the sheet name
  assign(sheet_name, filtered_data)
  
  # Print some information about the sheet
  cat("Sheet", sheet_name, "has been loaded, filtered, and NA values removed\n")
}

## Data Frame ----



control <- bind_rows(Tank1, Tank2, Tank3)
multi_hf <- bind_rows(Tank4, Tank5, Tank6)
ph1 <- bind_rows(Tank10, Tank11, Tank12)
ph2 <- bind_rows(Tank13, Tank14, Tank15)
ph3 <- bind_rows(Tank16, Tank17, Tank18)

# Combine all filtered data frames into a single data frame
combined_data <- bind_rows(Tank1, Tank2, Tank3, Tank4, Tank5, Tank6, Tank10, Tank11, Tank12, Tank13, Tank14, Tank15, Tank16, Tank17, Tank18)


# Saving 
write.csv(control, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Control_enviornmental-param_sampling.csv")
write.csv(multi_hf, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Triple_enviornmental-param_sampling.csv")
write.csv(ph1, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/ph1_enviornmental-param_sampling.csv")
write.csv(ph2, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/ph2_enviornmental-param_sampling.csv")
write.csv(ph3, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/ph3_enviornmental-param_sampling.csv")

write.csv(combined_data, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Combined_enviornmental-param_sampling.csv")

# Find min and max for graph axis
summary_data <- combined_data %>%
  summarize(across(c(pHT, Salinity, Temperature, Oxygen_per), list(min = min, max = max))) 


## Reading in data ----

# combined_data <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Combined_enviornmental-param_sampling.csv")
# control <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Control_enviornmental-param_sampling.csv")
# multi_hf <- read_csv( "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Triple_enviornmental-param_sampling.csv")
# ph1 <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/ph1_enviornmental-param_sampling.csv")
# ph2 <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/ph2_enviornmental-param_sampling.csv")
# ph3 <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/ph3_enviornmental-param_sampling.csv")


## Function: Graphing ---- 

# Function to create plots for each treatment
create_plots <- function(data) {
  
  # Define a custom theme
  custom_theme <- theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),  # White background
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          panel.border = element_blank(),     # Remove panel border
          axis.line = element_line(color = "black"), 
          text = element_text(size = 18))  # Set axis lines
  
  # Create plots for each variable with custom colors
  Temperature <- ggplot(data, aes(y = Temperature, x = Day)) +
    stat_summary(fun = "mean", geom = "line", color = viridis(4)[1]) +
    labs(#title = "Temperature", 
      y = "Mean Temperature (°C)") +  # Add units to y-axis label
    geom_vline(xintercept = c(65, 134, 183, 274, 358), linetype = "dashed", color = "grey") +  # Add vertical lines
    ylim(5,15)+
    custom_theme

  Oxygen <- ggplot(data, aes(y = Oxygen_per, x = Day)) +
    stat_summary(fun = "mean", geom = "line", color = viridis(4)[2]) +  # Plot mean oxygen percentage
    labs(#title = "Oxygen", 
      y = "Mean Oxygen (%)") +  # Add units to y-axis label
    geom_vline(xintercept = c(65, 134, 183, 274, 358), linetype = "dashed", color = "grey") +  # Add vertical lines
    ylim(30, 125) +  # Set the y-axis limits
    custom_theme
  
  
  pHT <- ggplot(data, aes(y = pHT, x = Day)) +
    stat_summary(fun = "mean", geom = "line", color = viridis(4)[3]) +
    labs(#title = "pHT", 
      y = "pH") +  # Add units to y-axis label
    geom_vline(xintercept = c(65, 134, 183, 274, 358), linetype = "dashed", color = "grey") +  # Add vertical lines
    ylim(5,10)+
    custom_theme
  
  Salinity <- ggplot(data, aes(y = Salinity, x = Day)) +
    stat_summary(fun = "mean", geom = "line", color = viridis(4)[4]) +
    labs(#title = "Salinity", 
      y = "Salinity (ppt)") +  # Add units to y-axis label
    geom_vline(xintercept = c(65, 134, 183, 274, 358), linetype = "dashed", color = "grey") +  # Add vertical lines
    ylim(30,40)+
    custom_theme
  
  # Store the plots in a list
  plot_list <- list(Temperature, Oxygen, pHT, Salinity)
  
  return(plot_list)
}

# Making Plots ----

control <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Control_enviornmental-param_sampling.csv")
multi_hf <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Triple_enviornmental-param_sampling.csv")
ph1 <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/ph1_enviornmental-param_sampling.csv")
ph2 <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/ph2_enviornmental-param_sampling.csv")
ph3 <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/ph3_enviornmental-param_sampling.csv")

combined <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Combined_enviornmental-param_sampling.csv")

# Define a list of data objects
treatment_types <- list(control, multi_hf, ph1, ph2, ph3)
treatment_names <- list("Mean Environmental Parameters for Control Treatment", 
                        "Mean Environmental Parameters for Multi_HF Treatment", 
                        "Mean Environmental Parameters for pH1 Treatment", 
                        "Mean Environmental Parameters for pH2 Treatment", 
                        "Mean Environmental Parameters for pH3 Treatment")

tank_types <- list(Tank1, Tank2, Tank3, Tank4, Tank5, Tank6, Tank10, Tank11, Tank12, Tank13, Tank14, Tank15, Tank16, Tank17, Tank18)
tank_names <- list("Mean Environmental Parameters of Tank 1", 
                   "Mean Environmental Parameters of Tank 2", 
                   "Mean Environmental Parameters of Tank 3", 
                   "Mean Environmental Parameters of Tank 4",
                   "Mean Environmental Parameters of Tank 5", 
                   "Mean Environmental Parameters of Tank 6", 
                   "Mean Environmental Parameters of Tank 10", 
                   "Mean Environmental Parameters of Tank 11", 
                   "Mean Environmental Parameters of Tank 12", 
                   "Mean Environmental Parameters of Tank 13", 
                   "Mean Environmental Parameters of Tank 14", 
                   "Mean Environmental Parameters of Tank 15", 
                   "Mean Environmental Parameters of Tank 16", 
                   "Mean Environmental Parameters of Tank 17", 
                   "Mean Environmental Parameters of Tank 18")
                   
# Create an empty list to store plot panels
tank_panels <- list()
treatment_panels <- list()

# Per Tank - Loop through each data object
for (i in seq_along(tank_types)) {
  # Apply create_plots function and store the plot list
  plot_list <- create_plots(tank_types[[i]])
  
  plot_panel <- plot_grid(plotlist = plot_list, nrow = 2, ncol = 2)
  
  # Adding white background to plot panel
  plot_panel <- plot_panel + theme(plot.background = element_rect(fill = "white"))
  
  # Add the current plot panel to the list
  tank_panels[[i]] <- plot_panel

}

# Per Treatment - Loop through each data object
for (i in seq_along(treatment_types)) {
  # Apply create_plots function and store the plot list
  plot_list2 <- create_plots(treatment_types[[i]])
  
  # Creating a new plot panel for the current treatment type
  plot_panel2 <- plot_grid(plotlist = plot_list2, nrow = 2, ncol = 2)
  
  # Adding white background to the new plot panel
  plot_panel2 <- plot_panel2 + theme(plot.background = element_rect(fill = "white"))
  
  # Add the current plot panel to the list
  treatment_panels[[i]] <- plot_panel2
}

## Saving Plots ----
# Assign names to plot panels
names(treatment_panels) <- c("control_panel", "multi_hf_panel", "ph1_panel", "ph2_panel", "ph3_panel")
names(tank_panels) <- c("Tank1", "Tank2", "Tank3", "Tank4", "Tank5", "Tank6", "Tank10", "Tank11", "Tank12", "Tank13", "Tank14", "Tank15", "Tank16", "Tank17", "Tank18")

# To display plots stored in a list in .rmd file. --> Appendix
# walk(plt_lst, print) 
# Define the directory path
treatment_directory <- "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Treatment/"
tank_directory <- "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Tank/"

# Save each plot
for (i in seq_along(treatment_panels)) {
  # Define the file path
  file_path <- paste0(treatment_directory, names(treatment_panels)[i], ".png")
  
  # Save the plot with error handling
  tryCatch({
    ggsave(file_path, plot = treatment_panels[[i]])
    print(paste("Plot saved:", file_path))
  }, error = function(e) {
    print(paste("Error saving plot:", e$message))
  })
}

for (i in seq_along(tank_panels)) {
  # Define the file path
  file_path <- paste0(tank_directory, names(tank_panels)[i], ".png")
  
  # Save the plot with error handling
  tryCatch({
    ggsave(file_path, plot = tank_panels[[i]])
    print(paste("Plot saved:", file_path))
  }, error = function(e) {
    print(paste("Error saving plot:", e$message))
  })
}

## Filtering -----

# Take mean of each variable per time period, but each tank is still present. 
# Enviornmental params data will be merged to dissolution data set via Tank

combined_data <- read.csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Combined_enviornmental-param_sampling.csv")

sum_tank_time <- combined_data %>%
  select(-Day, -Date, "X", "...1") %>%
  group_by(Tank, Treatment, Time_Period) %>%
  summarize(mean_pHT = mean(pHT, na.rm = TRUE),
            sd_pHT = sd(pHT, na.rm = TRUE),
            min_pHT = min(pHT, na.rm = TRUE),
            max_pHT = max(pHT, na.rm = TRUE),
            mean_Salinity = mean(Salinity, na.rm = TRUE),
            sd_Salinity = sd(Salinity, na.rm = TRUE),
            min_Salinity = min(Salinity, na.rm = TRUE),
            max_Salinity = max(Salinity, na.rm = TRUE),
            mean_Temperature = mean(Temperature, na.rm = TRUE),
            sd_Temperature = sd(Temperature, na.rm = TRUE),
            min_Temperature = min(Temperature, na.rm = TRUE),
            max_Temperature = max(Temperature, na.rm = TRUE),
            sd_Oxygen = sd(Oxygen_per, na.rm = TRUE),
            min_Oxygen = min(Oxygen_per, na.rm = TRUE),
            max_Oxygen = max(Oxygen_per, na.rm = TRUE), 
            mean_Oxygen = mean(Oxygen_per, na.rm = TRUE),
            mean_Oxygen_mgL = mean(Oxygen_mgL, na.rm = TRUE),
            sd_Oxygen_mgL = sd(Oxygen_mgL, na.rm = TRUE),
            min_Oxygen_mgL = min(Oxygen_mgL, na.rm = TRUE),
            max_Oxygen_mgL = max(Oxygen_mgL, na.rm = TRUE)) |>
  mutate(Tank = as.integer(gsub("Tank", "", Tank)), 
         Time = Time_Period) |>
  select(-Time_Period)

# This now has to be merged with dissolution and carbonate chemsitry sheet 

write.csv(sum_tank_time, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Envio-Param_mean_by_tank-treatment.csv")

## Summary stats ----

### Calculate mean, sd, min, max for each parameter - For each Treatments ----
treatment_sum_stats <- combined_data %>%
  select(-Tank, -Time_Period, -Day, -Date) %>%
  group_by(Treatment) %>%
  summarize(mean_pH = mean(pHT, na.rm = TRUE),
            sd_pH = sd(pHT, na.rm = TRUE),
            min_pH = min(pHT, na.rm = TRUE),
            max_pH = max(pHT, na.rm = TRUE),
            mean_Salinity = mean(Salinity, na.rm = TRUE),
            sd_Salinity = sd(Salinity, na.rm = TRUE),
            min_Salinity = min(Salinity, na.rm = TRUE),
            max_Salinity = max(Salinity, na.rm = TRUE),
            mean_Temperature = mean(Temperature, na.rm = TRUE),
            sd_Temperature = sd(Temperature, na.rm = TRUE),
            min_Temperature = min(Temperature, na.rm = TRUE),
            max_Temperature = max(Temperature, na.rm = TRUE),
            sd_Oxygen = sd(Oxygen_per, na.rm = TRUE),
            min_Oxygen = min(Oxygen_per, na.rm = TRUE),
            max_Oxygen = max(Oxygen_per, na.rm = TRUE), 
            mean_Oxygen_mgL = mean(Oxygen_mgL, na.rm = TRUE),
            sd_Oxygen_mgL = sd(Oxygen_mgL, na.rm = TRUE),
            min_Oxygen_mgL = min(Oxygen_mgL, na.rm = TRUE),
            max_Oxygen_mgL = max(Oxygen_mgL, na.rm = TRUE))

write.csv(treatment_sum_stats, paste(treatment_directory, "Summary_Stats_per_Tank.csv"))

### Per Tank - Calculate mean, sd, min, max for each parameter ----
tank_sum_stats <- combined_data %>%
  select(-Treatment, -Time_Period, -Day, -Date) %>%
  group_by(Tank) %>%
  summarize(mean_pHT = mean(pHT, na.rm = TRUE),
            sd_pHT = sd(pHT, na.rm = TRUE),
            min_pHT = min(pHT, na.rm = TRUE),
            max_pHT = max(pHT, na.rm = TRUE),
            mean_Salinity = mean(Salinity, na.rm = TRUE),
            sd_Salinity = sd(Salinity, na.rm = TRUE),
            min_Salinity = min(Salinity, na.rm = TRUE),
            max_Salinity = max(Salinity, na.rm = TRUE),
            mean_Temperature = mean(Temperature, na.rm = TRUE),
            sd_Temperature = sd(Temperature, na.rm = TRUE),
            min_Temperature = min(Temperature, na.rm = TRUE),
            max_Temperature = max(Temperature, na.rm = TRUE),
            mean_Oxygen = mean(Oxygen_per, na.rm = TRUE),
            sd_Oxygen = sd(Oxygen_per, na.rm = TRUE),
            min_Oxygen = min(Oxygen_per, na.rm = TRUE),
            max_Oxygen = max(Oxygen_per, na.rm = TRUE), 
            mean_Oxygen_mgL = mean(Oxygen_mgL, na.rm = TRUE),
            sd_Oxygen_mgL = sd(Oxygen_mgL, na.rm = TRUE),
            min_Oxygen_mgL = min(Oxygen_mgL, na.rm = TRUE),
            max_Oxygen_mgL = max(Oxygen_mgL, na.rm = TRUE))

write.csv(tank_sum_stats, paste(tank_directory, "Summary_Stats_per_Treatment.csv"))

# Per Treatment - Calculate mean, sd, min, max for each parameter ----

time_sum_stats <- combined_data %>%
  select(-Tank, -Day, -Date) %>%
  group_by(Treatment, Time_Period) %>%
  summarize(mean_pHT = mean(pHT, na.rm = TRUE),
            sd_pHT = sd(pHT, na.rm = TRUE),
            min_pHT = min(pHT, na.rm = TRUE),
            max_pHT = max(pHT, na.rm = TRUE),
            mean_Salinity = mean(Salinity, na.rm = TRUE),
            sd_Salinity = sd(Salinity, na.rm = TRUE),
            min_Salinity = min(Salinity, na.rm = TRUE),
            max_Salinity = max(Salinity, na.rm = TRUE),
            mean_Temperature = mean(Temperature, na.rm = TRUE),
            sd_Temperature = sd(Temperature, na.rm = TRUE),
            min_Temperature = min(Temperature, na.rm = TRUE),
            max_Temperature = max(Temperature, na.rm = TRUE),
            mean_Oxygen = mean('Oxygen_%', na.rm = TRUE),
            sd_Oxygen = sd('Oxygen_%', na.rm = TRUE),
            min_Oxygen = min('Oxygen_%', na.rm = TRUE),
            max_Oxygen = max('Oxygen_%', na.rm = TRUE))

# Separate each treatment type --> sheet/table per treatment with an overview of the summary stats per time period. 
