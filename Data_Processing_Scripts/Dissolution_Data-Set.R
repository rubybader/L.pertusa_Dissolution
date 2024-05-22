# Set up ----
## Libraries ----
library(tidyverse)
library(dplyr)
library(readxl)
library("skimr")
library("knitr")
library("kableExtra")
library("lme4")
library("stringr")

# Reading Data ----
# test
directory_path <- "/Volumes/T7 Shield/T5_Measured"

# Get list of folders in the directory
folders <- list.dirs(directory_path, recursive = FALSE, full.names = FALSE)

# Initialize an empty list to store the combined dataframes
morphometry_df_list <- list()

# Loop through each folder
for (folder_name in folders) {
  # Define the full path to the folder
  folder_path <- file.path(directory_path, folder_name)
  
  # Check if the folder exists
  if (dir.exists(folder_path)) {
    # Define the file paths for the CSV files
    morphometry_file <- file.path(folder_path, paste0(folder_name, "-morphometry.csv"))
    #thickness_stats_file <- file.path(folder_path, paste0(folder_name, "-thickness-stats.csv"))
    
    # Check if both CSV files exist
    if (file.exists(morphometry_file)) { #&& file.exists(thickness_stats_file)) 
      # Read morphometry CSV file into dataframe
      morphometry_df <- read.csv(morphometry_file)
      
      # Pivot the dataframe wide
      morphometry_df <- pivot_wider(morphometry_df, names_from = "X", values_from = "X0")
      
      
      # Adding a "sample" column
      #morphometry_df$sample <- folder_name
      
      # Append dataframes to the list
      morphometry_df_list[[folder_name]] <- morphometry_df
    }
  }
}

# Combine the dataframes into a single large dataframe
# Change tx before saving

results_t5 <- do.call(rbind, morphometry_df_list)

list_results <- list(results_t5, results_t4, results_t3, results_t2, results_t1, results_t0)

results_all <- bind_rows(list_results)

write.csv(results_all, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Python-scripts-microCT-analysis/raw_scan_data.csv")

# Formatting ----

results_all <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Python-scripts-microCT-analysis/raw_scan_data.csv")

dates <- read_excel("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Tank-Parameters.xlsx", 
                    sheet = "Scanning_Dates") 

treatment <- read_excel("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Tank-Parameters.xlsx", 
                        sheet = "Tank_Samples") 

dates <- dates |>
  dplyr::select(Day, Scanning_Date)

formated <- results_all|>
  mutate(ID = X, 
         Time = str_extract(ID, "T[0-5]"), 
         Sample = str_extract(ID, "([0][0-9][0-9])"), 
         Day = case_when(
           Time == "T0" ~ 1,
           Time == "T1" ~ 85,
           Time == "T2" ~ 134,
           Time == "T3" ~ 183,
           Time == "T4" ~ 274,
           Time == "T5" ~ 358,
           TRUE ~ NA_integer_  # Default value if none of the conditions are met
         )) |>
  mutate(Sample = as.numeric(str_remove(Sample, "([0][0])|([0])")))|>
  dplyr::select( ID, Sample, Time, Day, Sample, area_mm2 = 'area.in.mm2', vol_mm3 = 'volume.in.mm3', SA_mm1 = 'SA_Vol.in.mm.1') |>
  arrange(Sample, Day) %>% 
  left_join(dates, relationship = "many-to-many", by = "Day") |>
  left_join(treatment, relationship = "many-to-many", by = "Sample") |>
  mutate(Scanning_Date = as.POSIXct(Scanning_Date, format = "%Y-%m-%d %H:%M:%S")) |>
  mutate(Scanning_Date = as.Date(Scanning_Date, format = "%d/%b/%Y-%u")) |>
  filter(!Sample == '92')

write.csv(formated, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Python-scripts-microCT-analysis/calculated_scan_data.csv")


## Histogram of Raw Data ----
(raw <- ggplot(formated, aes(x = vol_mm3, fill = Time)) +
  geom_histogram(position = "dodge") +
   labs(x = expression(Volume)) +
 theme_minimal() +
   scale_fill_viridis_d())

# % change ----

  # has to be rate per day so that the volume loss is standardized over the time periods which are differing lengths
  # has to be merged with carbonate system data: temp, pHT, salinity, oxygen, arag sat 

## Volume ----

formated <- read.csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Python-scripts-microCT-analysis/calculated_scan_data.csv") |>
  #select(-'...1') |>
  mutate(Treatment = case_when(Treatment == "Triple_Stressor" ~ "Triple Stressor", TRUE ~ Treatment))

dates <- read_excel("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Tank-Parameters.xlsx", 
                    sheet = "Scanning_Dates") 

envio_params <- read.csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Envio-Param+Carbonate-System_mean_by_tank-treatment.csv") |>
  dplyr::select(-Sample, -'X', -Date, -Day) |>
  mutate(Time = as.factor(Time), 
         Treatment = case_when(Treatment == "pH 1" ~ "pH1", 
                               Treatment == "pH 2" ~ "pH2",
                               Treatment == "pH 3" ~ "pH3",
                               TRUE ~ Treatment))

dates <- dates |>
  dplyr::select(Day, Time = Time_Period) |>
  mutate(Time = as.factor(Time))

vol_output <- formated |>
  dplyr::select(Time, Treatment, Tank, Sample, Volume = vol_mm3) |>
  pivot_wider(names_from = Time, values_from = Volume) |>
  dplyr::select(Treatment, Tank, Sample, T0, T1, T2, T3, T4, T5) |>
  # Amount of volume lost per time interval
  mutate(total_vol_loss = ((T0 - 
                              ifelse(is.na(T5), 
                                     ifelse(is.na(T4), 
                                            ifelse(is.na(T3), T2, T3), 
                                            T4), 
                                     T5))), 
         rate_T1 = ((T1 - T0)/T0)* 100,
         rate_T2 = ((T2 - T1)/T1)* 100, 
         rate_T3 = ((T3 - T2)/T2)* 100, 
         rate_T4 = ((T4 - T3)/T3)* 100, 
         rate_T5 = ((T5 - T4)/T4)* 100, 
         drate_T1 = rate_T1 / (85-1), 
         drate_T2 = rate_T2 / (134-85), 
         drate_T3 = rate_T3 / (183-134), 
         drate_T4 = rate_T4 / (274-183), 
         drate_T5 = rate_T5 / (358-274)) |>
  dplyr::select(Tank, Sample, Treatment, drate_T1, drate_T2, drate_T3, drate_T4, drate_T5) |>
  pivot_longer(cols = 4:8, names_to = "Time", values_to = "Rate_per_day") |>
  filter(!is.na(Rate_per_day)) |>
  mutate(Time = as.factor(str_remove(Time, "drate_"))) |> 
  inner_join(formated, by = c("Time", "Treatment", "Sample", "Tank")) |>
  mutate(Treatment = case_when(Treatment == "Triple_Stressor" ~ "Triple Stressor", TRUE ~ Treatment)) |>
  dplyr::select(Tank, Treatment, Time, Day, Sample, vol_mm3, Rate_per_day) |>
  mutate(Time = as.factor(str_remove(Time, "T"))) |> 
  inner_join(envio_params, by = c("Time", "Treatment", "Tank"), relationship = "many-to-many") |>
  dplyr::select(Tank, Treatment, Time, Day, Sample, vol_mm3, Rate_per_day, "TA_(mmol/kgSW)" = "TA_.mmol.kgSW.", "TCO2_in_(mmol/kgSW)" = "TCO2_in_.mmol.kgSW.", "pCO2_in_(matm)" ="pCO2_in_.matm.", 
         ar_in, mean_pHT, mean_Salinity, mean_Temperature, mean_Oxygen) |>
  mutate(Day = paste("+ ", Day, " days", sep = ""), 
         Day = factor(Day, levels = c("+ 85 days", "+ 134 days", "+ 183 days", "+ 274 days", "+ 358 days")))

write.csv(vol_output, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Final_params_Data.csv" )

# Plot 
(plt_vol_mean_time <- ggplot(vol_output, aes(x = Day, y = Rate_per_day, fill = Treatment)) +
    geom_col(position = "dodge") +
    labs(x = "Time Point",
         y = "Mean rate of volume change (%)") +
    theme_minimal() +
    scale_fill_viridis_d())


# RAW DATA: There are (131 - 97 =) 34 samples , which is 31% of the data, that have a positive rate of volume change
  # data is not normally distributed, but run the test and see if this is a problem
test <- vol_output |>
  filter(Rate_per_day < 0 )

test2 <- vol_output |>
  filter(Treatment == "Control")

(raw <- ggplot(vol_output, aes(x = Rate_per_day, fill = Time,)) +
    geom_histogram(position = "dodge"))

# TABLE 

dates <- read_excel("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Tank-Parameters.xlsx", 
                    sheet = "Scanning_Dates") 

days <- dates |>
  select(Time = Time_Period, Day) |>
  mutate(Time = as.factor(Time))


vol_table <- vol_output |>
  group_by(Treatment, Time) |>
  summarize(mean_rate_per_day = mean(Rate_per_day), 
            sd_rate_per_day = sd(Rate_per_day)) |>
  mutate( 'Volume Change (% day-1)' = paste(round(mean_rate_per_day, digits = 3), " (", 
                                            round(sd_rate_per_day, digits = 3), ")", sep = "")) |>
  inner_join(days, by = "Time", relationship = "many-to-many") |>
  mutate(Day = paste("+", Day, "days", sep = " "), 
         Day = factor(Day, levels = c("+ 85 days", "+ 134 days", "+ 183 days", "+ 274 days", "+ 358 days"))) |>
  select(Day, Treatment, 'Volume Change (% day-1)') 
  

(tbl_vol_mean_time <- kable(vol_table, format = "html", digits = 3, booktabs = TRUE) |>
    kable_styling(full_width = FALSE))

    
# TABLE: Total Change of Volume (T5-T0/per day)
        # NAs bec some treatments only have one data point --> Discussion
    total_vol_change <- formated |>
      select(Time, Treatment, Tank, Sample, Volume = vol_mm3) |>
      pivot_wider(names_from = Time, values_from = Volume) |>
      select(Treatment, Tank, Sample, T0, T1, T2, T3, T4, T5) |>
      mutate(per_vol_loss = ((T0 - 
                                ifelse(is.na(T5), 
                                       ifelse(is.na(T4), 
                                              ifelse(is.na(T3), T2, T3), 
                                              T4), 
                                       T5)) / 
                               ifelse(is.na(T5), 
                                      ifelse(is.na(T4), 
                                             ifelse(is.na(T3), T2, T3), 
                                             T4), 
                                      T5)) * 100) |>
      mutate(total_rate = ((T5 - T0) / T0) * 100, 
             rate_per_day_total = total_rate / (358-1))  |>
      select(Treatment, Tank, Sample, rate_per_day_total ) |>
      filter(!is.na(rate_per_day_total)) |>
      group_by(Treatment) |>
      summarize(mean_rate_per_day = mean(rate_per_day_total),
                sd_rate_per_day = sd(rate_per_day_total)) |>
      mutate( 'Volume Change (%)' = mean_rate_per_day,
              'Standard deviation (%)' = sd_rate_per_day) |>
      select(-mean_rate_per_day, -sd_rate_per_day)
    
    (tbl_vol_mean_treatment <- kable(vol_mean_treatment, format = "html", digits = 3, longtable = TRUE) %>%
        kable_styling(full_width = FALSE)) 
    
    # Caption: "Mean rate of volume change by treatment (%)"
    save_kable(tbl_vol_mean_treatment, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /IMAGES/Mean_rate_vol_change_by_treatment.png" )



  # DATA SET: Stats ----
    
    test <- vol_output |>
      left_join(merged, by = c("Tank", "Treatment", "Time")) |>
      mutate(Sample = Sample.x) |>
      select(-'Sample.y', -'Sample.x') |>
      mutate(Time = as.numeric(Time))
      # removes all volume rate of change per day that is positve (ie. the skeleton is magically gaining mass)
      # filter(Rate_per_day < 0 )
    
    write.csv(test, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Final_Data.csv" )
    
    
    
    # SOME CODE FOR GRAPH ----
    
    results_all <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Python-scripts-microCT-analysis/raw_scan_data.csv")
    
    dates <- read_excel("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Tank-Parameters.xlsx", 
                        sheet = "Scanning_Dates") 
    dates <- dates |>
      select(Time = Time_Period, Day, Scanning_Date) |>
      mutate(Time = as.integer(Time)) |>
      filter(!Time == 0)
    
    treatment <- read_excel("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Tank-Parameters.xlsx", 
                            sheet = "Tank_Samples") 
    output <- results_all|>
      mutate(ID = Sample, 
             Time = str_extract(ID, "T[0-5]"),
             Sample = str_extract(ID, "([0][0-9][0-9])"), 
             Day = case_when(
               Time == "T0" ~ 1,
               Time == "T1" ~ 85,
               Time == "T2" ~ 134,
               Time == "T3" ~ 183,
               Time == "T4" ~ 274,
               Time == "T5" ~ 358,
               TRUE ~ NA_integer_  # Default value if none of the conditions are met
             ), 
             Time = str_remove(Time, "T"),
             Time = as.integer(Time)) |>
      mutate(Sample = as.numeric(str_remove(Sample, "([0][0])|([0])")))|>
      select( ID, Sample, Time, Day, Sample, area_mm2 = 'area.in.mm2', vol_mm3 = 'volume.in.mm3', SA_mm1 = 'SA_Vol.in.mm.1') |>
      arrange(Sample, Day) %>% 
      # Calculate rate of volume loss (volume change per unit time)
      merge(dates, by = "Time", all.x = TRUE) |>
      left_join(treatment, relationship = "many-to-many", by = "Sample") |>
      mutate(Scanning_Date = as.POSIXct(Scanning_Date, format = "%Y-%m-%d %H:%M:%S")) |>
      mutate(Scanning_Date = as.Date(Scanning_Date, format = "%d/%b/%Y-%u")) |>
      filter(!Sample == '92')
    
    write.csv(output, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Python-scripts-microCT-analysis/calculated_scan_data.csv")

  
    