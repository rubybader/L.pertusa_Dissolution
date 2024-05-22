## ---------------------------
##
## Script name: Carbonate-system
##
## Purpose of script: Making sense of the output data of SysCO2. Essentially the carbonate system of each tank throughout the duration of the study.
##
## Author: Ruby Rose Bader
##
## Date Created: 2024-03-20
##
## Copyright (c) Ruby Bader, 2024
## Email: ruby.bader@me.com
##
## ---------------------------
## Notes: Kristian Becks SysCO2 calculations have been used. Based on TA and DIC measurements and enviornmental tank parameters supplied by Krisitna Beck.
## ---------------------------

library(readxl)
library(dplyr)
library(skimr)
library(ggplot2)
library(cowplot)
library(writexl)


setwd("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry")

# Creating Data Frame for SysCO2 Inputs ----
  # Dates of Carbonate Chemistry Sampling: Select only rows that contain the specifc dates corresponding to DIC and TA measurments
  # Assosciate a TX with this

dates <- as.POSIXct(c("2023-03-07", "2023-04-04", "2023-05-04", "2023-05-30", "2023-06-27", "2023-07-26", "2023-08-22", "2023-09-19", "2023-10-19", "2023-11-14", "2023-12-19"))

combined_data <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Raw_data/Combined_enviornmental-param_sampling.csv")

inputs <- combined_data |>
  filter(Date %in% dates) |>
  select(Date, Treatment, Tank, pH, pHT, Temperature, Salinity) |>
  mutate(Tank = gsub("Tank", "", Tank)) |>
  mutate(id = paste0(Date, "-", Treatment, "-", Tank))

input_mean <- filtered %>%
  group_by(Date, Treatment) %>%
  summarize(
    Mean_pH = mean(pHT, na.rm = TRUE),
    SD_pH = sd(pHT, na.rm = TRUE),
    Mean_pHT = mean(pHT, na.rm = TRUE),
    SD_pHT = sd(pHT, na.rm = TRUE),
    Mean_Temperature = mean(Temperature, na.rm = TRUE),
    SD_Temp = sd(Temperature, na.rm = TRUE),
    Mean_Salinity = mean(Salinity, na.rm = TRUE),
    SD_Sal = sd(Salinity, na.rm = TRUE),
  )

write.csv(input_mean, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/Mean_Carbonate-System.csv")

# Matching TA with Date, Tank and Treatment 

ta_data <- read_excel("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/OUTPUT_Ruby_CO2Sys(TA-pH)_v2.xlsx", sheet = "Krisi_TA-pH")
# delete all rows where TA is N/A
# keep TCO2_(mmol/kgSW), TA_(mmol/kgSW), P_(dbars)

ta_data <- ta_data |>
  filter(!is.na('TA_(mmol/kgSW')) |>
  mutate(id = paste0(Date, "-", Treatment, "-", Tank)) |>
  select(id, 'TCO2_(mmol/kgSW)', 'TA_(mmol/kgSW)', 'P_(dbars)')


merged_data <- inner_join(filtered, ta_data, by = "id")

write.csv(merged_data, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/TA-TCO2_Carbonate-System.csv")

# OUTPUT Carbonate System ----

  # Environmental Parameters for merge CS

  # need to merge the carbonate system with the enviornmental params, per tank. Check there is tank col 
  # merge by tank and month. 
  # - id, Time = Months, - Date
  # not sure if i need sample id, i guess sample is a result of tank and treatment so it can stay


sum_tank_time <- read_csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Envio-Param_mean_by_tank-treatment.csv") |>
  select(- "...1") |>
  mutate(Treatment = case_when(
    Treatment == "pH1" ~ "pH 1",
    Treatment == "pH2" ~ "pH 2",
    Treatment == "pH3" ~ "pH 3",
    Treatment == "Multi_HF" ~ "Triple Stressor",
    TRUE ~ Treatment),
    Time = as.factor(Time))
  # This is a mean, sd, min/max per time periods per tank per treatment.

output <- read.csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Final_params_Data.csv") |>
  mutate(log_ar = log(ar_in), 
         Day = factor(Day, levels = c("+ 85 days", "+ 134 days", "+ 183 days", "+ 274 days", "+ 358 days"))) |>
  filter(!is.na(ar_in))

sum_ar <- output |>
  group_by(Treatment) |>
  summarize(mean = mean(ar_in), 
            sd = sd(ar_in), 
            min = min(ar_in), 
            max = max(ar_in))

sheet <- read_excel("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/OUTPUT_Ruby_CO2Sys(TA-pH)_v2.xlsx", sheet = "Krisi_TA-pH") |>
  mutate(id = paste0(Date, "-", Treatment, "-", Tank)) |>
  mutate(Time = as.factor(Month)) |>
  select(-Month, - Salinity, -Temperature, -pHT, - id) 
  # This sheet only has phT, temp, salinity for one specific day, it is not the mean of the time period (which is what we need for merging with the volume chnange data object)


merged <- sheet |>
  inner_join(sum_tank_time, by = c("Tank", "Treatment", "Time"), relationship = "many-to-many")


write.csv(merged, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Envio-Param+Carbonate-System_mean_by_tank-treatment.csv" )


# TABLE

merged <- read.csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/Envio-Param+Carbonate-System_mean_by_tank-treatment.csv")


total_mean <- merged |>
  select(Time, Day, Tank, Sample, Treatment, mean_Salinity, mean_Temperature, TA = 'TA_.mmol.kgSW.', TCO2 = 'TCO2_in_.mmol.kgSW.', 
         mean_pHT, ar_in, pCO2 = 'pCO2_in_.matm.', mean_Oxygen ) |>
  # Remove NA values 
  filter(!is.na(TA) & !is.na(pCO2) & !is.na(TCO2) & !is.na(mean_Oxygen) & !is.na(mean_pHT) & !is.na(mean_Temperature))|>
  group_by(Treatment) |>
  summarize(mean_pCO2 = mean(pCO2, na.rm = TRUE),
            sd_pCO2 = sd(pCO2, na.rm = TRUE),
            mean_o2 = mean(mean_Oxygen, na.rm = TRUE),
            sd_o2 = sd(mean_Oxygen, na.rm = TRUE), 
            mean_temp = mean(mean_Temperature, na.rm = TRUE),
            sd_temp = sd(mean_Temperature, na.rm = TRUE)) |>
  mutate("pCO2 (ppm)" = paste(round(mean_pCO2, digits = 3), "(", round(sd_pCO2, digits = 3), ")", sep = "" ), 
         "02 (mg L-1)" = paste(round(mean_o2, digits = 3), "(", round(sd_o2, digits = 3), ")", sep = "" ),
         "Temperature (C)" = paste(round(mean_temp, digits = 3), "(",round(sd_temp, digits = 3), ")", sep = "" )) |>
  select(Treatment, "pCO2 (ppm)", "02 (mg L-1)", "Temperature (C)")


(tbl_means <- kable(total_mean, digits = 3, booktabs = TRUE, labels = c("", "$pCO_{2} ppm$", "$0_{2} (mg L^{-1})$", "$Temperature (C)$")) |>
    kable_styling(full_width = FALSE))

modify_days <- function(day) {
  paste("Time +", day, "days", sep = " ") # Constructing the string
}
  

 table <- merged |>
  select(Time, Day, Tank, Sample, Treatment, mean_Salinity, mean_Temperature, TA = 'TA_.mmol.kgSW.', TCO2 = 'TCO2_in_.mmol.kgSW.', 
         pHT = mean_pHT, ar_in, pCO2 = 'pCO2_in_.matm.', mean_Oxygen_mgL ) |>
  # Remove NA values 
  filter(!is.na(TA) & !is.na(pCO2) & !is.na(TCO2) & !is.na(mean_Oxygen_mgL) & !is.na(mean_Temperature))|>
  # For making a table, it is a mean of each treatment per time point (see screenshot of example table)
  group_by(Treatment, Time, Day) |>
  summarize(mean_TA = mean(TA, na.rm = TRUE),
            sd_TA = sd(TA, na.rm = TRUE),
            mean_pCO2 = mean(pCO2, na.rm = TRUE),
            sd_pCO2 = sd(pCO2, na.rm = TRUE),
            mean_pHT = mean(pHT, na.rm = TRUE),
            sd_pHT = sd(pHT, na.rm = TRUE),
            mean_TCO2 = mean(TCO2, na.rm = TRUE),
            sd_TCO2 = sd(TCO2, na.rm = TRUE),
            mean_Sal = mean(mean_Salinity, na.rm = TRUE),
            sd_Sal = sd(mean_Salinity, na.rm = TRUE),
            mean_o2 = mean(mean_Oxygen_mgL, na.rm = TRUE),
            sd_o2 = sd(mean_Oxygen_mgL, na.rm = TRUE), 
            mean_temp = mean(mean_Temperature, na.rm = TRUE),
            sd_temp = sd(mean_Temperature, na.rm = TRUE), 
            mean_ar = mean(ar_in, na.rm = TRUE),
            sd_ar = sd(ar_in, na.rm = TRUE))|>
  ungroup() |>
  mutate(Days = modify_days(Day)) |>
  mutate(Days = as.factor(Days)) |>
  mutate(ar_column_name = paste(round(mean_ar, digits = 3), " (", round(sd_ar, digits = 3), ")", sep = ""), 
         at_column_name = paste(round(mean_TA, digits = 3), " (", round(sd_TA, digits = 3), ")", sep = ""), 
         ct_column_name = paste(round(mean_TCO2, digits = 3), " (", round(sd_TCO2, digits = 3), ")", sep = ""), 
         pht_column_name = paste(round(mean_pHT, digits = 3), " (", round(sd_pHT, digits = 3), ")", sep = "")
         #ppm_temp_o2 = paste(round(mean_pCO2, digits = 0), "ppm", round(mean_temp, digits = 1), expression(degree, "C)", sep = ""), round(mean_o2, digits = 1), "%", sep = " ")
         ) |>
  select(Treatment, Days, ar_column_name, at_column_name, ct_column_name, pht_column_name) |>
  pivot_longer(names_to = "Parameter", values_to = "Values", cols = 3:6 ) |>
  pivot_wider(names_from = "Treatment", values_from = "Values") |>
   mutate(Parameter = case_when(
     Parameter == "at_column_name" ~ "Total Alkalinity",
     Parameter == "ar_column_name" ~ "Aragonite Saturation",
     Parameter == "ct_column_name" ~ "Total CO2",
     Parameter == "pht_column_name" ~ "Total pH",
     TRUE ~ as.character(Parameter) # Keep original value if no condition matches
   ))
  
 write_xlsx(table, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/EDIT_Table_data.xlsx" )
 
 

table <- read_xlsx("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/EDIT_Table_data.xlsx")

# Replace NA values with empty strings
table[is.na(table)] <- ""

# Render the kable table with kableExtra
(tbl_cc <- kable(table, digits = 3, booktabs = TRUE, na = "", col.names = c("", "Control", "Triple Stressor", "pH 1", "pH 2", "pH 3")) %>%
  kable_styling(full_width = FALSE) |>
  row_spec(c(1, 6, 11, 16, 21, 26), bold = TRUE, extra_css = "font-weight: bold;", color = "black"))







write_xlsx(total_mean, "/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/TANK_PARAMETERS/EDIT_For_Params-Table.xlsx" )


# Plots ----

param_limits <- list(
  mean_TA = list(xlim = c(0, 215), ylim = c(2200, 2400)),
  mean_pHT = list(xlim = c(0, 215), ylim = c(7, 9)),
  mean_pCO2 = list(xlim = c(0, 215), ylim = c(1800, 2100)),
  mean_HCO3 = list(xlim = c(0, 215), ylim = c(1770, 2300)),
  mean_CO3 = list(xlim = c(0, 215), ylim = c(20, 230)), 
  
)

colors <- c("#132B43", "#264D71", "#4A7CA4", "#7FA7D4", "#BCD9F4")

custom_theme <- theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),  # White background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_blank(),     # Remove panel border
        axis.line = element_line(color = "black"), 
        text = element_text(size = 18))  # Set axis lines

# (v2) Improved Plots ----

  # plot the mean value and standard deviation for each timepoint - for pHT and aragonite saturation 
  # -  plot the mean value and standard deviation for each timepoint. 
  # - plotting all values of all treatments in different colours in the same plot to make them more comparable. 

(plotpHT <- ggplot(mean, aes(y = mean_pHT, x = Day, colour = Treatment)) +
    geom_point() +
    geom_errorbar(aes(ymin = mean_pHT - sd_pHT, ymax = mean_pHT + sd_pHT), width = 0.2) +  # Error bars for SD
    geom_smooth(method = "lm", se = FALSE, size = 0.5) +
    scale_colour_discrete(limits=c('Control', 'Triple Stressor', 'pH 1', 'pH 2', 'pH 3')) +
    labs(#title = "Mean pHT per Time Point for each Treatment", 
      y = "pHT") +  # Add units to y-axis label
    geom_vline(xintercept = c(65, 134, 183, 274, 358), linetype = "dashed", color = "grey") +  # Add vertical lines
    ylim(7, 9) +
    xlim(0, 215)  +
    custom_theme +
   # Move legend to top right corner
   theme(
     legend.position = c(.95, .95),
     legend.justification = c("right", "top"),
     legend.box.just = "right",
     legend.margin = margin(6, 6, 6, 6), 
     legend.title = element_text(face = "bold", size = 15))
)

(plotCO3 <- ggplot(mean, aes(y = mean_CO3, x = Day, colour = Treatment)) +
    geom_point() +
    geom_errorbar(aes(ymin = mean_CO3 - sd_CO3, ymax = mean_CO3 + sd_CO3), width = 0.2) +  # Error bars for SD
    geom_smooth(method = "lm", se = FALSE, size = 0.5) +
     labs(#title = "Mean CO3 (µmol/kgSW) per Time Point for each Treatment", 
      y = "CO3 (mmol/kgSW) ") +  # Add units to y-axis label
    geom_vline(xintercept = c(65, 134, 183, 274, 358), linetype = "dashed", color = "grey") +  # Add vertical lines
    ylim(20, 230) +
    xlim(0, 215)  +
    custom_theme +
    # Remove color legend
    guides(color = FALSE) )

plot_list <- list(plotCO3, plotpHT)
plot_panel <- plot_grid(plotlist = plot_list, nrow = 1, ncol = 2)
print(plot_panel)
ggsave("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/CC-Output-Plots.png", plot_panel)


# Tables ----

  # present the other values in a table (Other than pHT and CO3)
  # mean (of ead day) and sd per treatment

setwd("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry")

install.packages("knitr")
install.packages("kableExtra")

library("knitr")
library("kableExtra")

parameters <- c("TA", "pHT", "pCO2", "HCO3", "CO3")

summary_TA <- filtered |>
  group_by(Treatment) |>
  summarize( Mean = mean(TA, na.rm = TRUE), 
             SD = sd(TA, na.rm = TRUE))

tbl_TA <- kable(summary_TA, format = "html", caption = "Total Alkalinity (µmol/kgSW)", digits = 3) %>%
  kable_styling(full_width = FALSE) %>% 
  #add_header_above(c("Treatment" =1, "Mean" = 1, "SD" = 1)) %>%
  column_spec(1, bold = TRUE) |>
  save_kable("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/tbl_TA.png")

print(tbl_TA)

summary_HCO3 <- filtered |>
  group_by(Treatment) |>
  summarize( Mean = mean(HCO3, na.rm = TRUE), 
             SD = sd(HCO3, na.rm = TRUE))

tbl_HCO3 <- kable(summary_HCO3, format = "html", caption = "Bicarbonate ions (µmol/kgSW)", digits = 3) %>%
  kable_styling(full_width = FALSE) %>% 
  #add_header_above(c("Treatment" =1, "Mean" = 1, "SD" = 1)) %>%
  column_spec(1, bold = TRUE) |>
  save_kable("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/tbl_HCO3.png")


print(tbl_HCO3)

summary_CO3 <- filtered |>
  group_by(Treatment) |>
  summarize( Mean = mean(CO3, na.rm = TRUE), 
             SD = sd(CO3, na.rm = TRUE))

tbl_CO3 <- kable(summary_CO3, format = "html", caption = "Carbonate ions (µmol/kgSW)", digits = 3) %>%
  kable_styling(full_width = FALSE) %>% 
  #add_header_above(c("Treatment" =1, "Mean" = 1, "SD" = 1)) %>%
  column_spec(1, bold = TRUE) |>
  save_kable("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/tbl_CO3.png")


print(tbl_CO3)

summary_pCO2 <- filtered |>
  group_by(Treatment) |>
  summarize( Mean = mean(pCO2, na.rm = TRUE), 
             SD = sd(pCO2, na.rm = TRUE))

tbl_pCO2 <- kable(summary_pCO2, format = "html", caption = "Partial pressure of CO2 (µatm)", digits = 3) %>%
  kable_styling(full_width = FALSE) %>% 
  #add_header_above(c("Treatment" =1, "Mean" = 1, "SD" = 1)) %>%
  column_spec(1, bold = TRUE)|>
  save_kable("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/tbl_pCO2.png")


print(tbl_pCO2)

summary_pHT <- filtered |>
  group_by(Treatment) |>
  summarize( Mean = mean(pHT, na.rm = TRUE), 
             SD = sd(pHT, na.rm = TRUE))

tbl_pHT <- kable(summary_pHT, format = "html", caption = "Mean pH per Treatment over total time", digits = 3) %>%
  kable_styling(full_width = FALSE) %>% 
  #add_header_above(c("Treatment" =1, "Mean" = 1, "SD" = 1)) %>%
  column_spec(1, bold = TRUE) |>
  save_kable("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Carbonate-chemistry/tbl_pHT.png")
  

print(tbl_pHT)

table_list <- list(tbl_pHT, tbl_pCO2, tbl_CO3, tbl_HCO3, tbl_TA)

