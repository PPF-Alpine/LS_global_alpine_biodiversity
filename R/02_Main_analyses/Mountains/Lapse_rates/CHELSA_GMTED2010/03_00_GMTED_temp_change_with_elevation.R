
# in this script I calculate elevation change for each mountain range using the lapse rate (see script Join lapse rates GMBA v1- v2 and lapse rates all mountains)
# Elevation change = Delta Temp / Lapse rate x 1000

# Load configuration
source(
  here::here("R/00_Config_file.R")
)


library(tidyverse)
library(plotly)
library(ggplot2)
library(purrr)

#----------------------------------------------------------#
# 2. Load and join data ---
#----------------------------------------------------------#
treeline <-  readxl::read_excel(file.path(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Treeline/treeline_elevations_PPF.xls"))|> 
  mutate(Mean_elevation = round(Mean_elevation),0)

# load the lapse rates 
lapse_rates_05 <- readxl::read_excel(file.path(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Lapse_Rates/Lapse_Rates_GMBA_V2_Level_05.xlsx")) |>
  dplyr::select(Mountain_range_03, mean_level_03,median_level_03) |>
  distinct(Mountain_range_03, .keep_all = TRUE)|>
  rename(Mountain_range = Mountain_range_03)|>
  rename(lapse_rate_mean_05 = mean_level_03)|>
  rename(lapse_rate_median_05 = median_level_03)

lapse_rates_04 <- readxl::read_excel(file.path(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Lapse_Rates/Lapse_Rates_GMBA_V2_Level_04.xlsx"))|>
  group_by(Mountain_range_03)|>
  mutate(lapse_rate_median_04 = round(median(Lapse_rate),3))|>
  ungroup()|>
  dplyr::select(Mountain_range_03, Mountain_system, mean_level_03,lapse_rate_median_04) |>
  distinct(Mountain_range_03, .keep_all = TRUE)|>
  rename(Mountain_range = Mountain_range_03)|>
  rename(lapse_rate_mean_04 = mean_level_03)


tl_lapserate <- lapse_rates_04|> 
  left_join(lapse_rates_05,by="Mountain_range")|>
  left_join(treeline, by = "Mountain_range")|>
  dplyr::select(Mountain_system,Mountain_range,Mean_elevation,Mean_temperature,lapse_rate_mean_04,lapse_rate_median_04,lapse_rate_mean_05,lapse_rate_median_05,)|>
  drop_na(Mean_elevation)


# there is not for all level 03s a level 05 --> best to go with level 04

#-------------------------------------------------------------------------------#
# Calculate elevation change for different temperature thresholds (Delta T) ---
#-------------------------------------------------------------------------------#

# How does a temp difference of x Degree translate in Elevation Meters in each Mountain range ?

# Elevation Change = Delta Temp/Lapse rate x 1000

elev_change <- tl_lapserate|>
  mutate(elev_change_1_degree = round((-1/lapse_rate_mean_04)*1000),1)|>
  mutate(Mean_elevation_1_degree = Mean_elevation - elev_change_1_degree)|>
  mutate(elev_change_2_degree = round((-2/lapse_rate_mean_04)*1000),1)|> 
  mutate(Mean_elevation_2_degree = Mean_elevation - elev_change_2_degree)|> 
  mutate(elev_change_3_degree = round((-3/lapse_rate_mean_04)*1000),1)|>
  mutate(Mean_elevation_3_degree = Mean_elevation - elev_change_3_degree)|>
  mutate(elev_change_4_degree = round((-4/lapse_rate_mean_04)*1000),1)|>
  mutate(Mean_elevation_4_degree = Mean_elevation - elev_change_4_degree)|>
  mutate(elev_change_5_degree = round((-5/lapse_rate_mean_04)*1000),1)|>
  mutate(Mean_elevation_5_degree = Mean_elevation - elev_change_5_degree)|>
  mutate(elev_change_6_degree = round((-6/lapse_rate_mean_04)*1000),1)|>
  mutate(Mean_elevation_6_degree = Mean_elevation - elev_change_6_degree)|>
  mutate(elev_change_8_degree = round((-8/lapse_rate_mean_04)*1000),1)|>
  mutate(Mean_elevation_8_degree = Mean_elevation - elev_change_8_degree)|>
  mutate(elev_change_1_degree_med = round((-1/lapse_rate_median_04)*1000),1)|>
  mutate(Mean_elevation_1_degree_med = Mean_elevation - elev_change_1_degree)|>
  mutate(elev_change_2_degree_med = round((-2/lapse_rate_median_04)*1000),1)|> 
  mutate(Mean_elevation_2_degree_med = Mean_elevation - elev_change_2_degree)|> 
  mutate(elev_change_3_degree_med = round((-3/lapse_rate_median_04)*1000),1)|>
  mutate(Mean_elevation_3_degree_med = Mean_elevation - elev_change_3_degree)|>
  mutate(elev_change_4_degree_med = round((-4/lapse_rate_median_04)*1000),1)|>
  mutate(Mean_elevation_4_degree_med = Mean_elevation - elev_change_4_degree)|>
  mutate(elev_change_5_degree_med = round((-5/lapse_rate_median_04)*1000),1)|>
  mutate(Mean_elevation_5_degree_med = Mean_elevation - elev_change_5_degree)|>
  mutate(elev_change_6_degree_med = round((-6/lapse_rate_median_04)*1000),1)|>
  mutate(Mean_elevation_6_degree_med = Mean_elevation - elev_change_6_degree)|>
  mutate(elev_change_8_degree_med = round((-8/lapse_rate_median_04)*1000),1)|>
  mutate(Mean_elevation_8_degree_med = Mean_elevation - elev_change_8_degree)|>
  mutate(elev_change_1_05 = round((-1/lapse_rate_mean_05)*1000),1)|>
  mutate(Mean_elevation_1_05 = Mean_elevation - elev_change_1_05)|>
  mutate(elev_change_2_05 = round((-2/lapse_rate_mean_05)*1000),1)|> 
  mutate(Mean_elevation_2_05 = Mean_elevation - elev_change_2_05)|> 
  mutate(elev_change_3_05 = round((-3/lapse_rate_mean_05)*1000),1)|>
  mutate(Mean_elevation_3_05 = Mean_elevation - elev_change_3_05)|>
  mutate(elev_change_4_05 = round((-4/lapse_rate_mean_05)*1000),1)|>
  mutate(Mean_elevation_4_05 = Mean_elevation - elev_change_4_05)|>
  mutate(elev_change_5_05 = round((-5/lapse_rate_mean_05)*1000),1)|>
  mutate(Mean_elevation_5_05 = Mean_elevation - elev_change_5_05)|>
  mutate(elev_change_6_05 = round((-6/lapse_rate_mean_05)*1000),1)|>
  mutate(Mean_elevation_6_05 = Mean_elevation - elev_change_6_05)|>
  mutate(elev_change_8_05 = round((-8/lapse_rate_mean_05)*1000),1)|>
  mutate(Mean_elevation_8_05 = Mean_elevation - elev_change_8_05)|>
  mutate(elev_change_1_05_med = round((-1/lapse_rate_median_05)*1000),1)|>
  mutate(Mean_elevation_1_05_med = Mean_elevation - elev_change_1_05)|>
  mutate(elev_change_2_05_med = round((-2/lapse_rate_median_05)*1000),1)|> 
  mutate(Mean_elevation_2_05_med = Mean_elevation - elev_change_2_05)|>  
  mutate(elev_change_3_05_med = round((-3/lapse_rate_median_05)*1000),1)|>
  mutate(Mean_elevation_3_05_med = Mean_elevation - elev_change_3_05)|>
  mutate(elev_change_4_05_med = round((-4/lapse_rate_median_05)*1000),1)|>
  mutate(Mean_elevation_4_05_med = Mean_elevation - elev_change_4_05)|>
  mutate(elev_change_5_05_med = round((-5/lapse_rate_median_05)*1000),1)|>
  mutate(Mean_elevation_5_05_med = Mean_elevation - elev_change_5_05)|>
  mutate(elev_change_6_05_med = round((-6/lapse_rate_median_05)*1000),1)|>
  mutate(Mean_elevation_6_05_med = Mean_elevation - elev_change_6_05)|>
  mutate(elev_change_8_05_med = round((-8/lapse_rate_median_05)*1000),1)|>
  mutate(Mean_elevation_8_05_med = Mean_elevation - elev_change_8_05)|>
  drop_na()|>
  dplyr::select(-"1")|>
  arrange(Mean_elevation)

writexl::write_xlsx(elev_change,file.path(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Treeline/Treeline_Lapse_Rate_04_05.xlsx"))

