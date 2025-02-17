#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(openxlsx)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

# read in the cleaned experts list

expert_list_mammals <- readxl::read_excel(paste0(data_storage_path, "Biodiversity_combined/Expert_validation/experts_list_cleaned.xlsx"))|>filter(group=="mammals")

# Load the Data Preparation file 
source(
  here::here("R/01_Data_processing/01_Mammals/08_00_prep_mammal_data_expert_validation.R")
)

# read in the maximum elevation
max_elev <- readxl::read_excel(paste0(data_storage_path, "Mountains/Suzette_Alpine_biome/GMBA_mountains_max_elevation.xlsx")) 

#----------------------------------------------------------#
# add columns for sources, alpine, uncertainty etc., ---
#----------------------------------------------------------#

# mutate the source of the elevational ranges
Data_mammals_experts <- Data_mammals |>
  mutate(
    source_distribution_data = "Mammal Diversity Database (MDD)",
    source_reference = "Marsh et al., 2022. Expert range maps of global mammal distributions harmonised to three taxonomic authorities. Journal of Biogeography 49 (5): 979-992.",
    source_min_elevation = case_when(
      min_elevation_USE == min_elevation ~ "Handbook of the mammals of the world through Nathan Upham (HMW)",
      min_elevation_USE == min_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    ),
    source_max_elevation = case_when(
      max_elevation_USE == max_elevation ~ "Handbook of the mammals of the world through Nathan Upham (HMW)",
      max_elevation_USE == max_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    )
  ) |>
  left_join(max_elev,by="Mountain_range")|>
  mutate(Mean_elevation_treeline = round(Mean_elevation_treeline, 0)) |>
  select(
    sciname, order, family, 
    Mountain_system, Mountain_range,overlap_percentage_mountain,
    habitat,
    min_elevation_USE, max_elevation_USE,Mean_elevation_treeline,max_elevation_mountain_range,
    source_distribution_data, source_reference, source_min_elevation, source_max_elevation)|>
  rename(
    min_elevation = min_elevation_USE,
    max_elevation = max_elevation_USE,
    overlap_perc_mountain_range = overlap_percentage_mountain,
    mean_treeline = Mean_elevation_treeline
  )|>
  arrange(Mountain_range, desc(max_elevation), desc(overlap_perc_mountain_range))|>
  mutate(mountain_range_corrected ="",
         min_corrected = "",
         max_corrected = "",
         validated_elevation_data = "",
         confidence_assessment = "",
         alpine_status ="",reviewer_comments = ""
  )

#-------------------------------------------------------------------#
# filter mountain ranges where we do have experts ---
#--------------------------------------------------------------------#

# Get unique mountain ranges from df_mountain_ranges
unique_mr <- expert_list_mammals |> 
  filter(!is.na(email))|>
  distinct(mountain_range) |> 
  pull(mountain_range)

unique_mr <- Data_mammals_experts |> 
  distinct(Mountain_range) |> 
  pull(Mountain_range)

# Subset Data_birds_experts based on unique mountain ranges
subset_mammals <- Data_mammals_experts |> 
  filter(Mountain_range %in% unique_mr)

#--------------------------------------------#
# subset checklists to these mountain ranges---
#---------------------------------------------#

# Loop through each unique mountain range and save as Excel files
for (range in unique_mr) {
  # Replace slashes and spaces with underscores in the mountain range name
  safe_range_name <- gsub("[ /]", "_", range)  # Replaces slashes and spaces with underscores
  
  # Subset
  subset_range <- Data_mammals_experts |>
    filter(Mountain_range == range)
  
  # 
  wb <- createWorkbook()
  addWorksheet(wb, "Mammals")
  writeData(wb, "Mammals", subset_range)
  
  # 
  file_path <- paste0(data_storage_path, "Biodiversity_combined/Expert_validation/Checklists/Mammals/All_Lists/Mammals_", safe_range_name, ".xlsx")
  
  # Save 
  saveWorkbook(wb, file_path, overwrite = TRUE)
}
