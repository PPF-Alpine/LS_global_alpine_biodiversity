#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

# read in the cleaned experts list

expert_list_reptiles <- readxl::read_excel(paste0(data_storage_path, "Biodiversity_combined/Expert_validation/experts_list_cleaned.xlsx"))|>filter(group=="reptiles")


# Load the Data Preparation file 
source(
  here::here("R/02_Main_analyses/Reptiles/00_Reptile_Data_Preparations.R")
)

# read in the maximum elevation
max_elev <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/GMBA_mountains_max_elevation.xlsx")) 

# mutate the source of the elevational ranges
Data_reptiles_experts <- Data_reptiles |>
  mutate(
    source_distribution_data = "Global Assessment of Reptile Distribution (GARD)",
    source_reference = "Roll et. al. 2017 The global distribution of tetrapods reveals a need for targeted reptile conservation. Nature Ecology & Evolution 1:1677-1682",
    source_min_elevation = case_when(
      min_elevation_USE == min_elevation ~ "provided by Shai Meiri,GARD",
      min_elevation_USE == min_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    ),
    source_max_elevation = case_when(
      max_elevation_USE == max_elevation ~ "provided by Shai Meiri, GARD",
      max_elevation_USE == max_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    )
  ) |>
  left_join(max_elev,by="Mountain_range")|>
  mutate(Mean_elevation_treeline = round(Mean_elevation_treeline, 0)) |>
  select(
    sciname, TaxonID, group, family, 
    GMBA_ID, Mountain_system, Mountain_range,overlap_percentage_mountain,
    min_elevation_USE, max_elevation_USE,
    Mean_elevation_treeline,max_elevation_mountain_range,
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
         alpine_status ="",
         reviewer_comments = ""
  )

#-------------------------------------------------------------------#
# filter mountain ranges where we do have experts ---
#--------------------------------------------------------------------#

# Get unique mountain ranges from df_mountain_ranges
unique_mr <- expert_list_reptiles |> 
  filter(!is.na(email))|>
  distinct(mountain_range) |> 
  pull(mountain_range)

# Subset Data_birds_experts based on unique mountain ranges
subset_reptiles <- Data_reptiles_experts |> 
  filter(Mountain_range %in% unique_mr)

## this part if you want to get the individual lists for all ranges (not only where we have experts for)
# Get unique mountain ranges from df_mountain_ranges

unique_mr <- Data_reptiles_experts |> 
  distinct(Mountain_range) |> 
  pull(Mountain_range)

# Subset Data_birds_experts based on unique mountain ranges
subset_reptiles <- Data_reptiles_experts |> 
  filter(Mountain_range %in% unique_mr)

#--------------------------------------------#
# subset checklists to these mountain ranges---
#---------------------------------------------#

# Loop through each unique mountain range and save as Excel files
for (range in unique_mr) {
  # Replace slashes and spaces with underscores in the mountain range name
  safe_range_name <- gsub("[ /]", "_", range)  # Replaces slashes and spaces with underscores
  
  # Subset
  subset_range <- Data_reptiles_experts |>
    filter(Mountain_range == range)
  
  # 
  wb <- createWorkbook()
  addWorksheet(wb, "Reptiles")
  writeData(wb, "Reptiles", subset_range)
  
  # 
  file_path <- paste0(data_storage_path, "Biodiversity_combined/Expert_validation/Checklists/Reptiles/all_lists/Reptiles_", safe_range_name, ".xlsx")
  
  # Save 
  saveWorkbook(wb, file_path, overwrite = TRUE)
}


