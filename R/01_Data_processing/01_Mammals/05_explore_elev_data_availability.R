
#---------------------------------------------------------------------------------#
#        Explore availability of elevational data 
#--------------------------------------------------------------------------------#


#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(readxl)


# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#------------------------#
# 2. Load data
#-------------------------#

MDD_checklist <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/Checklist_Mammals_elevations_HMW.xlsx"))

#------------------------#
# 3. Explore data
#-------------------------#

# Count in how many different mountain ranges and mountain systems a species occurs
species_mountain_count <- MDD_checklist |> 
  group_by(sciname) |>
  summarise(
    number_mountain_ranges = n_distinct(Mountain_range),
    number_mountain_systems = n_distinct(Mountain_system),
    .groups = 'drop'
  ) |>
  arrange(-number_mountain_ranges)


# How much elevational info are available per mountain range
percentage_elevation_range <- MDD_checklist |>
  # Create a helper column to flag available information
  mutate(min_elevation_info = ifelse(!is.na(min_elevation), 1, 0),
         max_elevation_info = ifelse(!is.na(max_elevation), 1, 0),
         both_elevation_info = ifelse(!is.na(min_elevation) & !is.na(max_elevation), 1, 0)) |>
  # Group by range and calculate percentage
  group_by(Mountain_range) |>
  summarise(total_species = n_distinct(sciname),
            
            species_with_min_elevation_info = sum(min_elevation_info, na.rm = TRUE),
            percentage_min_elevation = round((species_with_min_elevation_info / total_species) * 100,2),
            
            species_with_max_elevation_info = sum(max_elevation_info, na.rm = TRUE),
            percentage_max_elevation = round((species_with_max_elevation_info / total_species) * 100,2),
            
            species_with_both_elevation_info = sum(both_elevation_info, na.rm = TRUE),
            percentage_both_elevation = round((species_with_both_elevation_info / total_species) * 100,2),
            
            .groups = 'drop')|>
  select(total_species,Mountain_range,percentage_min_elevation,percentage_max_elevation,percentage_both_elevation)


# How much elevational info are available per mountain system
percentage_elevation_system <- MDD_checklist |>
  # Create a helper column to flag available information
  mutate(min_elevation_info = ifelse(!is.na(min_elevation), 1, 0),
         max_elevation_info = ifelse(!is.na(max_elevation), 1, 0),
         both_elevation_info = ifelse(!is.na(min_elevation) & !is.na(max_elevation), 1, 0)) |>
  
  # Group by Mountain_system and calculate percentage
  group_by(Mountain_system) |>
  summarise(total_species = n_distinct(sciname),
            
            species_with_min_elevation_info = sum(min_elevation_info, na.rm = TRUE),
            percentage_min_elevation = round((species_with_min_elevation_info / total_species) * 100,2),
            
            species_with_max_elevation_info = sum(max_elevation_info, na.rm = TRUE),
            percentage_max_elevation = round((species_with_max_elevation_info / total_species) * 100,2),
            
            species_with_both_elevation_info = sum(both_elevation_info, na.rm = TRUE),
            percentage_both_elevation = round((species_with_both_elevation_info / total_species) * 100,2),
            
            .groups = 'drop')|>
  select(total_species,Mountain_system,percentage_min_elevation,percentage_max_elevation,percentage_both_elevation)


# Info in the habitat column 
percentage_habitat_info <- MDD_checklist |>
  # Create a helper column to flag available information
  mutate(habitat_info = ifelse(!is.na(habitat), 1, 0)) |>
  
  # Group by range and calculate percentage
  group_by(Mountain_range) |>
  summarise(total_species = n_distinct(sciname),
            species_with_habitat_info = sum(habitat_info, na.rm = TRUE),
            percentage_habitat = round((species_with_habitat_info / total_species) * 100, 2),
            .groups = 'drop') |>
  select(Mountain_range, total_species, percentage_habitat)

