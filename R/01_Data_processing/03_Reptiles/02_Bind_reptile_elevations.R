

#----------------------------------------------------------#
#         Bind Elevations to Species 
#----------------------------------------------------------#

# This script binds elevation data to species names (GARD)
# elevation data has been obtained by Squambase, Meiri 2024
# https://onlinelibrary.wiley.com/doi/10.1111/geb.13812 

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(visdat)
library(tidyverse)
library(readxl)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2. Load data -----
#----------------------------------------------------------#


file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/Reptiles_Checklist.xlsx")

# this binds the different sheets into one dataframe
Reptile_Checklist <- readxl::excel_sheets(file_path) |>
  map_df(~read_excel_sheets(.x))



# Load the elevation data
Elevation_data_Reptiles <- read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/additional_data/Meiri_2024_elevation_data.xlsx")) |> 
  rename(sciname = 'Species name (Binomial)')|>
  rename(min_elevation = "Minimum elevation (m)")|>
  rename(max_elevation = "Maximum elevation (m)")

#----------------------------------------------------------#
# 3. Left join data -----
#----------------------------------------------------------#

# Left join to see only the data where we have distribution data
Reptile_Elevations <- Reptile_Checklist|> left_join(Elevation_data_Reptiles,by = "sciname")|> 
  arrange(sciname)

GMBA_names_level_03 <- readRDS(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_names_level_03_04.rds"))|>
  filter(Hier_Lvl =="3")|>
  group_by(Mountain_range) |>
  summarise(gmba_ID = first(gmba_ID), 
            Mountain_system = first(Mountain_system))


Reptile_Elevations <- Reptile_Elevations |>
  left_join(GMBA_names_level_03, by = "Mountain_range")|>
  select(TaxonID, group, family, sciname, GMBA_ID, Mountain_system, 
         Mountain_range, species_area, overlap_percentage_mountain, 
         overlap_percentage_alpine, min_elevation, max_elevation)


#----------------------------------------------------------#
# 4. Save data -----
#----------------------------------------------------------#

writexl::write_xlsx(Reptile_Elevations, data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/Reptiles_Checklist_Elevations.xlsx")


