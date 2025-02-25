#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)


# run the Data Preparation file 
source(
  here::here("R/01_Data_processing/03_Reptiles/04_00_reptile_data_prep_for:expert_validation.R")
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
    mean_elevation_treeline = Mean_elevation_treeline
  )|>
  arrange(Mountain_range, desc(max_elevation), desc(overlap_perc_mountain_range))|>
  mutate(
    reviewer_comments = "",
    reviewer_certainty = "",
    reviewer_alpine =""
  )


