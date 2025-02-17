
# this script is used to categorize alpine species based on their regional elevataional ranges within each mountain range
# output are dataframes for generalists, 2,4,6 degree below treeline and alpine specialists

#----------------------------#
#     Set up and load data
#----------------------------#

source(here::here("R/00_Config_file.R"))

# libraries
library(here)
library(tidyverse)
library(RUtilpol)

checklist <- get_latest_file("alpine_vertebrate_dataset", 
                dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists"), 
                verbose = TRUE)


#----------------------------#
# Join with treeline elevations
#----------------------------#

# Join with treeline elevations
Treeline_Elevations <- readxl::read_excel("Data/Input/mountain_data/Treeline_Lapse_Rate_04_05.xlsx")

# load the table with the area size of each mountain range
area_size <- readxl::read_excel("Data/Input/mountain_data/Alpine_Biome_Area_size.xlsx")|>
  #readxl::read_excel(paste0(data_storage_path, "Mountains/Suzette_Alpine_biome/Alpine_Biome_Area_size.xlsx")) |>
  select(Mountain_range,area_size)|>
  mutate(log1p_area=log1p(area_size))


alpine_categories <- checklist|>
  left_join(Treeline_Elevations,by = c("Mountain_range","Mountain_system"))|>
  left_join(area_size,by="Mountain_range")|>
  select(-Mean_elevation) |># calculate how many m of species min and max limit is above and below the treeline
  mutate(
    min_rel_treeline = min_elevation - mean_treeline,
    max_rel_treeline = max_elevation - mean_treeline
  )|>
  select(group,sciname,Mountain_system,Mountain_range,mountain_range_ID,alpine_status,expert_validated,
         min_elevation,max_elevation,source_min_elevation,source_max_elevation,log1p_area,area_size,
         mean_treeline,Mean_elevation_1_degree,Mean_elevation_2_degree,
         Mean_elevation_4_degree,Mean_elevation_6_degree,Mean_elevation_8_degree)

#----------------------------#
# Filter for alpine categories
#----------------------------#
# Define the filter conditions
filter_conditions <- list(
  generalists = "max_elevation >= mean_treeline",
  specialists = "max_elevation >= mean_treeline & min_elevation >= mean_treeline",
  degree_2 = "max_elevation >= mean_treeline & min_elevation >= Mean_elevation_2_degree",
  degree_4 = "max_elevation >= mean_treeline & min_elevation >= Mean_elevation_4_degree",
  degree_6 = "max_elevation >= mean_treeline & min_elevation >= Mean_elevation_6_degree"
)

# Use purrr::imap to create a list of dataframes based on filter conditions
alpine_categories_list <- imap(filter_conditions, 
                          function(condition, name) {
                          alpine_categories |>
                          filter(!!rlang::parse_expr(condition))
})

# Now, the resulting alpine_dataframes list will contain your filtered dataframes:
mountain_generalists <- alpine_categories_list$generalists
alpine_specialists <- alpine_categories_list$specialists
UFL_alpine_2_degr <- alpine_categories_list$degree_2
mid_mont_alp_4_degr <- alpine_categories_list$degree_4
broad_mont_alp_6_degr <- alpine_categories_list$degree_6

#----------------------------#
#    Save latest file
#----------------------------#

# save the list of df
RUtilpol::save_latest_file(
  object_to_save = alpine_categories_list,  # Pass the object directly
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/Alpine_categories"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

# save the individual dataframes
RUtilpol::save_latest_file(
  object_to_save = mountain_generalists,  # Pass the object directly
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/Alpine_categories"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = alpine_specialists,  # Pass the object directly
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/Alpine_categories"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = UFL_alpine_2_degr,  # Pass the object directly
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/Alpine_categories"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = mid_mont_alp_4_degr,  # Pass the object directly
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/Alpine_categories"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = broad_mont_alp_6_degr,  # Pass the object directly
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/Alpine_categories"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

