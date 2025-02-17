#----------------------------#
#     Set up and load data
#----------------------------#

source(here::here("R/00_Config_file.R"))

library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(insight)

# Load checklist of dataframes
checklist <- get_latest_file(
  "alpine_categories_list",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Processed"),
  verbose = TRUE
)

# get all unique mountain ranges and groups 
all_mountain_ranges <- unique(checklist$generalists$Mountain_range)
all_groups <- unique(checklist$generalists$group)

# Load area size data and calculate log1p of area size
area_size <- readxl::read_excel(file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/Alpine_Biome_Area_size.xlsx"))|>
  select(Mountain_range, area_size)|>
  mutate(log1p_area = log1p(area_size))


#----------------------------#
#     Process Checklist
#----------------------------#

# Process each dataframe in checklist and store all results in final_results
final_results <- checklist|>
  imap_dfr(function(data, condition_name) {
    # Calculate richness
    richness_data <- relative_absolute_richness_verts(data)
    
    # Apply SAR function to each group
    sar_data <- map_df(unique(richness_data$group), ~fit_sar_groups(richness_data, .x))
    
    # Calculate SAR and residuals for total richness
    sar_total_data <- fit_sar_total(richness_data)
    
    # Combine SAR results for group and total, adding filter condition
    combined_data <- sar_total_data|>
      left_join(sar_data|> select(Mountain_range,group, predicted_group_richness, group_residuals), 
                by = c("Mountain_range", "group"))|>
      mutate(filter_condition = condition_name)
    
    combined_data
  })


#----------------------------#
#     Final Data Structure
#----------------------------#

# Arrange final_results with the specified column order
final_results <- final_results|>
  relocate(
    Mountain_range, 
    area_size, 
    log1p_area, 
    group, 
    filter_condition, 
    total_richness, 
    total_richness_log1p, 
    richness_group, 
    richness_group_log1p, 
    predicted_total_richness, 
    predicted_group_richness, 
    total_residuals, 
    group_residuals,
    z_value)


#----------------------------#
#    Save latest file
#----------------------------#

# ✅richness_sar contains absolute and relatvie vertebrate richness total and for each taxonomic group within mountain ranges

# save 
RUtilpol::save_latest_file(
  object_to_save = final_results,  
  file_name = "richness_sar",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

# save 
RUtilpol::save_latest_file(
  object_to_save = sar_models,  
  file_name = "sar_models_verts",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)
