


# Load checklist of dataframes
richness_sar <- RUtilpol::get_latest_file(
  "richness_sar",
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/richness_sar_results"),
  verbose = TRUE
)

library(dplyr)


#--------------------------------------------------------
# Calculate Proportions of Each Category to Generalists
#--------------------------------------------------------

# Extract baseline values for generalists
generalists_baseline <- richness_sar|>
  filter(filter_condition == "generalists")|>
  select(
    Mountain_range, group, 
    generalist_total_richness = total_richness,
    generalist_total_richness_log1p = total_richness_log1p,
    generalist_group_richness = richness_group,
    generalist_group_richness_log1p = richness_group_log1p,
    generalist_total_residuals = total_residuals,
    generalist_group_residuals = group_residuals
  )

# Join baseline values and calculate proportions
proportional_richness_results <- richness_sar|>
  left_join(generalists_baseline, by = c("Mountain_range", "group"))|>
  mutate(
    proportion_specialists_total = ifelse(generalist_total_richness == 0, 0, (total_richness / generalist_total_richness) * 100),
    log_prop_specialists_total = log1p(proportion_specialists_total),
    proportion_specialists_group = ifelse(generalist_group_richness == 0, 0, (richness_group / generalist_group_richness) * 100),
    log_prop_specialists_group = log1p(proportion_specialists_group)
  )|>
  ungroup()


#----------------------------#
#    Save latest file
#----------------------------#

# save the list of df
RUtilpol::save_latest_file(
  object_to_save = proportional_richness_results,  
  file_name = "proportions_alp_categories",
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/richness_sar_results"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)
