#----------------------------------------------------------#
#   Calculate absolute and relative richness for plants
#----------------------------------------------------------#

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# Load configuration
source(here::here("R/00_Config_file.R"))

# libraries
library(here)
library(tidyverse)
library(purrr)
library(insight)

# Load configuration
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# Load species lists -----
#----------------------------------------------------------#

species_list <- RUtilpol::get_latest_file(
  "species_list",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)

alpine_area_treeline_GIFT <- RUtilpol::get_latest_file(
  "alpine_area_treeline_GIFT",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)

#---------------------------------------------------------------------------------------------#
# Apply the function to calculate absolute and relative richness to all alpine categories 
#----------------------------------------------------------------------------------------------#

alp_generalist <- relative.absolute.richness.plants(species_list, "generalists")  # Applies only the max_elev filter
degr_4 <- relative.absolute.richness.plants(species_list, "treeline_GIFT_4_degree")
degr_2 <- relative.absolute.richness.plants(species_list, "treeline_GIFT_2_degree")
degr_6 <- relative.absolute.richness.plants(species_list, "treeline_GIFT_6_degree")
alp_specialist <- relative.absolute.richness.plants(species_list, "specialists")  

# Combine all results into one dataframe
combined_richness <- bind_rows(
  alp_generalist = alp_generalist, 
  degr_6 = degr_6,
  degr_4 = degr_4, 
  degr_2 = degr_2, 
  alp_specialist = alp_specialist,
  .id = "condition"
) |>
  select(-Mountain_range) |>
  distinct() |>
  mutate(residuals_log = sign(residuals) * log1p(abs(residuals)))|>
  mutate(log_alp_area =log1p(alpine_area))

#--------------------------------------------------------
# Get proportions of each category to overall generalists
#-----------------------------------------------------------
# how much contributes each alpine category to overall mountain generalists?

# get a baseline dataframe for mountain generalists
generalists_baseline <- combined_richness |>
  filter(condition == "alp_generalist") |>
  select(
    geo_entity,  
    generalist_richness = richness,
    generalist_richness_log = richness_log,
    generalist_residuals = residuals,
    generalist_residuals_log = residuals_log
  )

# calculate proportions
proportional_richness_results <- combined_richness |>
  left_join(generalists_baseline, by = "geo_entity") |>
  mutate(
    # Calculate the unlogged proportions for total richness and group-specific richness
    proportion_specialists = ifelse(generalist_richness == 0, 0, (richness / generalist_richness) * 100),
    # Calculate the logged proportions for total richness and group-specific richness
    log_prop_specialists = log1p(proportion_specialists)
  ) |>
  ungroup()

#------------------------------------------------------------------------------------
# Save the results dataframe 
#-----------------------------------------------------------------------------------

RUtilpol::save_latest_file(
  object_to_save = proportional_richness_results,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)
