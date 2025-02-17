# In this script I calculate species richness in alpine biomes globally for birds, mammals, reptiles combined and individually for each group


## TO DO: shorten script and write functions to R/FUNCTIONS 

#--------------------------------------------------------
# Set up and prepare data
#-----------------------------------------------------------

# no need to run config file for this script (if data storage path not needed)
# Load configuration
source(here::here("R/00_Config_file.R"))

# libraries
library(here)
library(tidyverse)
library(purrr)
library(insight)
library(mgcv)


# run this file to prepare the data
# here I compile the different datasets (i.e., mammals, birds, reptiles) and merge them into one
# in this step also treeline elevations are sourced

source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Results_and_Figures/prep_files/00_Data_Preparation_val_data.R")
)

# Assuming your dataframe is called `df`
excluded_mountain_ranges <- c(
  "Aldan Mountains",
  "Meseta Patagónica",
  "Sikhote-Alin Area",
  "Mediterranean islands",
  "Sierras Pampeanas",
  "Hawaian Islands",
  "Maya Highlands",
  "Balochistan Ranges",
  "Central China Mountains",
  "Central Siberian Plateau",
  "Cordillera de la Costa", 
  "Lena-Angara Plateau", 
  "North Island", 
  "Northern Baikal Mountains", 
  "Selenga Highlands", 
  "Sistema periférico", 
  "Stanovoy Highlands", 
  "Stanovoy Range", 
  "Ural Mountains",
  "South China Mountains"
)

# Use filter() to exclude rows with these mountain ranges
checklist_selected <- checklist_selected %>%
  filter(!(Mountain_range %in% excluded_mountain_ranges))

# Define different categories of species based on their elevational ranges relative to the treeline
filter_conditions <- list(
  generalists = "max_elevation >= mean_treeline",
  specialists = "max_elevation >= mean_treeline & min_elevation >= mean_treeline",
  degree_2 = "max_elevation >= mean_treeline & min_elevation >= Mean_elevation_2_degree",
  degree_4 = "max_elevation >= mean_treeline & min_elevation >= Mean_elevation_4_degree",
  degree_6 = "max_elevation >= mean_treeline & min_elevation >= Mean_elevation_6_degree"
)

#--------------------------------------------------------
# Function to calculate richness (total and for mammals, birds, reptiles individually)
#--------------------------------------------------------

calculate_richness <- function(data, condition) {
  # Get all unique mountain ranges and groups before filtering
  all_mountain_ranges <- unique(data$Mountain_range)
  all_groups <- unique(data$group)
  
  # Step 1: Calculate total richness per mountain
  total_richness_per_mountain <- data |>
    filter(!!rlang::parse_expr(condition)) |>
    group_by(Mountain_range) |>
    summarise(total_richness = n_distinct(sciname), .groups = "drop")
  
  # Ensure all mountain ranges are included, even if total_richness = 0
  total_richness_per_mountain <- total_richness_per_mountain |>
    complete(Mountain_range = all_mountain_ranges, fill = list(total_richness = 0))
  
  # Step 2: Calculate richness per group
  species_per_group <- data |>
    filter(!!rlang::parse_expr(condition)) |>
    group_by(Mountain_range, group) |>
    summarise(richness_group = n_distinct(sciname), .groups = "drop")
  
  # Ensure all mountain ranges and groups are included, even if richness_group = 0
  species_per_group <- species_per_group |>
    complete(Mountain_range = all_mountain_ranges, group = all_groups, fill = list(richness_group = 0))
  
  # Step 3: Combine total richness and group richness, calculate proportions, log-transform
  species_richness_combined <- species_per_group |>
    left_join(total_richness_per_mountain, by = "Mountain_range") |>
    mutate(
      proportion = ifelse(total_richness == 0, 0, round((richness_group / total_richness) * 100, 0)),  # safe proportion calculation
      richness_group_log = log1p(richness_group)  # log-transform richness group
    ) |>
    left_join(area_size, by = "Mountain_range") |>
    select(Mountain_range, group, richness_group, total_richness, proportion, everything()) |>
    drop_na()  # remove rows with missing data
  
  return(species_richness_combined)
}

#--------------------------------------------------------
# Function to fit Species Area Relationship (SAR) and calculate residuals
#--------------------------------------------------------

sar_for_groups <- function(data, group_name) {
  sar_group <- filter(data, group == group_name)
  
  lm_mod <- lm(log1p(richness_group) ~ log1p(area_size), data = sar_group, na.action = na.exclude) # log-log SAR model
  
  sar_group$predicted_group_richness <- exp(predict(lm_mod, newdata = sar_group)) # back-transform from log
  
  sar_group$group_residuals <- insight::get_residuals(lm_mod, type = "response")
  
  sar_group <- sar_group |>
    mutate(
      residuals_log_group = sign(group_residuals) * log1p(abs(group_residuals)),
      total_richness_log = log1p(total_richness)
    )
  
  return(sar_group)
}

#--------------------------------------------------------
# Function to fit SAR for total richness and calculate residuals
#--------------------------------------------------------

sar_for_total_richness <- function(data) {
  # Fit the SAR model using total richness and area size
  lm_mod_total <- lm(log1p(total_richness) ~ log1p(area_size), data = data, na.action = na.exclude)
  
  # Predict total richness using the model
  data$predicted_total_richness <- exp(predict(lm_mod_total, newdata = data))
  
  # Calculate residuals for total richness
  data$total_residuals <- insight::get_residuals(lm_mod_total, type = "response")
  
  # Log-transform the residuals
  data <- data |>
    mutate(
      residuals_log_total = sign(total_residuals) * log1p(abs(total_residuals)),
      total_richness_log = log1p(total_richness)
    )
  
  return(data)
}

#--------------------------------------------------------
# Loop through the different conditions of "alpine"
#--------------------------------------------------------

all_species_richness_combined <- list()
species_richness_sar <- list()
total_richness_sar <- list()  # New list to store total richness SAR data

for (condition_name in names(filter_conditions)) {
  condition <- filter_conditions[[condition_name]]
  
  # Calculate richness
  richness_data <- calculate_richness(checklist_selected, condition)
  all_species_richness_combined[[condition_name]] <- richness_data
  
  # Apply the SAR function to each group
  groups <- unique(richness_data$group)
  sar_data <- map_df(groups, ~sar_for_groups(richness_data, .x))
  species_richness_sar[[condition_name]] <- sar_data
  
  # Calculate SAR and residuals for total richness
  sar_total_data <- sar_for_total_richness(richness_data)
  total_richness_sar[[condition_name]] <- sar_total_data
}

# Combine all group-wise SAR data
species_richness_sar_df <- bind_rows(species_richness_sar, .id = "filter_condition")|>
  group_by(Mountain_range) |>
  filter(!(any(filter_condition == "generalists" & total_richness <= 4))) |>
  ungroup()

# Combine total richness SAR data
total_richness_sar_df <- bind_rows(total_richness_sar, .id = "filter_condition")|>
  group_by(Mountain_range) |>
  filter(!(any(filter_condition == "generalists" & total_richness <= 4))) |>
  ungroup()

# Combine richness data only
species_richness_comb <- bind_rows(all_species_richness_combined, .id = "filter_condition")|>
  group_by(Mountain_range) |>
  filter(!(any(filter_condition == "generalists" & total_richness <= 4))) |>
  ungroup()

# Have all SAR values together
sar_combined <- total_richness_sar_df |> 
  left_join(
    species_richness_sar_df |> 
      select(Mountain_range, group, filter_condition, predicted_group_richness, group_residuals, residuals_log_group),
    by = c("Mountain_range", "group", "filter_condition")
  )|>
  group_by(Mountain_range) |>
  filter(!(any(filter_condition == "generalists" & total_richness <= 4))) |>
  ungroup()



