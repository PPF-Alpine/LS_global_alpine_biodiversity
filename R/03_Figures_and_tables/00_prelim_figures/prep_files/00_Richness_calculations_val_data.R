
# Loop to calculate richness for individual groups and mountain ranges for groups of alpine
# generalists,specialists, degree below treeline and expert opinion

library(dplyr)
library(purrr)
library(insight)

# Define the different filter conditions
filter_conditions <- list(
  generalists = "max_elevation >= mean_treeline",
  specialists = "max_elevation >= mean_treeline & min_elevation >= mean_treeline",
  degree_2 = "max_elevation >= mean_treeline & min_elevation >= Mean_elevation_2_degree",
  degree_4 = "max_elevation >= mean_treeline & min_elevation >= Mean_elevation_4_degree",
  degree_6 = "max_elevation >= mean_treeline & min_elevation >= Mean_elevation_6_degree"
)

# Function to calculate richness for a given condition
calculate_richness <- function(data, condition) {
  total_richness_per_mountain <- data |>
    filter(!!rlang::parse_expr(condition)) |>
    group_by(Mountain_range) |>
    summarise(total_richness = n_distinct(sciname), .groups = "drop")
  
  species_per_group <- data |>
    filter(!!rlang::parse_expr(condition)) |>
    group_by(Mountain_range, group) |>
    summarise(n_group = n_distinct(sciname), .groups = "drop")
  
  species_richness_combined <- species_per_group |>
    left_join(total_richness_per_mountain, by = "Mountain_range") |>
    mutate(proportion = round((n_group / total_richness) * 100, 0)) |>
    mutate(n_group_log = log1p(n_group)) |>
    left_join(area_size, by = "Mountain_range") |>
    select(Mountain_range, group, n_group, total_richness, proportion, everything()) |>
    drop_na()
  
  return(species_richness_combined)
}

# Function to fit SAR, predict richness, and calculate residuals
sar_for_groups <- function(data, group_name) {
  sar_group <- filter(data, group == group_name)
  
  lm_mod <- lm(n_group ~ log1p(area_size), data = sar_group, na.action = na.exclude)
  
  sar_group$predicted_richness <- predict(lm_mod, newdata = sar_group)
  
  sar_group$residuals <- insight::get_residuals(lm_mod, type = "response")
  
  sar_group <- sar_group |>
    mutate(residuals_log = sign(residuals) * log1p(abs(residuals)),
           richness_log = log1p(total_richness))
  
  return(sar_group)
}

# Loop through each condition, calculate richness, and then SAR
all_species_richness_combined <- list()
species_richness_sar <- list()

for (condition_name in names(filter_conditions)) {
  condition <- filter_conditions[[condition_name]]
  
  # Calculate richness
  richness_data <- calculate_richness(checklist_selected, condition)
  all_species_richness_combined[[condition_name]] <- richness_data
  
  # List of groups
  groups <- unique(richness_data$group)
  
  # Apply the SAR function to each group
  sar_data <- map_df(groups, ~sar_for_groups(richness_data, .x))
  species_richness_sar[[condition_name]] <- sar_data
}

# Combine all the richness dataframes
all_species_richness_combined_df <- bind_rows(all_species_richness_combined, .id = "filter_condition")

# Combine all the SAR dataframes
species_richness_sar_df <- bind_rows(species_richness_sar, .id = "filter_condition")

