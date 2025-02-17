
# These are the functions to calculate relative and absolute richness across and within taxonomic vertebrate groups for each mountain range

#----------------------------#
#     Richness Calculation
#----------------------------#

# Function to calculate richness metrics for a given dataframe
relative_absolute_richness_verts <- function(data) {
  total_richness <- data|>
    group_by(Mountain_range)|>
    summarise(total_richness = n_distinct(sciname), .groups = "drop")|>
    complete(Mountain_range = all_mountain_ranges, fill = list(total_richness = 0))
  
  richness_group <- data|>
    group_by(Mountain_range, log1p_area, group)|>
    summarise(richness_group = n_distinct(sciname), .groups = "drop")|>
    complete(Mountain_range = all_mountain_ranges, group = all_groups, fill = list(richness_group = 0))
  
  richness_combined <- richness_group|>
    left_join(total_richness, by = "Mountain_range")|>
    mutate(
      total_richness_log1p = log1p(total_richness),
      richness_group_log1p = log1p(richness_group)
    )|>
    select(Mountain_range,group, richness_group, richness_group_log1p, total_richness, total_richness_log1p)|>
    left_join(area_size, by = "Mountain_range")|>
    drop_na()
}

#----------------------------#
#     SAR Model Fitting
#----------------------------#

# Function to fit SAR for groups and calculate residuals
fit_sar_groups <- function(data, group_name) {
  sar_group <- filter(data, group == group_name)
  lm_mod <- lm(richness_group_log1p ~ log1p_area, data = sar_group, na.action = na.exclude)
  
  sar_group|>
    mutate(
      predicted_group_richness = exp(predict(lm_mod, newdata = sar_group)),
      group_residuals = insight::get_residuals(lm_mod, type = "response")
    )
}

# Function to fit SAR for total richness and calculate residuals and z values
fit_sar_total <- function(data) {
  lm_mod_total <- lm(total_richness_log1p ~ log1p_area, data = data, na.action = na.exclude)
  
  z_value <- coef(lm_mod_total)["log1p_area"]
  
  data|>
    mutate(
      z_value = z_value,
      predicted_total_richness = exp(predict(lm_mod_total, newdata = data)),
      total_residuals = insight::get_residuals(lm_mod_total, type = "response")
    )
}

