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
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/Alpine_categories"),
  verbose = TRUE
)

# Define all unique mountain ranges and groups based on a sample entry (e.g., 'generalists')
all_mountain_ranges <- unique(checklist$generalists$Mountain_range)
all_groups <- unique(checklist$generalists$group)

# Load area size data and calculate log1p of area size
area_size <- read_excel("Data/Input/mountain_data/Alpine_Biome_Area_size.xlsx")|>
  select(Mountain_range, area_size)|>
  mutate(log1p_area = log1p(area_size))

#----------------------------#
#     Richness Calculation
#----------------------------#

# Function to calculate richness metrics for a given dataframe
calculate_richness <- function(data) {
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

#----------------------------#
#     Process Checklist
#----------------------------#

# Process each dataframe in checklist and store all results in final_results
final_results <- checklist|>
  imap_dfr(function(data, condition_name) {
    # Calculate richness
    richness_data <- calculate_richness(data)
    
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
    Mountain_range, area_size, log1p_area, group, filter_condition, 
    total_richness, total_richness_log1p, richness_group, richness_group_log1p, 
    predicted_total_richness, predicted_group_richness, total_residuals, group_residuals,z_value)

#----------------------------#
#     Get the Z Value for each category
#----------------------------#

# Extract unique z values and actual SAR data
z_values <- final_results|>
  group_by(filter_condition)|>
  distinct(z_value)

#----------------------------#
#   Get the actual SAR plots
#----------------------------#

# Generate predicted SAR values for each condition
sar_models <- final_results|>
  group_by(filter_condition)|>
  nest()|>
  mutate(
    model = map(data, ~lm(total_richness_log1p ~ log1p_area, data = .x)),
    predictions = map2(data, model, ~mutate(.x, predicted_sar = predict(.y, newdata = .x)))
  )|>
  unnest(predictions)

sar_models <- sar_models|>
  mutate(filter_condition = recode(
    filter_condition,
    "generalists" = "mountain generalist",
    "degree_6" = "broad montane alpine",
    "degree_4" = "mid-montane-alpine",
    "degree_2" = "UFL-alpine",
    "specialists" = "alpine specialist"
  ))

# Plot the actual SAR models
sar_plot <- ggplot(sar_models, aes(x = log1p_area, y = total_richness_log1p)) +
  geom_point(aes(color = filter_condition), alpha = 0.2, size = 3) +  # Transparent points
  geom_line(aes(y = predicted_sar, color = filter_condition), size = 1.2) +  # SAR model lines
  scale_color_manual(
    values = c(
      "mountain generalist" = "#006400",     # Dark Green
      "broad montane alpine" = "grey",      # Grey
      "mid-montane-alpine" = "#A0522D",     # Light Brown
      "UFL-alpine" = "darkmagenta",         # Magenta
      "alpine specialist" = "black"         # Black
    ),
    limits = c(
      "mountain generalist",
      "broad montane alpine",
      "mid-montane-alpine",
      "UFL-alpine",
      "alpine specialist"
    )  # Custom legend order
  ) +
  labs(
    x = "alpine area (log1p)",
    y = "Absolute richness (log1p)",
    color = "Alpine Categories"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),  # Add x and y axis lines
    axis.ticks = element_line(color = "black"),  # Add axis ticks
    axis.title = element_text(size = 14,),  # Customize axis title
    axis.text = element_text(size = 12),  # Customize axis text
    panel.grid = element_blank(),
    legend.position = "right",
    text = element_text(size = 12)
  )+
  scale_y_continuous(
    breaks = c(1,3,5,7,9),  # Set custom y-axis breaks
    limits = c(-1,9))

# Display the SAR plot
print(sar_plot)

print(z_values)

desktop_path<-"~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/"
ggsave(filename = paste0(desktop_path, "SAR_verts.png"), plot = sar_plot, width = 5, height = 5, dpi = 300) 

# Save the richness plot
ggsave(
  filename = paste0(desktop_path, "SAR_verts.pdf"), 
  plot = sar_plot, 
  width = 7,  # Increase width to ensure better aspect ratio
  height = 5, # Keep height equal to width for a square
  dpi = 500
)
#----------------------------#
#    Save latest file
#----------------------------#

# save the list of df
RUtilpol::save_latest_file(
  object_to_save = final_results,  
  file_name = "richness_sar",
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/richness_sar_results"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

# save the list of df
RUtilpol::save_latest_file(
  object_to_save = sar_models,  
  file_name = "sar_models_verts",
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists/richness_sar_results"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)
