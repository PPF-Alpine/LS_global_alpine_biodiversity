

# Load checklist of dataframes
sar_models <- get_latest_file(
  "sar_models_verts",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),
  verbose = TRUE
)


final_results <- get_latest_file(
  "richness_sar",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),
  verbose = TRUE
)

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



output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Verts/SAR_verts.pdf")
# Save the richness plot
ggsave(
  filename = paste0(output_file), 
  plot = sar_plot, 
  width = 7,  # Increase width to ensure better aspect ratio
  height = 5, # Keep height equal to width for a square
  dpi = 500
)
