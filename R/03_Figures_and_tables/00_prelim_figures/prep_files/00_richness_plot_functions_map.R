
library(dplyr)
library(ggplot2)
library(sf)


# First Calculate the area_size range for "generalists" as reference size
generalists_data <- alpine_bubbles_richness %>%
  filter(filter_condition == "generalists")
size_range <- range(generalists_data$area_size, na.rm = TRUE)

# Function to generate and save maps for richness and residuals
generate_and_save_maps <- function(alpine_bubbles_richness, alpine_bubbles_residuals, filter_condition, world, alpine_biome, size_range) {
  # Filter the data based on the filter condition
  alpine_bubbles_richness_filtered <- alpine_bubbles_richness %>%
    filter(filter_condition == !!filter_condition)
  
  alpine_bubbles_residuals_filtered <- alpine_bubbles_residuals %>%
    filter(filter_condition == !!filter_condition)
  
  # Plot the richness map
  map_ric <- ggplot() +
    geom_sf(data = world, fill = "gray95", color = "gray95") + 
    geom_sf(data = alpine_biome, fill = "grey", alpha = 0.6) + # Plot the shapefile regions
    geom_point(data = alpine_bubbles_richness_filtered, aes(x = longitude, y = latitude, size = area_size, fill = richness_log),
               shape = 21, alpha = 0.6) +
    scale_size(range = c(2, 20), limits = size_range, name = "Area size alpine biome", guide = FALSE) +
    scale_fill_gradientn(colors = map_colors) +
    coord_sf() + # Adjust CRS if needed based on your shapefile
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 20),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) 
  
  # Plot the residuals map with a fixed color scale
  map_res <- ggplot() +
    geom_sf(data = world, fill = "gray95", color = "gray95") + 
    geom_sf(data = alpine_biome, fill = "grey", alpha = 0.6) + # Plot the shapefile regions
    geom_point(data = alpine_bubbles_residuals_filtered, aes(x = longitude, y = latitude, size = area_size, fill = residuals_log),
               shape = 21, alpha = 0.6) +
    scale_size(range = c(2, 20), limits = size_range, name = "Area size alpine biome", guide = FALSE) +
    scale_fill_gradientn(colors = map_colors, limits = c(-1.54, 1.33)) +
    coord_sf() + # Adjust CRS if needed based on your shapefile
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 20),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
  
  output_dir <- "~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Basemaps"
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Save the richness map
  richness_filename <- paste0(output_dir, "/", filter_condition, "_richness.pdf")
  pdf(richness_filename, width = 12, height = 6)
  print(map_ric)
  dev.off()
  
  # Save the residuals map
  residuals_filename <- paste0(output_dir, "/", filter_condition, "_residuals.pdf")
  pdf(residuals_filename, width = 12, height = 6)
  print(map_res)
  dev.off()
}


