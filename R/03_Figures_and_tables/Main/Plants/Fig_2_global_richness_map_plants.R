


library(biscale)

# run this file to prep data for the map

source(here::here("R/03_Figures_and_tables/Main/Plants/01_prep_bivariate_map_plants.R"))


#--------------------------------------------------------
# Create bubble map on bivariate scale
#--------------------------------------------------------

# Define named color palette for bivariate mapping
named_colour_vector <- palettes::pal_colour(c(
  "1-1" = "#f3f3f3", # Low X, Low Y
  "2-1" = "#b4d3e1", # Medium X, Low Y
  "3-1" = "#509dc2", # High X, Low Y
  "1-2" = "#f3e6b3", # Low X, Medium Y
  "2-2" = "#b3b3b3", # Medium X, Medium Y
  "3-2" = "#376387", # High X, Medium Y
  "1-3" = "#f3b300", # Low X, High Y
  "2-3" = "#b36600", # Medium X, High Y
  "3-3" = "#000000"  # High X, High Y
))


plot(named_colour_vector)

#--------------------------------------------------------
# Calculate range for scaling bubble sizes
#--------------------------------------------------------

# Bubble sizes are scaled based on square root of generalist richness
size_range <- range(sqrt(map_data$generalist_richness), na.rm = TRUE)

#--------------------------------------------------------
# Generate map with bubbles Eckert IV projection
#--------------------------------------------------------

x11()

# Build the map
map_with_bubbles <- ggplot() +
  # Background: world map
  geom_sf(data = world, fill = "gray88", color = "gray88") +
  # Overlay: alpine biome polygons
  geom_sf(data = alpine_biome_filtered, aes(fill = "grey20"), color = NA, size = 0.1, alpha = 1) +
  # Add text labels for GIFT IDs
  geom_text(data = map_data_sf, 
            aes(x = longitude, y = latitude, label = GIFT_ID), 
            size = 3, vjust = -1, hjust = 0.5, color = "black") +
  # Add bivariate color scale
  bi_scale_fill(pal = named_colour_vector, dim = 3) +
  # Plot bubbles with size and bivariate classification
  geom_point(data = map_data_sf, 
             aes(x = longitude, y = latitude, color = bi_class, size = sqrt(generalist_richness)), 
             alpha = 0.9) +
  # Add bivariate color scale to bubble borders
  bi_scale_color(pal = named_colour_vector, dim = 3) +
  # Scale bubble sizes
  scale_size_continuous(range = c(2, 22), limits = size_range, guide = "none") +
  # Set coordinate system to Eckert IV projection
  coord_sf(crs = eckertIV) +
  # Apply bivariate map theme
  bi_theme() +
  theme_void() +
  guides(fill = "none", color = "none")


print(map_with_bubbles)

#--------------------------------------------------------
# Create bivariate legend
#--------------------------------------------------------

x11()

# Generate legend for bivariate map
bi_legend(
  pal = named_colour_vector,
  dim = 3,
  xlab = "Proportion of Specialists",  # Label for X-axis
  ylab = "Residuals (Log Total)",      # Label for Y-axis
  size = 8,
  flip_axes = FALSE,                   
  rotate_pal = FALSE                   
)

#--------------------------------------------------------
# Save map as PDF
#--------------------------------------------------------

output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Main/map_generalists_plants.pdf")


# Save the map to a PDF file
pdf(output_file, width = 11, height = 6)
print(map_with_bubbles)
dev.off()
