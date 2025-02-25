# RUN : 12_prep_bivariate_chloropleth_residuals
library(ggplot2)
library(tidyverse)
library(sf)
library(biscale)
library(rnaturalearth)
library(rnaturalearthdata)
#--------------------------------------------------------
# Get proportions of each category to overall generalists
#-----------------------------------------------------------
# Step 1: Extract the baseline values for generalists
generalists_baseline <- sar_combined %>%
  filter(filter_condition == "generalists") %>%
  select(Mountain_range, group, 
         generalist_total_richness = total_richness,
         generalist_total_richness_log = total_richness_log,
         generalist_richness_group = richness_group,
         generalist_group_richness_log = richness_group_log,
         generalist_total_residuals = total_residuals,
         generalist_total_residuals_log =residuals_log_total,
         generalist_group_residuals = group_residuals,
         generalist_group_residuals_log =residuals_log_group)

# Step 2: Join the baseline values back to the original dataframe and calculate proportions
proportional_richness_results <- sar_combined %>%
  left_join(generalists_baseline, by = c("Mountain_range", "group")) %>%
  mutate(
    # Calculate the unlogged proportions for total richness and group-specific richness
    proportion_4_degree_total = ifelse(generalist_total_richness == 0, 0, (total_richness / generalist_total_richness) * 100),
    proportion_4_degree_group = ifelse(generalist_richness_group == 0, 0, (richness_group / generalist_richness_group) * 100),
    
    # Calculate the logged proportions for total richness and group-specific richness
    log_prop_4_degree_total = log1p(proportion_4_degree_total), # log transform of proportion
    log_prop_4_degree_group = log1p(proportion_4_degree_group)  # log transform of proportion
  ) %>%
  ungroup()



#--------------------------------------------------------
# Get dataframe for map
#-----------------------------------------------------------

alp_generalist_4_degree <- proportional_richness_results %>%
  group_by(Mountain_range)|>
  select(Mountain_range, 
         filter_condition,
         generalist_total_residuals,
         generalist_total_residuals_log,
         generalist_total_richness,
         generalist_total_richness_log,
         total_richness_log,
         total_richness,
         total_residuals, 
         residuals_log_total,
         proportion_4_degree_total, log_prop_4_degree_total
  )|>
  distinct()|>
  filter(filter_condition== "degree_4")|>
  drop_na()# clean this out - these are rows with no specialists



#--------------------------------------------------------
# Load the shapes
#-----------------------------------------------------------

alpine_biome <- sf::st_read(paste(data_storage_path, "Mountains/Suzette_Alpine_Biome/Alpine_Biome_Suzette.shp", sep = "/")) %>%
  rename(Mountain_range = MapName) %>%
  rename(area_size = Area) %>%
  mutate(area_size = round(area_size, 0)) %>%
  mutate(log_area = log(area_size)) %>%
  filter(Mountain_range %in% unique(alp_generalist_4_degree$Mountain_range))

alpine_biome <- validate_shapes_individually(alpine_biome)

# Subset alpine biomes with no data, remove geometry, convert area_size to numeric, and calculate centroids
alpine_biome_centroids <- alpine_biome %>%
  filter(Mountain_range %in% unique(alp_generalist_4_degree$Mountain_range)) %>%
  mutate(area_size = as.numeric(area_size)) %>%
  sf::st_centroid()

# Remove geometry and store as a data frame
alpine_biome_df <- alpine_biome_centroids %>%
  st_set_geometry(NULL)

# Prepare a data frame for plotting the bubbles
alpine_bubbles_richness <- alpine_biome_df %>%
  select(Mountain_range, area_size, log_area) %>%
  cbind(st_coordinates(alpine_biome_centroids)) %>%
  rename(longitude = X, latitude = Y) %>%
  left_join(alp_generalist_4_degree, by = "Mountain_range") %>%
  drop_na() %>%
  distinct()|>
  mutate(log_prop_specialists_total_jitter = log_prop_4_degree_total + rnorm(n(), mean = 0, sd = 1e-5))

# Merge the data with the shapefile
map_data <- alpine_biome %>%
  left_join(alp_generalist_4_degree, by = c("Mountain_range" = "Mountain_range"))|>
  mutate(log_prop_specialists_total_jitter = log_prop_4_degree_total + rnorm(n(), mean = 0, sd = 1e-5))

#--------------------------------------------------------
# Classify data into 3x3 grid for bivariate mapping
#-----------------------------------------------------------

# Classify data into 3x3 grid using quantiles
map_data_bi <- bi_class(map_data, 
                        x = log_prop_specialists_total_jitter,  # Use the jittered version
                        y = generalist_total_residuals, 
                        style = "quantile", 
                        dim = 3)

alpine_bubbles_richness <- alpine_bubbles_richness %>%
  bi_class(x = log_prop_specialists_total_jitter, y = generalist_total_residuals, dim = 3, style = "quantile")




#--------------------------------------------------------
# Create bubble map on bivariate scale
#-----------------------------------------------------------


#https://cartoscience.github.io/bivariate-color-matrix/

# First Calculate the total_richness_log range for scaling
size_range <- range(sqrt(alpine_bubbles_richness$generalist_total_richness), na.rm = TRUE)

#size_range <- range(alpine_bubbles_richness$generalist_total_richness, na.rm = TRUE)

# Get world data excluding Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$continent != "Antarctica", ]

# Create the map with the background shapes and the bivariate-colored bubbles
x11()  # Optional, opens a new plotting window
map_with_bubbles <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray95") + 
  # Plot the GMBA shapes as background
  geom_sf(data = alpine_biome, mapping = aes(fill = "grey"), color = NA, size = 0.1, alpha = 0.8) +
  # Color fill for the background shapes
  bi_scale_fill(pal = "Brown2", dim = 3) +
  # Add bubbles at the centroid locations
  geom_point(data = alpine_bubbles_richness, 
             aes(x = longitude, y = latitude, color = bi_class, 
                 size = sqrt(generalist_total_richness)), 
             #size = generalist_total_richness), 
             alpha = 0.9) +
  # Use the same bivariate color scale for the bubbles
  bi_scale_color(pal = "Brown2", dim = 3) +
  # Adjust the theme
  bi_theme() +
  theme_void() +
  # Control bubble size based on total_richness_log
  scale_size_continuous(range = c(2,22), limits = size_range, guide = "none") +
  guides(fill = "none", color = "none")

# Plot the final map
map_with_bubbles


# 
pdf("~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Basemaps/Chloropleth/vertebrates_4_degree.pdf", width = 11, height = 6)

# 
print(map_with_bubbles)

# 
dev.off()


# Plot the final map with the bubbles
map_with_bubbles




