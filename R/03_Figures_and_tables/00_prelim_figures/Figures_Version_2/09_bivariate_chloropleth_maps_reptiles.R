
# RUN proportion alpine categories first

#--------------------------------------------------------
# Get dataframe for map
#-----------------------------------------------------------

reptiles_proportion <- proportional_richness_results %>%
  filter(condition == "specialists" & group == "reptiles") %>% # Assuming there is a "group" column for species type
  group_by(Mountain_range) %>%
  summarise(
    generalist_log1p_richness_group = sum(generalist_log1p_richness_group, na.rm = TRUE),
    generalist_richness_group = sum(generalist_richness_group, na.rm = TRUE),
    proportion_specialists_group = sum(proportion_specialists_group, na.rm = TRUE),
    log_prop_specialists_group = sum(log_prop_specialists_group, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Complete the data to ensure all mountain ranges are present
  complete(Mountain_range = unique(proportional_richness_results$Mountain_range), 
           fill = list(
             generalist_log1p_richness_group = 0,
             generalist_richness_group = 0,
             proportion_specialists_group = 0,
             log_prop_specialists_group = 0
           ))


#--------------------------------------------------------
# Create map using the bubbles
#-----------------------------------------------------------

alpine_biome <- sf::st_read(paste(data_storage_path,"Mountains/Suzette_Alpine_Biome/Alpine_Biome_Suzette.shp", sep = "/"))|>
  rename(Mountain_range = MapName)|>
  rename(area_size = Area)|>
  mutate(area_size = round(area_size, 0))|>
  mutate(log_area = log(area_size))|>
  filter(Mountain_range %in% unique(reptiles_proportion$Mountain_range))

alpine_biome<- validate_shapes_individually(alpine_biome)

# Subset alpine biomes with no data, remove geometry, convert area_size to numeric, and calculate centroids
alpine_biome_centroids <- alpine_biome |>
  filter(Mountain_range %in% unique(reptiles_proportion$Mountain_range)) |>
  mutate(area_size = as.numeric(area_size)) |>
  st_centroid()

# Remove geometry and store as a data frame
alpine_biome_df <- alpine_biome_centroids |>
  st_set_geometry(NULL)


# Prepare a data frame for plotting the bubbles
alpine_bubbles_richness <- alpine_biome_df |>
  select(Mountain_range, area_size,log_area) |>
  cbind(st_coordinates(alpine_biome_centroids)) |>
  rename(longitude = X, latitude = Y)|>
  left_join(reptiles_proportion,by= "Mountain_range")|>
  drop_na()|>
  distinct()

# Merge the data with the shapefile
map_data <- alpine_biome %>%
  left_join(reptiles_proportion, by = c("Mountain_range" = "Mountain_range"))



# Classify data into 3x3 grid
map_data_bi <- bi_class(map_data, 
                        x = log_prop_specialists_group, 
                        y = generalist_log1p_richness_group, 
                        style = "equal", # or "jenks"
                        dim = 3)

#--------------------------------------------------------
# Create map on bivariate scale using GMBA shapes as background
#-----------------------------------------------------------

alpine_bubbles_richness <- alpine_bubbles_richness %>%
  bi_class(x = log_prop_specialists_group, y = generalist_log1p_richness_group, style = "equal", dim = 3)

# First Calculate the area_size range 
size_range <- range(alpine_bubbles_richness$area_size, na.rm = TRUE)

# Get world data excluding Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$continent != "Antarctica",]

# Create the map with the background shapes and the bivariate-colored bubbles
x11() # Optional, opens a new plotting window
map_with_bubbles <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray95") + 
  geom_point(data = alpine_bubbles_richness, 
             aes(x = longitude, y = latitude, color = bi_class, size = area_size), 
             alpha = 0.9) +
  # Plot the GMBA shapes as background
  geom_sf(data = alpine_biome, mapping = aes(fill = "grey"), color = NA, size = 0.1,alpha=0.8) +
  # Color fill for the background shapes
  bi_scale_fill(pal = "BlueGold", dim = 3) +
  # Add bubbles at the centroid locations
  
  # Use the same bivariate color scale for the bubbles
  bi_scale_color(pal = "BlueGold", dim = 3) +
  # Adjust the theme
  bi_theme() +
  theme_void() +
  # Control bubble size
  scale_size_continuous(range = c(2, 22),limits = size_range,guide = "none") # Adjust range as needed for better visualization

# Plot the final map with the bubbles
map_with_bubbles

pdf("~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Basemaps/Chloropleth/reptiles_richness.pdf", width = 12, height = 6)
print(map_with_bubbles)
dev.off()