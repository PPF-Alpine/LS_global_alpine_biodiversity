

# RUN prep biv chloropleth residuals first
library(biscale)
library(sf)
#--------------------------------------------------------
# Get dataframe for each group
#-----------------------------------------------------------
generalists_baseline <- sar_combined %>%
  filter(filter_condition == "generalists") %>%
  select(Mountain_range, group, 
         generalist_total_richness_log = total_richness_log,
         generalist_total_residuals_log =residuals_log_total,
         generalist_group_richness_log = richness_group_log,
         generalist_group_residuals_log =residuals_log_group)

# Step 2: Join the baseline values back to the original dataframe and calculate proportions
bivariate_residuals <- sar_combined %>%
  left_join(generalists_baseline, by = c("Mountain_range", "group"))

#--------------------------------------------------------
# Get dataframe for map
#-----------------------------------------------------------

mammals_res <- bivariate_residuals %>%
  filter(filter_condition == "specialists" & group == "mammals")|>
  select(Mountain_range, 
         group,
         filter_condition,
         generalist_group_residuals_log,
         residuals_log_group
  )|>
  distinct()|>
  drop_na()# clean this out - these are rows with no specialists

#--------------------------------------------------------
# Load the shapes
#-----------------------------------------------------------

alpine_biome <- sf::st_read(paste(data_storage_path,"Mountains/Suzette_Alpine_Biome/Alpine_Biome_Suzette.shp", sep = "/"))|>
  rename(Mountain_range = MapName)|>
  rename(area_size = Area)|>
  mutate(area_size = round(area_size, 0))|>
  mutate(log_area = log(area_size))|>
  filter(Mountain_range %in% unique(mammals_res$Mountain_range))

alpine_biome<- validate_shapes_individually(alpine_biome)

# Subset alpine biomes with no data, remove geometry, convert area_size to numeric, and calculate centroids
alpine_biome_centroids <- alpine_biome |>
  filter(Mountain_range %in% unique(mammals_res$Mountain_range)) |>
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
  left_join(mammals_res,by= "Mountain_range")|>
  drop_na()|>
  distinct()

# Merge the data with the shapefile
map_data <- alpine_biome %>%
  left_join(mammals_res, by = c("Mountain_range" = "Mountain_range"))



# Classify data into 3x3 grid
map_data_bi <- bi_class(map_data, 
                        x = residuals_log_group, 
                        y = generalist_group_residuals_log, 
                        style = "quantile", # or "jenks"
                        dim = 3)

#--------------------------------------------------------
# Create bubble map on bivariate scale
#-----------------------------------------------------------

alpine_bubbles_richness <- alpine_bubbles_richness %>%
  bi_class(x = residuals_log_group, y = generalist_group_residuals_log,  dim = 3,style="quantile")

range(alpine_bubbles_richness$residuals_log_group)
range(alpine_bubbles_richness$generalist_group_residuals_log)

# First Calculate the area_size range 
size_range <- range(alpine_bubbles_richness$area_size, na.rm = TRUE)

# Get world data excluding Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$continent != "Antarctica",]

# Create the map with the background shapes and the bivariate-colored bubbles
x11() # Optional, opens a new plotting window
map_with_bubbles <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray95") + 
  # Plot the GMBA shapes as background
  geom_sf(data = alpine_biome, mapping = aes(fill = "grey"), color = NA, size = 0.1,alpha=0.8) +
  # Color fill for the background shapes
  bi_scale_fill(pal = "BlueGold", dim = 3) +
  # Add bubbles at the centroid locations
  geom_point(data = alpine_bubbles_richness, 
             aes(x = longitude, y = latitude, color = bi_class, size = area_size), 
             alpha = 0.9) +
  # Use the same bivariate color scale for the bubbles
  bi_scale_color(pal = "BlueGold", dim = 3) +
  # Adjust the theme
  bi_theme() +
  theme_void() +
  # Control bubble size
  scale_size_continuous(range = c(2, 22),limits = size_range,guide = "none") # Adjust range as needed for better visualization

# Plot the final map with the bubbles
map_with_bubbles

bi_legend(pal = "BlueGold",
          dim = 3,
          xlab = "Residuals Specialists",
          ylab = "Residuals Generalists",
          size = 8)

pdf("~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Basemaps/Chloropleth/mammals_residuals.pdf", width = 12, height = 6)
print(map_with_bubbles)
dev.off()