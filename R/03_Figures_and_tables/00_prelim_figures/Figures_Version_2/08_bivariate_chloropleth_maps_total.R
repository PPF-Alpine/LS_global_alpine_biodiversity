library(ggplot2)
library(tidyverse)
library(sf)
library(biscale)
library(rnaturalearth)
library(rnaturalearthdata)

# RUN proportion alpine categories first

#--------------------------------------------------------
# Get dataframe for map
#-----------------------------------------------------------

specialist_proportion <- proportional_richness_results %>%
  group_by(Mountain_range)|>
  select(Mountain_range, 
         condition,
         generalist_log1p_total_richness,
         generalist_total_richness, 
         proportion_total_richness,
         proportion_log1p_total_richness)|>
  distinct()|>
  filter(condition== "specialists")|>
  drop_na()# clean this out - these are rows with no specialists

#--------------------------------------------------------
# Load the shapes
#-----------------------------------------------------------

# load alpine biome 
alpine_polygons <- sf::st_read(paste(data_storage_path,"Mountains/Suzette_Alpine_Biome/Alpine_Biome_Suzette.shp", sep = "/"))|>
  rename(Mountain_range = MapName)|>
  rename(area_size = Area)|>
  mutate(area_size = round(area_size, 0))|>
  mutate(log_area = log(area_size))|>
  filter(Mountain_range %in% unique(specialist_proportion$Mountain_range))

mountain_shapes <- sf::st_read("C:/Users/losch5089/OneDrive - University of Bergen/Desktop/Datasets/Mountains/GMBA_Mountains_v2.0/Level_03/GMBA_Inventory_v2.0_Level_03.shp")|>
  rename(Mountain_range = MapName)|>
  filter(Mountain_range %in% unique(specialist_proportion$Mountain_range))


# Merge the data with the shapefile
map_data <- mountain_shapes %>%
  left_join(specialist_proportion, by = c("Mountain_range" = "Mountain_range"))


#--------------------------------------------------------
# Create bivariate scale
#-----------------------------------------------------------

# Classify data into 3x3 grid
map_data_bi <- bi_class(map_data, 
                        x = proportion_log1p_total_richness, 
                        y = generalist_log1p_total_richness, 
                        style = "equal", # or "jenks"
                        dim = 3)


#--------------------------------------------------------
# Create map on bivariate scale using GMBA shapes as background
#-----------------------------------------------------------
x11()
ggplot() +
  geom_sf(data = map_data_bi, mapping = aes(fill = bi_class), color = "white", size = 0.1) +
  bi_scale_fill(pal = "BlueGold", dim = 3) +
  bi_theme() +
  theme_void()

bi_legend(pal = "BlueGold",
          dim = 3,
          xlab = "Proportion Specialists",
          ylab = "Generalist Total Richness",
          size = 8)

#--------------------------------------------------------
# Create map using the bubbles
#-----------------------------------------------------------

alpine_biome <- sf::st_read(paste(data_storage_path,"Mountains/Suzette_Alpine_Biome/Alpine_Biome_Suzette.shp", sep = "/"))|>
  rename(Mountain_range = MapName)|>
  rename(area_size = Area)|>
  mutate(area_size = round(area_size, 0))|>
  mutate(log_area = log(area_size))|>
  filter(Mountain_range %in% unique(specialist_proportion$Mountain_range))

alpine_biome<- validate_shapes_individually(alpine_biome)

# Subset alpine biomes with no data, remove geometry, convert area_size to numeric, and calculate centroids
alpine_biome_centroids <- alpine_biome |>
  filter(Mountain_range %in% unique(specialist_proportion$Mountain_range)) |>
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
  left_join(specialist_proportion,by= "Mountain_range")|>
  drop_na()|>
  distinct()

# Merge the data with the shapefile
map_data <- alpine_biome %>%
  left_join(specialist_proportion, by = c("Mountain_range" = "Mountain_range"))



# Classify data into 3x3 grid
map_data_bi <- bi_class(map_data, 
                        x = proportion_total_richness, 
                        y = generalist_total_richness, 
                        style = "equal", # or "jenks"
                        dim = 3)

#--------------------------------------------------------
# Create bubble map on bivariate scale
#-----------------------------------------------------------

alpine_bubbles_richness <- alpine_bubbles_richness %>%
  bi_class(x = log_prop_specialists_total, y = generalist_log1p_total_richness,  dim = 3,style="equal")

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

pdf("~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Basemaps/Chloropleth/total_richness.pdf", width = 12, height = 6)
print(map_with_bubbles)
dev.off()

bi_legend(pal = "Brown2",
          dim = 3,
          xlab = "Proportion alpine specialists",
          ylab = "Total species richness",
          size = 8)


#--------------------------------------------------------
# Create map on bivariate scale using GMBA and alpine biome shapes as background
#-----------------------------------------------------------
# Merge the data with the shapefile
map_data_ab <- alpine_biome %>%
  left_join(specialist_proportion, by = c("Mountain_range" = "Mountain_range"))



# Classify data into 3x3 grid
map_data_bi_ab <- bi_class(map_data_ab, 
                        x = proportion_log1p_total_richness, 
                        y = generalist_log1p_total_richness, 
                        style = "equal", # or "jenks"
                        dim = 3)

# Merge the data with the shapefile
map_data_mount <- mountain_shapes %>%
  left_join(specialist_proportion, by = c("Mountain_range" = "Mountain_range"))


# Classify data into 3x3 grid
map_data_bi_mount <- bi_class(map_data_mount, 
                           x = proportion_log1p_total_richness, 
                           y = generalist_log1p_total_richness, 
                           style = "equal", # or "jenks"
                           dim = 3)

#--------------------------------------------------------
# Create map on bivariate scale using GMBA shapes as background
#-----------------------------------------------------------
x11() # Optional, to open a new plotting window
gmbaab<-ggplot() +
  # Plot the mountain shapes in the background with higher transparency
  geom_sf(data = map_data_bi_mount, 
          mapping = aes(fill = bi_class), 
          color = NA, 
          size = 0.1, 
          alpha = 0.5) +  # Set higher transparency for the background
  # Plot the alpine biome shapes on top with lower transparency
  geom_sf(data = map_data_bi_ab, 
          mapping = aes(fill = bi_class), 
          color = NA, 
          size = 0.1, 
          alpha = 1) +  # Set lower transparency for the top layer
  # Apply the same bivariate color scale
  bi_scale_fill(pal = "BlueGold", dim = 3) +
  # Adjust the theme
  bi_theme() +
  theme_void()

bi_legend(pal = "BlueGold",
          dim = 3,
          xlab = "Proportion Specialists",
          ylab = "Generalist Total Richness",
          size = 8)


pdf("~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Basemaps/Chloropleth/gmba_and_ab.pdf", width = 12, height = 6)
print(gmbaab)
dev.off()