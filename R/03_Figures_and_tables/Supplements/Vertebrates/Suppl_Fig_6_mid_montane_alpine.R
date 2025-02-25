#----------------------------#
#     Set up and load data
#----------------------------#

source(here::here("R/00_Config_file.R"))

library(ggplot2)
library(tidyverse)
library(sf)
library(biscale)
library(rnaturalearth)
library(rnaturalearthdata)



proportional_richness_results <- RUtilpol::get_latest_file(
  "proportions_alp_categories",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),
  verbose = TRUE
)

mountain_range_ID <- RUtilpol::get_latest_file(
  "mountain_range_ID",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains"),
  verbose = TRUE
)


#----------------------------#
# Join IDs and filter condition
#----------------------------#

proportional_richness_results <- proportional_richness_results|>
  left_join(mountain_range_ID|>
              select(Mountain_range,mountain_range_ID),
            by="Mountain_range")|>
  filter(filter_condition== "degree_4")


mountain_range_ID<-proportional_richness_results|>
  select(Mountain_range,mountain_range_ID)|>
  distinct()


# Eckert IV projection
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"



#--------------------------------------------------------
# Load the shapes
#--------------------------------------------------------

alpine_biome <- sf::st_read(paste(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/alpine_biome.shp", 
                                  sep = "/")) |>
  rename(Mountain_range = MapName) |>
  rename(area_size = Area) |>
  mutate(area_size = round(area_size, 0)) |>
  mutate(log_area = log(area_size)) |>
  filter(Mountain_range %in% unique(proportional_richness_results$Mountain_range)) |>
  st_transform(crs = eckertIV)  

alpine_biome <- validate_shapes_individually(alpine_biome)


# Calculate centroids in Eckert IV projection
alpine_biome_centroids <- alpine_biome |>
  filter(Mountain_range %in% unique(proportional_richness_results$Mountain_range)) |>
  mutate(area_size = as.numeric(area_size)) |>
  st_centroid() |>
  st_transform(crs = eckertIV)  # Ensure centroids are in Eckert IV projection

# Remove geometry and store as a data frame
alpine_biome_df <- alpine_biome_centroids |>
  st_set_geometry(NULL) |>
  select(Mountain_range, area_size, log_area) |>
  cbind(st_coordinates(alpine_biome_centroids)) |>
  rename(longitude = X, latitude = Y)

# Prepare a data frame for plotting the bubbles
alpine_bubbles_richness <- proportional_richness_results |>
  left_join(alpine_biome_df, by = "Mountain_range") |>
  mutate(log_prop_specialists_group_jitter = log_prop_specialists_group + rnorm(n(), mean = 0, sd = 1e-5)) |>
  distinct()|>
  filter(filter_condition=="degree_6")|>
  distinct(Mountain_range, .keep_all = TRUE)


#alpine_bubbles_richness <- alpine_bubbles_richness|>filter(Mountain_range!="Mongolian Highlands")

# Merge the data with the shapefile
map_data <- alpine_biome |>
  left_join(alpine_bubbles_richness, by = c("Mountain_range" = "Mountain_range"))


#--------------------------------------------------------
# Classify data into 3x3 grid for bivariate mapping
#--------------------------------------------------------

# Classify data into 3x3 grid using quantiles
map_data_bi <- bi_class(map_data, 
                        x = log_prop_specialists_group_jitter,  # Use the jittered version
                        y = generalist_group_residuals, 
                        style = "quantile", 
                        dim = 3)

alpine_bubbles_richness <- alpine_bubbles_richness |>
  bi_class(x = log_prop_specialists_group_jitter, y = generalist_group_residuals, dim = 3, style = "quantile")

#--------------------------------------------------------
# Create bubble map on bivariate scale
#--------------------------------------------------------

# Calculate the total_richness_log range for scaling
size_range <- range(sqrt(alpine_bubbles_richness$generalist_group_richness), na.rm = TRUE)

# Get world data excluding Antarctica and transform to Eckert IV projection
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$continent != "Antarctica", ] |>
  st_transform(crs = eckertIV)  # Transform world map to Eckert IV

#--------------------------------------------------------
# Choose colour
#--------------------------------------------------------

named_colour_vector <- palettes::pal_colour(c(
  "1-1" = "#f3f3f3", # low x, low y
  "2-1" = "#b4d3e1",
  "3-1" = "#509dc2", # high x, low y
  "1-2" = "#f3e6b3",
  "2-2" = "#b3b3b3", # medium x, medium y
  "3-2" = "#376387",
  "1-3" = "#f3b300", # low x, high y
  "2-3" = "#b36600",
  "3-3" = "#000000" # high x, high y
))

plot(named_colour_vector)

#--------------------------------------------------------
# Create Map
#--------------------------------------------------------
# Create the map with the background shapes and the bivariate-colored bubbles
x11()  # Optional, opens a new plotting window
map_with_bubbles <- ggplot2::ggplot() +
  geom_sf(data = world, fill = "gray88", color = "gray88") + 
  # Plot the GMBA shapes as background
  geom_sf(data = alpine_biome, mapping = aes(fill = "gray20"), color = NA, size = 0.1) +
  # Color fill for the background shapes
  bi_scale_fill(pal = named_colour_vector, dim = 3) +
  # Add bubbles at the centroid locations
  geom_point(data = alpine_bubbles_richness, 
             aes(x = longitude, y = latitude, color = bi_class, 
                 size = sqrt(generalist_group_richness)), 
             alpha = 0.9,position = position_jitter(width = 0.5, height = 0.5)) +
  # Add text labels for each point
  geom_text(data = alpine_bubbles_richness, 
            aes(x = longitude, y = latitude, label = mountain_range_ID), 
            size = 3, vjust = 0.5, hjust = 0.5, color = "black") +
  # Use the same bivariate color scale for the bubbles
  bi_scale_color(pal = named_colour_vector, dim = 3) +
  # Adjust the theme
  bi_theme() +
  coord_sf(crs = eckertIV) +
  theme_void() +
  # Control bubble size based on total_richness_log
  scale_size_continuous(range = c(2, 22), limits = size_range, guide = "none") +
  guides(fill = "none", color = "none")

# Plot the final map
map_with_bubbles


# 
# 
output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Verts/mid_montane_4_degr.pdf")

# Save the map to a PDF file
pdf(output_file, width = 11, height = 6)
print(map_with_bubbles)
dev.off()



