

# Use this code to plot the background maps for richness and residual plots 
# the size of the bubbles in the map refer to the size of alpine biomes

library(here)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# Load configuration
source(
  here::here("R/00_Config_file.R")
)


# STEP 1: prepare the data
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Results_and_Figures/prep_files/00_Data_Preparation_val_data.R")
)

# STEP 2: calculate richness
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Results_and_Figures/01_calculate_richness_and_SAR.R")
)



# species_richness_sar_df is the dataframe to work with

#-------------------------------------#
# Prepare the data for the map -----
#--------------------------------------#
# Get world data excluding Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$continent != "Antarctica",]

# load alpine biome Suzette
alpine_biome <- sf::st_read(paste(data_storage_path,"Mountains/Suzette_Alpine_Biome/Alpine_Biome_Suzette.shp", sep = "/"))|>
  rename(Mountain_range = MapName)|>
  rename(area_size = Area)|>
  mutate(area_size = round(area_size, 0))|>
  mutate(log_area = log(area_size))|>
  filter(Mountain_range %in% unique(species_richness_sar_df$Mountain_range))

alpine_biome<- validate_shapes_individually(alpine_biome)

# Subset alpine biomes with no data, remove geometry, convert area_size to numeric, and calculate centroids
alpine_biome_centroids <- alpine_biome |>
  filter(Mountain_range %in% unique(species_richness_sar_df$Mountain_range)) |>
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
  left_join(species_richness_sar_df|>
              select(filter_condition,Mountain_range,total_richness,richness_log),by= "Mountain_range")|>
  drop_na()|>
  distinct()

# Prepare a data frame for plotting the bubbles
alpine_bubbles_residuals <- alpine_biome_df |>
  select(Mountain_range, area_size,log_area) |>
  cbind(st_coordinates(alpine_biome_centroids)) |>
  rename(longitude = X, latitude = Y)|>
  left_join(species_richness_sar_df|>
              select(filter_condition,Mountain_range,residuals,residuals_log),by= "Mountain_range")|>
  drop_na()|>
  distinct()

#--------------------------------------------------------------------------------------------#
# Map for total richness with differently sized bubbles dependent on size of Alpine Biome  -----
#----------------------------------------------------------------------------------------------#

# STEP 3: run the plot functions
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Results_and_Figures/prep_files/00_richness_plot_functions_map.R")
)

# Define the colors for the maps
map_colors <- c("#1A2A5A",
                "#256C85", 
                "#37AF91", 
                "#44BF2F",
                "#A3970E",
                "#F2C12E",
                "#F2994A",
                "#D8461B", 
                "#AD2E24",
                "#4E1703")



# 

# Filter by different group of alpine
# "generalists" "specialists" "degree_2" "degree_4"   "degree_6"  

generate_and_save_maps(alpine_bubbles_richness, alpine_bubbles_residuals, "degree_6", world, alpine_biome, size_range)



#---------------------------------------------#
# Grey background map (if needed)  -----
#----------------------------------------------#

map_empty<-ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray95") + 
  geom_sf(data = alpine_biome,fill="grey",alpha=0.6) + # Plot the shapefile regions
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





#---------------------------------------------#
# To plot maps individually  -----
#----------------------------------------------#


alpine_bubbles_richness_filtered <- alpine_bubbles_richness|>filter(filter_condition=="expert_opinion")

# plot the map
map_ric<-ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray95") + 
  geom_sf(data = alpine_biome,fill="grey",alpha=0.6) + # Plot the shapefile regions
  geom_point(data = alpine_bubbles_richness_filtered, aes(x = longitude, y = latitude, size = area_size,fill=richness_log),
             shape = 21,alpha = 0.6) +
  scale_size(range = c(2, 20), name="Area size alpine biome",guide=FALSE) +
  scale_fill_gradientn(colors = c("#1A2A5A",
                                  "#256C85", 
                                  "#37AF91", 
                                  "#44BF2F",
                                  "#A3970E",
                                  "#F2C12E",
                                  "#F2994A",
                                  "#D8461B", 
                                  "#AD2E24",
                                  "#4E1703")) +
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
  ) +
  labs(title = "total species richness (all groups)", 
       caption = paste0("Bubble size displayed according to area size of the alpine biome: ",
                        min(alpine_biome$area_size),"km2 -", max(alpine_biome$area_size),"km2."))



x11()
plot(map_ric)
