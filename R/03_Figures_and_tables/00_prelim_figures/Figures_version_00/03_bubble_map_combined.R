# I have differently sized polygons of the alpine biome - find the relationship between area size and species richness

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gridExtra)
library(sf)
library(scales)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

# Run the test Preparation file 
# this script reformats the mammal test
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/00_Biodiversity_data_preparation.R")
)


# load the table with the area size of each mountain range
area_size <- readxl::read_excel(paste0(data_storage_path, "Mountains/Suzette_Alpine_biome/Alpine_Biome_Area_size.xlsx")) |>
  select(Mountain_range,area_size)|>
  mutate(log_area=log1p(area_size))


#----------------------------------------------------------#
# Calculate richness for each mountain range  -----
#----------------------------------------------------------#
checklist_selected <- checklist_combined|>
  filter(Mountain_system!="East Siberian Mountains",
         Mountain_system!="Central and Northern Siberian Mountains",
         Mountain_system!="Svalbard",
         Mountain_system!="North America Arctic Islands",
         Mountain_system!="North America Plains",
         Mountain_range!="Alaska-Yukon Ranges")

# calculate the total richness per mountain range
total_richness_per_mountain <- checklist_selected |>
  filter(max_elevation_USE >= Mean_elevation_treeline) |>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_2_degree)|>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_treeline)|>
  group_by(Mountain_range) |>
  summarise(total_richness = n_distinct(sciname), .groups = "drop")

#  number of species per group for each mountain range
species_per_group <- checklist_selected |>
  filter(max_elevation_USE >= Mean_elevation_treeline) |>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_treeline)|>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_2_degree)|>
  group_by(Mountain_range, group) |>
  summarise(n_group = n_distinct(sciname), .groups = "drop")

# calculate the proportion of each group in overall richness
species_richness_combined <- species_per_group |>
  left_join(total_richness_per_mountain, by = "Mountain_range") |>
  mutate(proportion = round((n_group / total_richness)*100),0) |>
  mutate(n_group_log = log1p(n_group))|>
  left_join(area_size, by = "Mountain_range") |>
  select(Mountain_range, group, n_group, total_richness, proportion, everything()) |>
  drop_na()

#----------------------------------------------------------#
# Fit a linear model specis richness ~ area size -----
#----------------------------------------------------------#

# Fit a lm
lm_mod <- lm(log1p(total_richness)~log1p(area_size),data=species_richness_combined,na.action=na.exclude)

# Calculate predicted richness
# model predicts value of log(1+ richness) use exp to transform predictions back to the original scale
species_richness_combined$predicted_richness <- exp(predict(lm_mod, newdata = species_richness_combined)) 

# Calculate residuals
species_richness_combined$residuals <- insight::get_residuals(lm_mod,type="response")

# log the residuals and richness 
species_richness_combined <- species_richness_combined|> 
  mutate(residuals_log = sign(residuals) * log1p(abs(residuals)))|> # to keep the sign of residuals
  mutate(richness_log = log1p(total_richness))

plot(log1p(total_richness)~log1p(area_size),data=species_richness_combined)
abline(lm)
title(main = "Biodiversity combined - lm")

#-------------------
# linear model for all groups
#-------------------------------

# Function to fit model, predict richness, and calculate residuals for a specific group
sar_for_groups <- function(data, group_name) {
  # Filter data for the group
  test_group <- filter(data, group == group_name)
  
  # Fit the linear model
  lm_mod <- lm(log1p(n_group) ~ log1p(area_size), data = test_group, na.action = na.exclude)
  
  # Calculate predicted richness and transform back
  test_group$predicted_richness <- exp(predict(lm_mod, newdata = test_group))
  
  
  # Calculate residuals
  test_group$residuals <- insight::get_residuals(lm_mod, type = "response")
  
  # Log the residuals and richness
  test_group <- test_group |>
    mutate(residuals_log = sign(residuals) * log1p(abs(residuals)),
           richness_log = log1p(total_richness))
  
  # Return the modified group test
  test_group
}

# List of groups
groups <- unique(species_richness_combined$group)

# Apply the function to each group and bind the results into a single testframe
species_richness_groups <- map_df(groups, ~sar_for_groups(species_richness_combined, .x))


#------------------------------------------------------------------------------------------#
# Create a map with bubble size on alpine biome respective to area size of alpine biome -----
#-------------------------------------------------------------------------------------------#
# Get world data excluding Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$continent != "Antarctica",]

# load alpine biome Suzette
alpine_biome <- sf::st_read(paste(data_storage_path,"Mountains/Suzette_Alpine_Biome/Alpine_Biome_Suzette.shp", sep = "/"))|>
  rename(Mountain_range = MapName)|>
  rename(area_size = Area)|>
  mutate(area_size = round(area_size, 0))|>
  mutate(log_area = log(area_size))

alpine_biome<- validate_shapes_individually(alpine_biome)

# subset alpine biomes with no data
data<-unique(species_richness_combined$Mountain_range)

alpine_biome <- alpine_biome |> filter(Mountain_range %in% data)

alpine_biome_df <- alpine_biome |>   sf::st_set_geometry(NULL)

alpine_biome$area_size <- as.numeric(alpine_biome$area_size)

# Calculate the centroids of the alpine_biome polygons
alpine_biome_centroids <- st_centroid(alpine_biome)

#----------------------------------#
# Bubble map for total richness  -----
#----------------------------------#

# Prepare a data frame for plotting the bubbles
alpine_bubbles_richness <- alpine_biome |>
  st_set_geometry(NULL) |>
  select(Mountain_range, area_size,log_area) |>
  cbind(st_coordinates(alpine_biome_centroids)) |>
  rename(longitude = X, latitude = Y)|>
  left_join(species_richness_combined|>
              select(Mountain_range,total_richness,richness_log),by= "Mountain_range")|>
  drop_na()|>
  distinct()

# Prepare a data frame for plotting the bubbles
alpine_bubbles_residuals <- alpine_biome |>
  st_set_geometry(NULL) |>
  select(Mountain_range, area_size,log_area) |>
  cbind(st_coordinates(alpine_biome_centroids)) |>
  rename(longitude = X, latitude = Y)|>
  left_join(species_richness_combined|>
              select(Mountain_range,residuals,residuals_log),by= "Mountain_range")|>
  drop_na()|>
  distinct()

# plot the map
map_ric<-ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray95") + 
  geom_sf(data = alpine_biome,fill="grey",alpha=0.6) + # Plot the shapefile regions
  geom_point(data = alpine_bubbles_richness, aes(x = longitude, y = latitude, size = area_size,fill=richness_log),
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



map_res<-ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray95") + 
  geom_sf(data = alpine_biome,fill="grey",alpha=0.6) + # Plot the shapefile regions
  geom_point(data = alpine_bubbles_residuals, aes(x = longitude, y = latitude, size = area_size,fill=residuals_log),
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
  ) 


x11()
plot(map_ric)

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


# Open the PDF device in A4 landscape (dimensions in inches, width > height)
pdf("~/Desktop/Datasets/output_shared/Vertebrates/maps/Basemaps/bubble_map_richness_no_lim.pdf", width = 12, height = 6)
map_ric
# Close the PDF device
dev.off()



pdf("~/Desktop/Datasets/output_shared/Vertebrates/maps/Basemaps/bubble_map_residuals_no_lim.pdf", width = 11.69, height = 8.27) 
map_res
# Close the PDF device
dev.off()

# Set up the SVG device, specifying the file path and size
svg("~/Desktop/Datasets/output_shared/Vertebrates/maps/Basemaps/bubble_map_richness.svg", width = 11, height = 18,bg="transparent")
# Plot the circular_barplot or map_bubbles (whichever plot you intend to save)
plot(map_ric)  # or replace this with your specific plot command if it's different
# Close the SVG device to save the file
dev.off()
