#----------------------------#
#     Set up and load data
#----------------------------#

source(here::here("R/00_Config_file.R"))

library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(insight)
library(sf)
library(ggplot2)
library(ggrepel)

prop_categories <- RUtilpol::get_latest_file(
  "proportions_alp_categories",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),
  verbose = TRUE
)


alpine_biome <- sf::st_read(file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/alpine_biome.shp"))|>
  rename(Mountain_range = MapName, area_size = Area)|>
  mutate(area_size = round(area_size, 0), log_area = log(area_size))|>
  filter(Mountain_range %in% unique(prop_categories$Mountain_range))


# Validate Alpine Biome shapes
alpine_biome <- validate_shapes_individually(alpine_biome)


# Filter for generalists and select relevant columns
prop_catgories_filter <- prop_categories|>
  filter(filter_condition == "generalists")|>
  select(Mountain_range, filter_condition, total_richness,total_richness_log1p,total_residuals)|>
  distinct(Mountain_range, .keep_all = TRUE)

# Eckert IV projection
#eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

alpine_biome <- alpine_biome |>
  st_transform(crs = 4326)


#--------------------------------------------------------
# Generate Centroids and Prepare Data for Plotting
#--------------------------------------------------------

alpine_biome_centroids <- alpine_biome|>
  filter(Mountain_range %in% unique(prop_catgories_filter$Mountain_range))|>
  mutate(area_size = as.numeric(area_size))|>
  st_centroid()

alpine_biome_df <- alpine_biome_centroids|>
  st_set_geometry(NULL)

alpine_biome_richness <- alpine_biome_df|>
  select(Mountain_range, area_size, log_area)|>
  cbind(st_coordinates(alpine_biome_centroids))|>
  rename(longitude = X, latitude = Y)|>
  left_join(prop_catgories_filter, by = "Mountain_range")|>
  drop_na()|>
  distinct()

#--------------------------------------------------------
# Calculate latitudinal ranges
#--------------------------------------------------------

latitudinal_ranges <- alpine_biome |>
  group_by(Mountain_range) |>
  summarise(
    lat_range = st_bbox(geometry)["ymax"] - st_bbox(geometry)["ymin"],
    .groups = "drop"
  )

latitudinal_ranges_df <- latitudinal_ranges|>
  st_set_geometry(NULL)

alpine_biome_richness <- alpine_biome_richness|>
  left_join(latitudinal_ranges_df,by="Mountain_range")|>
  mutate(
    lat_range_normalized = 1 + (lat_range - min(lat_range)) / (max(lat_range) - min(lat_range))
  )|>
  mutate(log_lat_range = log1p(lat_range))|>
  mutate(
    ymin_latrange = latitude - (lat_range / 2),  # Adjust these calculations as needed
    ymax_latrange = latitude + (lat_range / 2)   # For demonstration, scaled range is halved
  )


#----------------------------#
#    Save latest file
#----------------------------#

# save the list of df
RUtilpol::save_latest_file(
  object_to_save = alpine_biome_richness,  
  file_name = "centroids_richness_sar",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)
