#----------------------------------------------------------#
#   Prepare plant data for normalized richness plot
#----------------------------------------------------------#

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# Load configuration
source(here::here("R/00_Config_file.R"))


library(here)
library(tidyverse)
library(purrr)
library(insight)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(biscale)

#----------------------------------------------------------#
# Load data
#----------------------------------------------------------#

proportional_richness_results <- RUtilpol::get_latest_file(
  "proportional_richness_results",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)

alpine_area_treeline_GIFT <- RUtilpol::get_latest_file(
  "alpine_area_treeline_GIFT",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)

# meta information about GIFT regions (for labelling)
gift_info <- readxl::read_xlsx(
  path = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/GIFT_info_table.xlsx")
)

# Define Eckert IV projection
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#----------------------------------------------------------
# Prepare Data for alpine generalists
#----------------------------------------------------------
alp_generalist_specialist <- proportional_richness_results |> 
  group_by(geo_entity) |> 
  select(geo_entity, condition, generalist_residuals, generalist_residuals_log,
         generalist_richness, generalist_richness_log, richness_log, richness,
         residuals, residuals_log, proportion_specialists, log_prop_specialists) |> 
  distinct() |> 
  filter(condition == "alp_generalist")

# for the Latitude plots and normalized plots we look here at alpine generalists 
# filter  for different alpine categories 

#--------------------------------------------------------
# Load GIFT polygons to calculate centroids 
#--------------------------------------------------------

# The gift shapes (for the centroids)
gift_shapes_df <- sf::st_read(
  file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/shapes/27032024_gift_shapes.shp"),
  options = "ENCODING=ISO-8859-1"
) |>
  st_transform(crs = eckertIV)

# alpine area in GIFT has been calculated in ARCGISPro
GIFT_alp_area <- sf::st_read(
  file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/shapes/alpine_area_gift.shp")
) |>
  st_transform(crs = eckertIV)

#----------------------------------------------------------
# Load and Process Spatial Data
#----------------------------------------------------------

# GIFT Alpine Area Data
GIFT_alp_area <- sf::st_read(
  file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/shapes/alpine_area_gift.shp"))

# GIFT Shapes for the centroids and latitudinal ranges
gift_shapes_df <- sf::st_read(
  file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/shapes/27032024_gift_shapes.shp"),
                              options = "ENCODING=ISO-8859-1") |> 
  filter(geo_entity %in% unique(GIFT_alp_area$geo_entity))


#----------------------------------------------------------
# Get the information on alpine area within GIFT regions
#----------------------------------------------------------
# 
area_subset <- alpine_area_treeline_GIFT |> 
  mutate(log_alp_area = log1p(alpine_area),
         perc_alpine_area = round(perc_alpine_area, 2),
         treeline_GIFT = round(treeline_GIFT, 0)) |> 
  select(geo_entity, alpine_area, perc_alpine_area, log_alp_area, treeline_GIFT) |> 
  distinct()

gift_shapes_df_info <- gift_shapes_df |> 
  left_join(area_subset, by = "geo_entity")

#----------------------------------------------------------
# Calculate Centroids
#----------------------------------------------------------

geoentities_centroids <- sf::st_centroid(gift_shapes_df_info)

# 
alpine_biome_richness <- gift_shapes_df_info |> 
  sf::st_set_geometry(NULL) |> 
  cbind(sf::st_coordinates(geoentities_centroids)) |> 
  rename(longitude = X, latitude = Y) |> 
  drop_na() |> 
  left_join(alp_generalist_specialist, by = "geo_entity")

#----------------------------------------------------------
# Calculate Latitudinal ranges
#----------------------------------------------------------

latitudinal_ranges <- GIFT_alp_area |> 
  group_by(geo_entity) |> 
  summarise(lat_range = st_bbox(geometry)["ymax"] - st_bbox(geometry)["ymin"], .groups = "drop")

latitudinal_ranges_df <- latitudinal_ranges |> st_set_geometry(NULL)

alpine_biome_richness <- alpine_biome_richness |> 
  left_join(latitudinal_ranges_df, by = "geo_entity") |> 
  mutate(lat_range_normalized = 1 + (lat_range - min(lat_range)) / (max(lat_range) - min(lat_range)),
         log_lat_range = log1p(lat_range),
         ymin_latrange = latitude - (lat_range / 2),
         ymax_latrange = latitude + (lat_range / 2)) |> 
  filter(generalist_richness > 7)

#----------------------------------------------------------
# Save Results
#----------------------------------------------------------

RUtilpol::save_latest_file(
  object_to_save = alpine_biome_richness,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)


