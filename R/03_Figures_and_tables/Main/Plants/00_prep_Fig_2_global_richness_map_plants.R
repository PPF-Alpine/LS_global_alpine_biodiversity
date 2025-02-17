#----------------------------------------------------------#
#   Prepare plant data for Bivariate Map
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

# Load the latest files
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

#--------------------------------------------------------
# Get the richness dataframe for the map
#--------------------------------------------------------

# Filter and process proportional richness results for alpine specialists
alp_generalist_specialist <- proportional_richness_results |>
  filter(condition == "alp_specialist") |> # Modify condition to map other categories 
  select(
    geo_entity, condition, generalist_residuals, generalist_residuals_log,
    generalist_richness, generalist_richness_log, richness_log, richness,
    residuals, residuals_log, proportion_specialists, log_prop_specialists
  ) |>
  distinct()

# ❗ IMPORTANT:

# this filters only for specialists so the column log prop specialists is proportion of alpine specialists within all (mountain generalists).
# Change the filter condition to e.g., degr_2 to map the proportion of degr_2 within all mountain generalist species 

#--------------------------------------------------------
# Load spatial background data
#--------------------------------------------------------

# the world map
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent != "Antarctica") |>
  st_transform(crs = eckertIV)

# the alpine biome
# load alpine biome Suzette
alpine_biome <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/alpine_biome.shp", 
                                  sep = "/"))|>
  rename(Mountain_range = Mntn_rn)|>
  rename(area_size = area_sz)|>
  rename(log_area = log_are)|>
  st_transform(crs = eckertIV) 

# keep only those polygons where we have GIFT checklists for 
alpine_biome_filtered <- alpine_biome |>
  filter(Mountain_range %in% alpine_area_treeline_GIFT$Mountain_range)


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

#--------------------------------------------------------
# Join information on alpine area size to geo entities
#--------------------------------------------------------

# Filter GIFT shapes to only include relevant geo entities
gift_shapes_df_filtered <- gift_shapes_df |>
  filter(geo_entity %in% unique(GIFT_alp_area$geo_entity))

# Prepare area subset for joining
area_subset <- alpine_area_treeline_GIFT |>
  mutate(
    log_alp_area = log1p(alpine_area),
    perc_alpine_area = round(perc_alpine_area, 2),
    treeline_GIFT = round(treeline_GIFT, 0)
  ) |>
  select(geo_entity, alpine_area, perc_alpine_area, log_alp_area, treeline_GIFT) |>
  distinct()

# Merge area information into GIFT shapes
gift_shapes_df_info <- gift_shapes_df_filtered |>
  left_join(area_subset, by = "geo_entity") |>
  mutate(alpine_area = as.numeric(alpine_area))

#--------------------------------------------------------
# Calculate the centroids to plot them as bubbles on the map
#--------------------------------------------------------
geoentities_centroids <- gift_shapes_df_info |>
  st_centroid() |>
  st_transform(crs = eckertIV)

# Prepare data for plotting bubbles
alpine_bubbles <- gift_shapes_df_info |>
  sf::st_set_geometry(NULL) |>
  cbind(sf::st_coordinates(geoentities_centroids)) |>
  rename(longitude = X, latitude = Y) |>
  drop_na()

# Add proportional richness data
alpine_bubbles_join <- alpine_bubbles |>
  left_join(alp_generalist_specialist, by = "geo_entity") |>
  filter(generalist_richness > 7)|>
  # Add the GIFT ID (labels for the map)
  left_join(gift_info|>select(geo_entity,region_ID),by="geo_entity")|>
  rename(GIFT_ID = region_ID)


#--------------------------------------------------------
# Classify data into 3x3 grid for bivariate mapping
#--------------------------------------------------------

map_data <- alpine_bubbles_join |>
  mutate(log_prop_specialists_jitter = log_prop_specialists + rnorm(n(), mean = 0, sd = 1e-5)) |>
  bi_class(x = log_prop_specialists_jitter, y = generalist_residuals_log, dim = 3, style = "quantile")

# rank the priority which bubbles should be on top
map_data <- map_data |>
  mutate(order = case_when(
    bi_class == "3-3" ~ 4,  # Highest priority
    bi_class == "3-2" ~ 3,  # Second highest priority
    bi_class == "3-1" ~ 2,  # Third priority
    bi_class == "1-3" ~ 0,  # Lowest priority
    TRUE ~ 1                # All other classes have default priority
  ))

# Reorder the data based on the new order column
map_data_ordered <- map_data |>
  arrange(order)

# Convert the ordered map_data to an sf object with Eckert IV projection
map_data_sf <- st_as_sf(map_data_ordered, coords = c("longitude", "latitude"), crs = eckertIV) 

# Extract coordinates from the transformed sf object
map_data_coords <- st_coordinates(map_data_sf)

# mutate the coordinates
map_data_sf <- map_data_sf |>
  mutate(longitude = map_data_coords[, "X"], latitude = map_data_coords[, "Y"])

# Classify data into bivariate 3x3 grid using quantiles
map_data_bi <- bi_class(map_data, 
                        x = log_prop_specialists_jitter,  # Use the jittered version
                        y = generalist_residuals_log, 
                        style = "quantile",
                        dim = 3)

