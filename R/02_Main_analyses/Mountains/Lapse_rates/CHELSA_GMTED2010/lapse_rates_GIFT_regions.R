
# in this script I calculate elevation change with temperature wihin GIFT regions

#----------------------------#
#     Set up 
#----------------------------#

source(here::here("R/00_Config_file.R"))

library(sf)
library(raster)
library(tidyverse)
library(broom)
library(elevatr)

#--------------------------------#
# Functions ----
#-------------------------------#

# Function to crop the temperature raster and get elevation data
get_elevation_raster <- function(mountain_range_shape, mountain_range_name) {
  print(paste("Processing elevation and temperature data for:", mountain_range_name))
  
  temp_cropped <- raster::crop(temp, mountain_range_shape) # Adjusted variable name
  temp_mountain <- raster::mask(temp_cropped, mountain_range_shape)
  
  DEM_cropped <- raster::crop(DEM, mountain_range_shape) # Adjusted variable name
  DEM_mountain <- raster::mask(DEM_cropped, mountain_range_shape)
  
  print(paste("Finished processing data for:", mountain_range_name))
  return(list(temp_mountain = temp_mountain, DEM_mountain = DEM_mountain))
}


# Function to resample the elevation raster and extract values
resample_and_extract <- function(DEM_mountain, temp_mountain, mountain_range_name) {
  print(paste("Extracting values for:", mountain_range_name))
  
  elevation_values <- raster::getValues(DEM_mountain)
  temperature_values <- raster::getValues(temp_mountain)
  
  combined_data <- tibble::tibble(elevation = elevation_values, temperature = temperature_values) |> drop_na()
  
  print(paste("Finished resampling and extraction for:", mountain_range_name))
  return(combined_data)
}

# Function to fit the linear model and extract the lapse rate
get_lapse_rate <- function(combined_data, mountain_range_name) {
  print(paste("Calculating lapse rate for:", mountain_range_name))
  
  model <- lm(temperature ~ elevation, data = combined_data)
  slope <- tidy(model)$estimate[2]
  lapse_rate <- slope * 1000  # Convert to °C per 1000 meters
  
  print(paste("Finished calculating lapse rate for:", mountain_range_name))
  return(lapse_rate)
}


#--------------------------------#
# Loop through Mountain ranges--
#-------------------------------#

# Load temperature data into Renv
temp <- raster::raster(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Chelsa/bio/ChelsaV2.1Climatologies/CHELSA_bio10_01_V2.1.tif"))

#load DEM
DEM <- raster::raster(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/DEM/GMTED2010_30.tiff"))

# Load mountain shapes 
gift_polygons <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Plants/shapes/27032024_gift_shapes.shp", sep = "/"),options = "ENCODING=ISO-8859-1")|>
  dplyr::select(geo_entity,geometry)

gift_shapes_df <- gift_polygons|>sf::st_set_geometry(NULL)

gift_polygons<- validate_shapes_individually(gift_polygons)

# choose some mountains to test function

gift_polygons <- gift_polygons |>
dplyr::filter(geo_entity =="New_Zealand"| geo_entity =="Alps")

# to store lapse rates
lapse_rate_list <- list()

# loop through mountain ranges
for (i in unique(gift_polygons$geo_entity)) {
  current_shape <- gift_polygons %>% filter(geo_entity == i)
  if (nrow(current_shape) > 0) {
    tryCatch({
      data_list <- get_elevation_raster(current_shape, i)
      combined_data <- resample_and_extract(data_list$DEM_mountain, data_list$temp_mountain, i)
      lapse_rate_list[[i]] <- get_lapse_rate(combined_data, i)
    }, error = function(e) {
      cat("Error processing", i, ": ", e$message, "\n")
    })
  }
}



# Dataframe 
lapse_rate_df <- tibble(
  geo_entity = names(lapse_rate_list),
  Lapse_rate = unlist(lapse_rate_list))|>
  dplyr::left_join(gift_shapes_df,by="geo_entity")
  

print(lapse_rate_df)

writexl::write_xlsx(lapse_rate_df, data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/Lapse_Rates_GIFT_shapes.xlsx")

#-------------------------------------------------------------------------------------------------------#
# Alternative - use level 05 lapse rates calculated for each level 05 (smallest scale alpine biome) and calculate elev changes with temperatue for geo units in these mountain ranges--
#--------------------------------------------------------------------------------------------------------#

# load the alpine biome/geo entity conversion and the treeline in each geo entity
alpine_biome <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/area_size_overlap_gift_ab.xlsx"))|>
  select(geo_entity,alpine_biome)|>
  rename(Mountain_range=alpine_biome)


treeline_GIFT <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/alpine_area_GIFT.xlsx"))|>
  select(mean_treeline,geo_entity)|>
  rename(treeline_GIFT = mean_treeline)

# load the lapse rates for level 05
lapse_rates_05 <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/Lapse_Rates/Lapse_Rates_GMBA_V2_Level_05.xlsx")) |>
  dplyr::select(Mountain_range_03, mean_level_03) |>
  distinct(Mountain_range_03, .keep_all = TRUE)|>
  rename(Mountain_range = Mountain_range_03)|>
  rename(lapse_rate_mean_05 = mean_level_03)


tl_lapserate <- alpine_biome |>
  left_join(lapse_rates_05, by = "Mountain_range") |>
  group_by(geo_entity) |>  # Group by both 'geo_entity' and 'Mountain_range'
  summarise(average_lapse_rate_05 = mean(lapse_rate_mean_05, na.rm = TRUE), .groups = "drop")|>
  left_join(treeline_GIFT, by = "geo_entity")


elev_change <- tl_lapserate|>
  mutate(elev_change_1_degree = round((-1/average_lapse_rate_05)*1000),1)|>
  mutate(treeline_GIFT_1_degree = treeline_GIFT - elev_change_1_degree)|>
  mutate(elev_change_2_degree = round((-2/average_lapse_rate_05)*1000),1)|> # mutate elevation change with 2 degree
  mutate(treeline_GIFT_2_degree = treeline_GIFT - elev_change_2_degree)|> # mutate treeline - elevation change for 2 degree 
  mutate(elev_change_3_degree = round((-3/average_lapse_rate_05)*1000),1)|>
  mutate(treeline_GIFT_3_degree = treeline_GIFT - elev_change_3_degree)|>
  mutate(elev_change_4_degree = round((-4/average_lapse_rate_05)*1000),1)|>
  mutate(treeline_GIFT_4_degree = treeline_GIFT - elev_change_4_degree)|>
  mutate(elev_change_5_degree = round((-5/average_lapse_rate_05)*1000),1)|>
  mutate(treeline_GIFT_5_degree = treeline_GIFT - elev_change_5_degree)|>
  mutate(elev_change_6_degree = round((-6/average_lapse_rate_05)*1000),1)|>
  mutate(treeline_GIFT_6_degree = treeline_GIFT - elev_change_6_degree)|>
  mutate(elev_change_8_degree = round((-8/average_lapse_rate_05)*1000),1)|>
  mutate(treeline_GIFT_8_degree = treeline_GIFT - elev_change_8_degree)

writexl::write_xlsx(elev_change, data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/Treeline_Lapse_Rates_GIFT.xlsx")

