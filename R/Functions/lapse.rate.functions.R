#--------------------------------#
# Functions to calculate lapse rates across mountain ranges----
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
