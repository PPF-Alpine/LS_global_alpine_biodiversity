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
  
  temp_cropped <- raster::crop(temp, mountain_range_shape)
  temp_mountain <- raster::mask(temp_cropped, mountain_range_shape)
  
  elevation_mountain <- elevatr::get_elev_raster(mountain_range_shape,
                                                 source = "gl3",
                                                 clip = "locations",
                                                 z = 7,
                                                 neg_to_na = TRUE,
                                                 override_size_check = TRUE)
  
  print(paste("Finished processing data for:", mountain_range_name))
  return(list(temp_mountain = temp_mountain, elevation_mountain = elevation_mountain))
}



# Function to resample the elevation raster and extract values
resample_and_extract <- function(elevation_mountain, temp_mountain, mountain_range_name) {
  print(paste("Resampling and extracting values for:", mountain_range_name))
  
  elevation_mountain_resampled <- resample(elevation_mountain, temp_mountain, method = "bilinear")
  
  elevation_values <- raster::getValues(elevation_mountain_resampled)
  temperature_values <- raster::getValues(temp_mountain)
  
  combined_data <- tibble(elevation = elevation_values, temperature = temperature_values) %>% drop_na()
  
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

# Download and save temperature chelsa
#temp <- Chelsa.Clim.download(save.location ="~/Desktop/Datasets/Mountains/Chelsa",parameter = "bio", bio.var = c(1))


# Load temperature data into Renv
temp <- raster::raster(paste0(data_storage_path,"Mountains/Chelsa/bio/ChelsaV1.2Climatologies/CHELSA_bio10_01_V1.2.tif"))

# Load mountain shapes 
mountain_shapes <- sf::st_read(paste(data_storage_path,"Mountains/GMBA_Mountains_v2.0/Level_03/GMBA_Inventory_v2.0_Level_03.shp", sep = "/")) |>
  select(Level_02,Level_03,geometry)|>
  rename(Mountain_system = Level_02) |>
  rename(Mountain_range = Level_03)

# choose some mountains to test function
#mountain_shapes <- mountain_shapes |> filter(Mountain_range=="Chukotka Mountains"|Mountain_range=="Northern Andes"| Mountain_range=="Southern Andes")

# to store lapse rates
lapse_rate_list <- list()

# loop through mountain ranges
for (i in unique(mountain_shapes$Mountain_range)) {
  current_shape <- mountain_shapes %>% filter(Mountain_range == i)
  if (nrow(current_shape) > 0) {
    tryCatch({
      data_list <- get_elevation_raster(current_shape, i)
      combined_data <- resample_and_extract(data_list$elevation_mountain, data_list$temp_mountain, i)
      lapse_rate_list[[i]] <- get_lapse_rate(combined_data, i)
    }, error = function(e) {
      cat("Error processing", i, ": ", e$message, "\n")
      # Optionally, log or handle error further
    })
  }
}

# Dataframe 
lapse_rate_df <- tibble(
  Mountain_range = names(lapse_rate_list),
  Lapse_rate = unlist(lapse_rate_list)
)

print(lapse_rate_df)

check_and_write_xlsx(lapse_rate_df, data_storage_path, "Mountains/Lapse_Rates/Lapse_Rates_GMBA_V2.xlsx")

#

