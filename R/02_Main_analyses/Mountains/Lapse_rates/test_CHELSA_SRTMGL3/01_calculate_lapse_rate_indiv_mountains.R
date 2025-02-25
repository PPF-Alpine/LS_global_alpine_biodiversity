
#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(tidyverse)
library(broom)
library(raster)

# download the Climat download Package to download chelsa data
if(!require(devtools)) install.packages("devtools")
library(devtools)
devtools::install_github("HelgeJentsch/ClimDatDownloadR")

library(ClimDatDownloadR)

help(clim)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2. Load data ---
#----------------------------------------------------------#

# Download and save temperature chelsa
#temp <- Chelsa.Clim.download(save.location ="~/Desktop/Datasets/Mountains/Chelsa",parameter = "bio", bio.var = c(1))


# Load temperature data into Renv
temp <- raster::raster(paste0(data_storage_path,"Mountains/Chelsa/bio/ChelsaV1.2Climatologies/CHELSA_bio10_01_V1.2.tif"))

# load gmba mountain shapes version 2
mountain_shapes <- sf::st_read(paste(data_storage_path,"Mountains/Mountain_Alpine_Biome_Input/GMBA_Mountains_Input.shp", sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)

# Filter for northern Andes to test
mountain_range <- mountain_shapes |> 
  filter(Mountain_range == "Northern Andes")

#----------------------------------------------------------#
# 3. crop the temperature to the mountain ---
#----------------------------------------------------------#

temp_cropped <- raster::crop(temp, mountain_range) # use crop first and then mask --> faster
temp_mountain <- raster::mask(temp_cropped, mountain_range)

#----------------------------------------------------------#
# 4. Download the elevation raster per mountain range ---
#----------------------------------------------------------#

# get elevation raster data for northern Andes
elevation_mountain <- elevatr::get_elev_raster(mountain_range,
                                               source = "gl3",
                                               clip = "locations", #this clips to polygon of mountain range
                                               z = 7, # zoom level 7 = resolution of 30arc seconds ~1km (closest to chelsa data)
                                               neg_to_na = TRUE,
                                               override_size_check = TRUE)

x11()
plot(elevation_mountain)

# this downloads elevation raster at a resolution of 0.005465835 but temperature raster is 0.008333333

#----------------------------------------------------------#
# 5. Resample elevation raster to temperature raster---
#----------------------------------------------------------#
# 
elevation_mountain <- resample(elevation_mountain, temp_mountain, method = "bilinear")

#--------------------------------------------------------------------------------------------------#
# 6. Find relationship between temperature and elevation in each mountain range = Lapse Rate ---
#---------------------------------------------------------------------------------------------------#

# extract the raster values
elevation_values <- raster::getValues(elevation_mountain)
temperature_values <- raster::getValues(temp_mountain)

# Combine the values into df
combined_data <- tibble(elevation = elevation_values, temperature = temperature_values)|>drop_na()

# plot temp against elevation
x11()
ggplot(combined_data, aes(x = elevation, y = temperature)) +
  geom_point(alpha = 0.5, color = "red") +  
  labs(x = "Elevation (meters)", y = "Temperature (°C)", title = "Elevation vs Temperature in Northern Andes") +
  theme_minimal()

# Fit the linear model
model <- lm(temperature ~ elevation, data = combined_data)

# get the coefficients
tidy_model <- tidy(model)

# Extract the slope (coefficient of elevation)
slope <- tidy_model$estimate[2]  # second coefficient is the slope of the elevation predictor

slope # -0.0052706 --> for every meter increase in elevation the temperature decreases by ~0.0052706 degree --> -5.2 degree per 1000 m

lapse_rate <- slope*1000
print(lapse_rate)

summary(model)

