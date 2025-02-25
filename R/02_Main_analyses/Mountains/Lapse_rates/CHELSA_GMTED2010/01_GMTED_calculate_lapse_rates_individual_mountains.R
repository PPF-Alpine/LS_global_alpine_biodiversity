
# this script is used to calculate lapse rates for individual mountain ranges 


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
temp <- raster::raster(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Chelsa/bio/ChelsaV2.1Climatologies/CHELSA_bio10_01_V2.1.tif"))

# load gmba mountain shapes version 2
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)

# load gmba mountain shapes version 2 Level 04
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Levels/Level_04/GMBA_Inventory_v2.0_Level_04.shp", 
                                     sep = "/"))|>
  dplyr::select(Level_02,Level_03,Level_04,geometry)|>
  dplyr::rename(Mountain_system = "Level_02") |>
  dplyr::rename(Mountain_range_03 = "Level_03")|>
  dplyr::rename(Mountain_range = "Level_04")

#load DEM
DEM <- raster::raster(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/DEM/GMTED2010_30.tiff"))

# Filter for northern Andes to test
mountain_range <- mountain_shapes |> 
  filter(Mountain_range == "Northern Andes")

#----------------------------------------------------------#
# 3. crop the temperature to the mountain ---
#----------------------------------------------------------#

temp_cropped <- raster::crop(temp, mountain_range) # use crop first and then mask --> faster
temp_mountain <- raster::mask(temp_cropped, mountain_range)

DEM_cropped <- raster::crop(DEM, mountain_range) # use crop first and then mask --> faster
DEM_mountain <- raster::mask(DEM_cropped, mountain_range)

# DEM resolution : 0.008333333
# temp resolution: 0.008333333


#--------------------------------------------------------------------------------------------------#
# 6. Find relationship between temperature and elevation in each mountain range = Lapse Rate ---
#---------------------------------------------------------------------------------------------------#

# extract the raster values
elevation_values <- raster::getValues(DEM_mountain)
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

