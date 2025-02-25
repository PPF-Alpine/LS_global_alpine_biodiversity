
# In this script I calculate lapse rates across all mountain ranges
# different levels of GMBA mountain ranges can be used. level 03, 04, 05 

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
#  load data
#-------------------------------#

# Load temperature data into Renv
temp <- raster::raster(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Chelsa/bio/ChelsaV2.1Climatologies/CHELSA_bio10_01_V2.1.tif"))


# load gmba mountain shapes version 2 Level 05
# ❗ change here input file to run on different GMBA levels
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Levels/Level_04/GMBA_Inventory_v2.0_Level_04.shp", 
                                     sep = "/"))|>
  dplyr::select(Level_02,Level_03,Level_04,geometry)|>
  dplyr::rename(Mountain_system = "Level_02") |>
  dplyr::rename(Mountain_range_03 = "Level_03")|>
  dplyr::rename(Mountain_range = "Level_04")

#load DEM
DEM <- raster::raster(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/DEM/GMTED2010_30.tiff"))


mountain_shapes_df <- mountain_shapes|>
  sf::st_set_geometry(NULL)

#--------------------------------#
# test with indiviudal mountain ranges--
#-------------------------------#
# choose some mountains to test function
mountain_shapes <- mountain_shapes |>
  dplyr::filter(Mountain_range_03 =="Rocky Mountains"|
                  Mountain_range_03 =="Northern Andes")

#--------------------------------#
# Loop through Mountain ranges--
#-------------------------------#

# to store lapse rates
lapse_rate_list <- list()

# loop through mountain ranges
for (i in unique(mountain_shapes$Mountain_range)) {
  current_shape <- mountain_shapes %>% filter(Mountain_range == i)
  if (nrow(current_shape) > 0) {
    tryCatch({
      data_list <- get_elevation_raster(current_shape, i)
      combined_data <- resample_and_extract(data_list$DEM_mountain, data_list$temp_mountain, i)
      lapse_rate_list[[i]] <- get_lapse_rate(combined_data, i)
    }, error = function(e) {
      cat("Error processing", i, ": ", e$message, "\n")
      # Optionally, log or handle error further
    })
  }
}

#--------------------------------#
# create a results dataframe-
#-------------------------------#

# Dataframe 
lapse_rate_df <- tibble(
  Mountain_range = names(lapse_rate_list),
  Lapse_rate = unlist(lapse_rate_list))|>
  dplyr::left_join(mountain_shapes_df,by="Mountain_range")|>
  group_by(Mountain_range_03)|>
  mutate(mean_level_03= round(mean(Lapse_rate),3))|>
  mutate(median_level_03= round(median(Lapse_rate),3))|>
  ungroup()

print(lapse_rate_df)

# creates a dataframe with the lapse rate of level 04 mountain range with a mean and median across level 03 mountain range

#--------------------------------#
# Save the data -
#-------------------------------#

writexl::write_xlsx(lapse_rate_df, data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/Lapse_Rates/Lapse_Rates_GMBA_V2_Level_05.xlsx")

#

