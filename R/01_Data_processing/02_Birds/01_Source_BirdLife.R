#----------------------------------------------------------#
#         Source Selected BirdLife Data
#----------------------------------------------------------#

# 

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(sf)

# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 2. Load the range shapefiles  -----
#----------------------------------------------------------#

birds_shapes <- sf::st_read(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/birdlife_merge.shp"))|>
  rename(sciname=sci_nam)

#------------------------------------------------------------------------#
#  Slice the data into 10 chunks to reduce computation time -----
#----------------------------------------------------------------------------#

# In total there are 10367 species to process
# I divided in 10 chunks: chunk_1 : 10
n <- nrow(birds_shapes)

# Calculate the size of each chunk
chunk_size <- ceiling(n / 10)

# Create a new column that assigns each row to a chunk
birds_shapes <- birds_shapes |>
  mutate(chunk = ((row_number() - 1) %/% chunk_size) + 1)

# split the data into a list of 10 sf data frames
sf_chunks <- split(birds_shapes, birds_shapes$chunk)

lapply(sf_chunks, nrow)

# Define the directory to save shapefiles
output_directory <- paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife")

# Loop through chunks and write it as shapefile
lapply(seq_along(sf_chunks), function(i) {
  chunk <- sf_chunks[[i]]
  # Construct the shapefile name
  shapefile_name <- paste0(output_directory, "/chunk_", i, ".shp")
  # Write the chunk to a shapefile
  st_write(chunk, shapefile_name, delete_layer = TRUE, quiet = TRUE)
})









