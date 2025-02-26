#----------------------------------------------------------#
#         Overlap Birds ranges with GMBA shapefile
#----------------------------------------------------------#

# This script overlaps birds distribution ranges (BirdLife International) with GMBA mountain ranges (level 03) and alpine biome 
# In total there are 10367 species to process
# I divided in 10 chunks: chunk_1 : 10

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(dplyr)
library(openxlsx)
library(furrr)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2. Define the chunk name and load the data -----
#----------------------------------------------------------#

# Define the chunk
chunk_name <- "chunk_10" # Replace this with the the different chunks 

# specify file path 
file_path <- paste0(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/", chunk_name, ".shp"))

# Load the shapefile
birds_shapes <- sf::st_read(file_path, options = "ENCODING=ISO-8859-1")|> 
  dplyr::rename(sciname = sci_nam)


# set geometry false to investigate data 
birds_no_geom <- sf::st_set_geometry(birds_shapes, NULL)


#----------------------------------------------------------#
# 2. Source gmba mountain and alpine biome shps   -----
#----------------------------------------------------------#

#source the gmba regions whith alpine biome
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", 
                                     sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)


# source the alpine biome 
alpine_biome <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/alpine_biome.shp", sep = "/"))|>
  rename(Mountain_range = Mntn_rn)

# check if there are any invalid shapes
mountain_shapes <- make_shapes_valid(mountain_shapes) 

alpine_biome <- make_shapes_valid(alpine_biome) 

# there shouldnt be any invalid shapes

#----------------------------------------------------------------------------------------#
# 3. Intersect species ranges with GMBA and Alpine Biome and calculate % of overlap -----
#-----------------------------------------------------------------------------------------#

# The function intersect_species_mountain ranges:
# 1. creates bboxes for mountain ranges and for the species that is beeing processed. 
# 2. If sp and mountain bbox intersect 
#   2.1. it takes the area of a species in km2 (is already in reptile dataset)
#   2.2. the percentage of overlap of the species range with the mountain range 
#   2.3. the percentage of overlap with the alpine biome in that mountain range
# 3. removes all species with < 1% overlap with a GMBA Mountain range

# To test function
#birds_shapes_filtered <- birds_shapes |> 
#filter(sciname == "Ardea alba" | sciname == "Acrocephalus scirpaceus")

# Execute the main function
results <- overlap_mountains_and_alpinebiome(birds_shapes, mountain_shapes, alpine_biome)

# Result is a list with two dataframes:
# processed contains all species that have succesfully been processed
# not processed contains species where an error occured

results_processed <- results$processed
results_not_processed <- results$not_processed

#-----------------------------------------------------------------------------
# 4. Remove all species which s distribution ranges overlap < 1% with GMBA range
#-----------------------------------------------------------------------------

results_filtered <- results_processed |> filter(overlap_percentage_mountain >= 1)


#-------------------------
# 5. Restructure dataframes
#-------------------------

# Join the  dataset with the intersection results
birds_final <- inner_join(birds_shapes, results_filtered[, c("sciname",
                                                             "Mountain_range",
                                                             "overlap_percentage_mountain",
                                                             "overlap_percentage_alpine","species_area")], 
                          by = "sciname")
# Write to a checklist
birds_checklist <- birds_final|>
  sf::st_set_geometry(NULL) |> # to remove the geometries for the checklist
  select(sciname,
         Mountain_range,
         species_area,
         overlap_percentage_mountain,
         overlap_percentage_alpine)


#------------------------------------------#
# 7. Save the data as checklist  -----
#------------------------------------------#

## The checklist:

# Define the path to your Excel file
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_prelim_checklist.xlsx")

# function to write the data to an excel file: each order is written to a seperate sheet
save_excel_sheet(file_path, chunk_name, birds_checklist)


#-------------------------------------------#
# 6. Save the data with geometries  -----
#-------------------------------------------#

# assign the order name to save it
assign(chunk_name, birds_final, envir = .GlobalEnv)

# this is the 
RUtilpol::save_latest_file(
  object_to_save =paste0(chunk_name),
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/geom"),
  prefered_format = "rds",
  use_sha = TRUE) 


