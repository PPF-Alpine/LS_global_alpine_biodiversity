#----------------------------------------------------------#
#         Overlap Reptile ranges with GMBA shapefile
#----------------------------------------------------------#

# This script overlaps reptile distribution ranges with GMBA mountain ranges (level 03) and alpine biome 
# The species range shps are partly very large files. Therefore, I process each group seperately
# There are 6 groups (see 00_Source_data_GARD)

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
# 2. Define the group name and load the data -----
#----------------------------------------------------------#
# These are the 6 groups

# Rhynchocephalia           
# amphisbaenian           
# croc
# lizard
# snake
# turtle

# Define the group name
group_name <- "lizard" # Replace this with the name of the group

# Construct the file path 
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/GARD_2022/groups/", group_name, ".shp")

# Load the shapefile
reptile_shapes <- sf::st_read(file_path, options = "ENCODING=ISO-8859-1")|> 
  dplyr::rename(sciname = binomial)


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
#reptile_shapes_filtered <- reptile_shapes |>
#filter(sciname == "Amphisbaena camura" | sciname == "Amphisbaena pericensis")
#filter(sciname == "Amphisbaena pericensis")


# Execute the main function
results <- overlap_mountains_and_alpinebiome(reptile_shapes, mountain_shapes, alpine_biome)

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
reptiles_final <- inner_join(reptile_shapes, results_filtered[, c("sciname",
                                                                  "Mountain_range",
                                                                  "overlap_percentage_mountain",
                                                                  "overlap_percentage_alpine")], 
                             by = "sciname")
# Write to a checklist
reptiles_checklist <- reptiles_final|>
  sf::st_set_geometry(NULL) |> # to remove the geometries for the checklist
  select(TaxonID,
         group,
         family,
         sciname,
         Mountain_range,
         area,
         overlap_percentage_mountain,
         overlap_percentage_alpine)|> rename(species_area = area)

#-------------------------------------------#
# 6. Save the data with geometries  -----
#-------------------------------------------#

# assign the order name to save it
assign(group_name, reptiles_final, envir = .GlobalEnv)

# this is the 
RUtilpol::save_latest_file(
  object_to_save =paste0(group_name),
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/geom"),
  prefered_format = "rds",
  use_sha = TRUE) 

#------------------------------------------#
# 7. Save the data as checklist  -----
#------------------------------------------#

# Define the path to your Excel file
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/Reptiles_Checklist.xlsx")

# function to write the data to an excel file: each order is written to a seperate sheet
save_excel_sheet(file_path, group_name, reptiles_checklist)


