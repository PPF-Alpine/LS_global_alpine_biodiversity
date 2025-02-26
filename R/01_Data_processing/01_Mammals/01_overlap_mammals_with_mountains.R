
#----------------------------------------------------------#
#         Overlap Mammal ranges with GMBA shapefile
#----------------------------------------------------------#

# This script overlaps mammal distribution ranges with GMBA mountain ranges (level 03) and alpine biome 
# in the end the geometries of the species a checklist is saved with each order as seperate sheet. 

# ❗ The species range shps are partly very large files. Therefore, I process each order seperately

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(dplyr)
library(openxlsx)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2. Define the order name  -----
#----------------------------------------------------------#

# Define the order name
order_name <- "Afrosoricida" # Replace this with the name of the order

# The range shps are stored in zip files. this script loads the range polygons for the defined order, unpacks and loads them. 
source(
  here::here("R/01_Data_processing/01_Mammals/00_source_mammal_data_MDD.R")
)

# These are all the orders that need to be processed
print(all_order_names)

#----------------------------------------------------------#
# 2. Source gmba mountain and alpine biome shps   -----
#----------------------------------------------------------#

#source the gmba regions whith alpine biome
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", 
                                     sep = "/"))|>
                  rename(Mountain_system = Mntn_sy)|> 
                  rename(Mountain_range = Mntn_rn)


# source the alpine biome shapefile 
alpine_biome <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/alpine_biome.shp", sep = "/"))|>
  rename(Mountain_range = Mntn_rn)



mountain_shapes <- make_shapes_valid(mountain_shapes) 

alpine_biome <- make_shapes_valid(alpine_biome) 
#----------------------------------------------------------------------------------------#
# 3. Intersect species ranges with GMBA and Alpine Biome and calculate % of overlap -----
#-----------------------------------------------------------------------------------------#

# The function intersect_species_mountain ranges:
# 1. creates bboxes for mountain ranges and for the species that is beeing processed. 
# 2. If sp and mountain bbox intersect 
#   2.1. it calculates the area of a species in km2
#   2.2. the percentage of overlap of the species range with the mountain range 
#   2.3. the percentage of overlap with the alpine biome in that mountain range
# 3. removes all species with < 1% overlap with a GMBA Mountain range


results <- overlap_mountains_and_alpinebiome(mammals, mountain_shapes, alpine_biome)


# Result is a list with two dataframes:

# processed contains all species that have succesfully been processed
# in case an error occurs - species data is saved fur further debugging in the not processed dataframe 

results_processed <- results$processed
results_not_processed <- results$not_processed

length(unique(results_processed$sciname))
length(unique(results_not_processed$sciname))


#-----------------------------------------------------------------------------
# 4. Remove all species which s distribution ranges overlap < 1% with GMBA range
#-----------------------------------------------------------------------------

results_filtered <- results_processed |> 
  filter(overlap_percentage_mountain >= 1)


#-------------------------
# 5. Restructure dataframes
#-------------------------

# Join the  dataset with the intersection results
mammals_final <- inner_join(mammals, results_filtered[, c("sciname",
                                                          "Mountain_range",
                                                          "species_area",
                                                          "overlap_percentage_mountain",
                                                          "overlap_percentage_alpine")], 
                            by = "sciname")


# Write to a checklist
mammals_checklist <- mammals_final|>
  sf::st_set_geometry(NULL) |> # to remove the geometries for the checklist
  select(order,
         family,
         sciname,
         Mountain_range,
         species_area,
         overlap_percentage_mountain,
         overlap_percentage_alpine,
         author,
         year,
         citation,
         rec_source)


#-------------------------------------------#
# 6. Save the data with geometries  -----
#-------------------------------------------#

# assign the order name to save it
assign(order_name, mammals_final, envir = .GlobalEnv)

# this is the 
RUtilpol::save_latest_file(
  object_to_save =paste0(order_name),
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/geom"),
  prefered_format = "rds",
  use_sha = TRUE) 

#------------------------------------------#
# 7. Save the data as checklist  -----
#------------------------------------------#

# Define the path to your Excel file
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/Mammals_prelim_checklist.xlsx")

# function to write the data to an excel file: each order is written to a seperate sheet
save_excel_sheet(file_path, order_name, mammals_checklist)


