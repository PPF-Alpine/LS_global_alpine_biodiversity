#----------------------------------------------------------#
#         Get elevations with DEM 
#----------------------------------------------------------#

# In this script I extract quartiles for species min and max elevations from their range shps using SRTMGL3
# Shuttle Radar Topography Mission (SRTM GL3) Global 90m
# https://portal.opentopography.org/raster?opentopoID=OTSRTM.042013.4326.1

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(tidyverse)
library(data.table)
library(openxlsx)


# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 2. Load species data and set API key  -----
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

# Read the checklist that includes the elevation data
Checklist_Elev <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Reptiles/processed/Reptiles_Checklist_Elevations.xlsx"))|>
  filter(group==group_name)


# Load the shapefiles 
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/GARD_2022/groups/", group_name, ".shp")

# Load the shapefile
reptile_shapes <- sf::st_read(file_path, options = "ENCODING=ISO-8859-1")|> 
  dplyr::rename(sciname = binomial)

# insert API elvatr package
topo_key <-"" #insert you API key
elevatr::set_opentopo_key(topo_key)

#------------------------------#
# 2. Load the mountains  -----
#------------------------------#

#source the gmba regions whith alpine biome
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", 
                                     sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)

# check if there are any invalid shapes
mountain_shapes <- make_shapes_valid(mountain_shapes) 

#-----------------------------------------------------------#
# 3. Load the species geometries (distribution ranges)  -----
#------------------------------------------------------------#

# merge the geometries to the checklist
Checklist_Elev_DEM <- merge(Checklist_Elev, reptile_shapes, by = c("sciname"), all.x = TRUE)

#------------------------------------------------------------------------#
# 4. Get reptile elevational ranges with DEM -----
#-------------------------------------------------------------------------#

# Filter group to process
group <- Checklist_Elev_DEM |> 
  filter(group == group_name)|>
  distinct(sciname, Mountain_range, Mountain_system, .keep_all = TRUE)

# Define the focus GMBA systems 
Focus_GMBA_systems<-unique(group$Mountain_system)

# Validate the shapes in the df to process
group<- validate_shapes_individually(group)

# This is the old function from the mammal workflow --> new one has to be refined
results_dem_df <- extract_elevational_ranges(group, Focus_GMBA_systems)


# Bind the dataframes togeher
results_dem_df_b <- group|> 
  left_join(results_dem_df,by=c("sciname","Mountain_range","Mountain_system"))|>
  rename(max_elevation_validation = max_elevation)|>
  rename(min_elevation_validation = min_elevation)|>
  sf::st_as_sf(results_dem_df_b)|> 
  sf::st_set_geometry(NULL)

#-------------------------------#
# 5. Restructure Dataframes -----
#--------------------------------#

quantile_info <- results_dem_list$quantile_info
results_dem_df <- results_dem_list$results

results_dem_df <- test|> 
  left_join(results_dem_df,by=c("sciname","Mountain_range","Mountain_system"))|>
  select(-geometry)|>
  rename(max_elevation_validation = max_elevation)|>
  rename(min_elevation_validation = min_elevation)|>
  left_join(quantile_info,by=c("sciname","Mountain_range"))


#---------------------------#
# 6. Save data -----
#--------------------------#

# Define the dynamic file path
file_path <- file.path(data_storage_path, 
                       "subm_global_alpine_biodiversity/Data/Reptiles/processed/", 
                       paste0("Reptiles_Checklist_Elevations_DEM_", group_name, ".xlsx"))

# Save the file
writexl::write_xlsx(results_dem_df_b, file_path)










