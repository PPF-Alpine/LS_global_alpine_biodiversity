#----------------------------------------------------------#
#         Get elevations with DEM 
#----------------------------------------------------------#

# In this script I extract quartiles for species min and max elevations from their range shps using DEM

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

# Read the checklist that includes the elevation data
Checklist_Elev <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Elevations_Qu_J.xlsx"))

# load shapefile
file_path <- paste0(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/birdlife_merge.shp"))

# 
birds_shapes <- sf::st_read(file_path, options = "ENCODING=ISO-8859-1")|> 
  dplyr::rename(sciname = sci_nam)|> 
  select(sciname,geometry)

# insert API for elevatr package
# https://cran.r-project.org/web/packages/elevatr/elevatr.pdf
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
# 3. Define chunks to reduce computation time   -----
#------------------------------------------------------------#

# merge the geometries to the checklist
Checklist_Elev_DEM_merge <- merge(Checklist_Elev, birds_shapes, by = c("sciname"), all.x = TRUE)

#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1:500,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[501:1000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1001:1500]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1501:3500,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[3501:7000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[7001:10001,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[10001:13000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[13001:16000,]
Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[16001:19617,]
#------------------------------------------------------------------------#
# 4. Get birds elevational ranges with DEM -----
#-------------------------------------------------------------------------#

# Define the focus GMBA systems 
Focus_GMBA_systems<-unique(Checklist_Elev_DEM$Mountain_system)

# Validate the shapes in the df to process
Checklist_Elev_DEM<- validate_shapes_individually(Checklist_Elev_DEM)


# This is the old function from the mammal workflow --> new one has to be refined
results_dem_df <- extract_elevational_ranges(Checklist_Elev_DEM, Focus_GMBA_systems)


# Bind the dataframes togeher
results_dem_df_b <- Checklist_Elev_DEM|> 
  left_join(results_dem_df,by=c("sciname","Mountain_range","Mountain_system"))|>
  rename(max_elevation_validation = max_elevation)|>
  rename(min_elevation_validation = min_elevation)|>
  sf::st_as_sf(results_dem_df_b)|> 
  sf::st_set_geometry(NULL)

results_dem_df_b <- results_dem_df_b|> 
  sf::st_set_geometry(NULL)
#---------------------------#
# 6. Save data -----
#--------------------------#

# write the individual chunks
writexl::write_xlsx(results_dem_df_b, data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Checklist_Elevations_DEM_16001_19617.xlsx")







