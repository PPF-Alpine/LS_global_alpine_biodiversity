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

# Read the checklist that includes the elevation data
Checklist_Elev <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/Checklist_Mammals_elevations_HMW.xlsx"))


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

# Load the latest mammal file
mammals_geom <- RUtilpol::get_latest_file(
  file_name = "mammal_geometries",
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/geom/"))

# merge the geometries to the checklist
Checklist_Elev_DEM_merge <- merge(Checklist_Elev, 
                                  mammals_geom, by = c("sciname", "order"), all.x = TRUE)


#------------------------------------------------------------------------#
# 4. Get mammal elevational ranges with DEM -----
#-------------------------------------------------------------------------#

# The files are large, I therefore process them in chunks

#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1:1000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1001:2000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[2001:3000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[3001:4000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[4001:5000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[5001:6000,]
Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[6001:7397,]

Checklist_Elev_DEM <- Checklist_Elev_DEM |> 
  rename(geometry = geom)|>
  filter(Mountain_system != "East Siberian Mountains")

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

#---------------------------#
# 6. Save data -----
#--------------------------#

# 
saveRDS(results_dem_df_b, data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/DEM/Mammals_Checklist_Elevations_DEM_6001_7397.rds")

