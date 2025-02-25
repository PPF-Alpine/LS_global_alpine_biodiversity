#----------------------------------------------------------#
#         Investigate the Checklist for Birds
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
# 2. Load species data a--
#----------------------------------------------------------#

# Read the checklist that includes the elevation data
Checklist_Elev_DEM <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Checklist_Elevations_DEM.xlsx"))

# In case there are some duplicates --> check length
Checklist_Elev_DEM_subs <- Checklist_Elev_DEM|>
  distinct(sciname, Mountain_range, Mountain_system, .keep_all = TRUE)


lowov <- Checklist_Elev_DEM |> 
  filter(overlap_percentage_mountain<=1)|>
  distinct(sciname)


highov <- Checklist_Elev_DEM |> 
  filter(overlap_percentage_mountain>=1)|>
  distinct(sciname)

#----------------------------------------------------------#
# 3. Which elevation data to use 
#----------------------------------------------------------#

# elevation data Quintero and Jetz but spatial unit ? 
birds_elevations <- readxl::read_xlsx(paste0(data_storage_path,"Birds/Input/Elevation_Data/Birds_Elevations_Qu_J.xlsx"))|>
  mutate(Mountain_range_Qu_J = case_when(            # For the Andes Qu J distinguish between ARG, BOL, Col, Ecu, Per, Ven
    country == "ARG" ~ "Southern Andes",
    country == "CHL" ~ "Southern Andes",
    country == "BOL" ~ "Central Andes",
    country == "COL" ~ "Northern Andes",
    country == "ECU" ~ "Northern Andes",
    country == "PER" ~ "Central Andes",
    country == "VEN" ~ "Northern Andes",
    TRUE ~ Mountain_range_Qu_J # 
  ))

mountains_qu_j <- birds_elevations |> 
  distinct(Mountain_range_Qu_J)

mountains_checklist <- Checklist_Elev_DEM |> 
  distinct(Mountain_range)

# These have to be merged and matche --> correspond to different spatial units of GMBA version 1 and 2 





