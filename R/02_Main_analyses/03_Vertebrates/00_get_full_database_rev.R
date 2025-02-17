
#----------------------------#
#     Set up 
#----------------------------#

source(here::here("R/00_Config_file.R"))

# libraries
library(here)
library(tidyverse)
library(RUtilpol)

#-------------------------------------------------------------------------#
#     get the expert validated databases for reptiles, birds and mammals
#--------------------------------------------------------------------------#

# 
mammals<- readxl::read_excel(paste0(data_storage_path, "/Biodiversity_combined/Final_lists/alpine_mammal_database.xlsx"))|>
  mutate(group="mammals")

birds<- readxl::read_excel(paste0(data_storage_path, "/Biodiversity_combined/Final_lists/alpine_bird_database.xlsx"))|>
  mutate(group="birds")

reptiles<- readxl::read_excel(paste0(data_storage_path, "/Biodiversity_combined/Final_lists/alpine_reptile_database.xlsx"))|>
  mutate(group="reptiles")

#----------------------------#
#    Combine to one dataset 
#----------------------------#

alpine_vertebrate_dataset<-bind_rows(mammals,birds,reptiles)

# Define exclusion lists
excluded_mountain_systems <- c("East Siberian Mountains", "Svalbard", "North America Arctic Islands")
excluded_mountain_ranges <- c(
  "Alaska-Yukon Ranges", 
  "Aldan Mountains", 
  "Meseta Patagónica", 
  "Sikhote-Alin Area", 
  "Mediterranean islands", 
  "Sierras Pampeanas", 
  "Hawaian Islands", 
  "Maya Highlands", 
  "Balochistan Ranges", 
  "Central China Mountains", 
  "Central Siberian Plateau", 
  "Cordillera de la Costa", 
  "Lena-Angara Plateau", 
  "North Island", 
  "Northern Baikal Mountains", 
  "Selenga Highlands", 
  "Sistema periférico", 
  "Stanovoy Highlands", 
  "Stanovoy Range", 
  "Ural Mountains", 
  "South China Mountains"
)

# Filter out undesired mountain systems and ranges in one step
alpine_vertebrate_dataset <- alpine_vertebrate_dataset|>
  filter(
    !(Mountain_system %in% excluded_mountain_systems),
    !(Mountain_range %in% excluded_mountain_ranges)
  )

length(unique(alpine_vertebrate_dataset$sciname))

alpine_vertebrate_dataset <- alpine_vertebrate_dataset |>
  mutate(
    mountain_range_ID = dense_rank(Mountain_range)  # Rank alphabetically
  )

mountain_range_ID<-alpine_vertebrate_dataset|>
  select(Mountain_system,Mountain_range,mountain_range_ID)|>
  distinct()
#----------------------------#
#    Save latest file
#----------------------------#

RUtilpol::save_latest_file(
  object_to_save = alpine_vertebrate_dataset,  # Pass the object directly
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = mountain_range_ID,  # Pass the object directly
  dir = file.path(data_storage_path, "Biodiversity_combined/Final_lists"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)
