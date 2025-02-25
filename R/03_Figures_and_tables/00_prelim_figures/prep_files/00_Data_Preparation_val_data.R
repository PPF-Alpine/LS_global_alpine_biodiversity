# Data Preparation for figures

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gridExtra)
library(svglite)

# get the expert validated databases for reptiles, birds and mammals
mammals<- #readxl::read_excel("Data/Input/species_data/alpine_mammal_database.xlsx")|>
  readxl::read_excel(paste0(data_storage_path, "/Biodiversity_combined/Final_lists/alpine_mammal_database.xlsx"))|>
  select(sciname,Mountain_system,Mountain_range,mean_treeline,min_elevation,max_elevation,expert_validated,alpine_status)|>
  mutate(group="mammals")

birds<- #readxl::read_excel("Data/Input/species_data/alpine_bird_database.xlsx")|>
  readxl::read_excel(paste0(data_storage_path, "/Biodiversity_combined/Final_lists/alpine_bird_database.xlsx"))|>
  select(sciname,Mountain_system,Mountain_range,mean_treeline,min_elevation,max_elevation,expert_validated,alpine_status)|>
  mutate(group="birds")

reptiles<- #readxl::read_excel("Data/Input/species_data/alpine_reptile_database.xlsx")|>
  readxl::read_excel(paste0(data_storage_path, "/Biodiversity_combined/Final_lists/alpine_reptile_database.xlsx"))|>
  select(sciname,Mountain_system,Mountain_range,mean_treeline,min_elevation,max_elevation,expert_validated,alpine_status)|>
  mutate(group="reptiles")

checklist_combined<-bind_rows(mammals,birds,reptiles)


#-----------------------------------------------------------------------------------------------------------------------------#
# 2. Mutate the treeline elevations and calculate how much min elevation is below the treeline 
#------------------------------------------------------------------------------------------------------------------------------#
#Treeline_Elevations <- readxl::read_xlsx(paste0(data_storage_path,"Mountains/Tree_Line/Treeline_Karger_Complete_New.xlsx"))

Treeline_Elevations <- readxl::read_excel("Data/Input/mountain_data/Treeline_Lapse_Rate_04_05.xlsx")
  #readxl::read_excel(paste0(data_storage_path,"Mountains/Tree_Line/Treeline_Lapse_Rate_04_05.xlsx"))

# Join with treeline elevations
checklist_combined <- checklist_combined|>
  left_join(Treeline_Elevations,by = c("Mountain_range","Mountain_system"))|>
  select(-Mean_elevation) |># calculate how many m of species min and max limit is above and below the treeline
  mutate(
    min_rel_treeline = min_elevation - mean_treeline,
    max_rel_treeline = max_elevation - mean_treeline
  )|>
  select(group,sciname,Mountain_system,Mountain_range,alpine_status,expert_validated,
         min_elevation,max_elevation,
         mean_treeline,Mean_elevation_1_degree,Mean_elevation_2_degree,
         Mean_elevation_4_degree,Mean_elevation_6_degree,Mean_elevation_8_degree)


# load the table with the area size of each mountain range
area_size <- readxl::read_excel("Data/Input/mountain_data/Alpine_Biome_Area_size.xlsx")|>
  #readxl::read_excel(paste0(data_storage_path, "Mountains/Suzette_Alpine_biome/Alpine_Biome_Area_size.xlsx")) |>
  select(Mountain_range,area_size)|>
  mutate(log_area=log1p(area_size))


checklist_selected <- checklist_combined|>
  filter(Mountain_system!="East Siberian Mountains",
         Mountain_system!="Svalbard",
         Mountain_system!="North America Arctic Islands",
         Mountain_range!="Alaska-Yukon Ranges")

# 
expert_validated <- checklist_selected|>
  select(Mountain_range,expert_validated,group)|>
  distinct()
