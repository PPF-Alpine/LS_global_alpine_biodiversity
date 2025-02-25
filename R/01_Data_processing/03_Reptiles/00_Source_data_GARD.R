#----------------------------------------------------------#
#         Source Reptile distribution Data from GARD
#----------------------------------------------------------#

# zips can be downloaded via the GARD Global Assessment of Reptile Distribution v.1.7: http://www.gardinitiative.org/data.html

# global distributions of 10914 reptile species merged and collated from verious sources

#Roll et. al. 2017 The global distribution of tetrapods reveals a need for targeted reptile conservation. Nature Ecology & Evolution 1:1677-1682

#Caetano, et al. 2022. Automated assessment reveals that the extinction risk of reptiles is widely underestimated across space and phylogeny. PLoS Biology, 20(5): e3001544.

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
reptile_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Reptiles/GARD_2022/Gard_1_7_ranges.shp", sep = "/"),options = "ENCODING=ISO-8859-1")

reptile_shapes <- make_shapes_valid(reptile_shapes)

#----------------------------------------------------------#
# 3. Check out data  -----
#----------------------------------------------------------#
# Check out data structure
reptile_shapes_df <- as.data.frame(reptile_shapes)


reptile_shapes_df |> 
  group_by(group) |> 
  summarise(num_species = n_distinct(binomial))

# there are 6 groups of reptiles
# lizards are the biggest group with > 6000 species

#----------------------------------------------------------#
# 4. Split the data by group  -----
#----------------------------------------------------------#

# Split the data by 'group'
groups_list <- split(reptile_shapes, reptile_shapes$group)

# Loop through each group and save as a new shapefile
for (group_name in names(groups_list)) {
  # output file name
  output_file_name <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/GARD_2022/groups/", group_name, ".shp")
  
  # Write the shapefile
  sf::st_write(groups_list[[group_name]], output_file_name, delete_dsn = TRUE)
}
