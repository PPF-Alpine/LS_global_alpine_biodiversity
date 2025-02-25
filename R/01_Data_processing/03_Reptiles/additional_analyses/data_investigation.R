#----------------------------------------------------------#
# check out missing data 
#----------------------------------------------------------#

# Load the shapefiles 
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/GARD_2022/Gard_1_7_ranges.shp")
reptile_shapes <- sf::st_read(file_path, options = "ENCODING=ISO-8859-1")
reptile_shapes <- reptile_shapes |> rename(sciname = binomial)

# Load the elevation data
Elevation_data_Reptiles <- read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/Reptiles_Checklist_Elevations.xlsx")) |> 
  rename(sciname = 'Species name (Binomial)')|>
  rename(min_elevation = "Minimum elevation (m)")|>
  rename(max_elevation = "Maximum elevation (m)")


# Full join to see complete dataset
elevation_species_full <- reptile_shapes|> full_join(Elevation_data_Reptiles,by = "sciname")|> 
  sf::st_set_geometry(NULL)|>
  arrange(sciname)

# Subsetting the DataFrame 
subset_elevation_species <- elevation_species_full|> select("area", "sub-order")|>
  rename("Missing Distribution Data"="area",
         "Missing Elevation Data"="sub-order")


# Using vis_miss to visualize missing data
vis_miss(subset_elevation_species)


# Left join to see only the data where we have distribution data
elevation_species_left <- reptile_shapes|> left_join(Elevation_data_Reptiles,by = "sciname")|> 
  sf::st_set_geometry(NULL)|>
  arrange(sciname)

subset_elevation_species <- elevation_species_left|> select("sub-order")|>
  rename("Missing Elevation Data"="sub-order")


# Using vis_miss to visualize missing data
vis_miss(subset_elevation_species)
