#-----------------------------------------------------------------------------------#
#         Data Preparation to visualize and analayse Mammals above the treeline
#------------------------------------------------------------------------------------#


# Load the elevation data
Data_mammals <- readxl::read_excel(paste0(data_storage_path, "Mammals/Output/Checklist/Checklist_Mammals_elevations_DEM_all_mountains.xlsx"))|>
  rename(min_elevation = min_elevation_validation,
         max_elevation = max_elevation_validation)

# check for duplicates
duplicates <- Data_mammals|>
  distinct(sciname, Mountain_range, Mountain_system, .keep_all = TRUE)

# 
#----------------------------------------------------------#
# 2. Create Conditions which elevations are used for mammals ---
#----------------------------------------------------------#
# If species occurs in one mountain system and has min and max elevation (MDD) - USE
# If species occurs in one mountain system and has only min OR max (GARD) - Use GARD and respective other DEM
# If species occurs in > one mountain system OR has NO min and max (GARD) USE DEM

Data_mammals <- Data_mammals |>
  # Add a column that counts the number of unique mountain systems per species
  group_by(sciname) |>
  mutate(unique_mountain_systems = n_distinct(Mountain_system)) |>
  ungroup() |>
  # Apply conditions
  rowwise() |>
  mutate(
    min_elevation_USE = case_when(
      # If more than one mountain system, always use min_elev_DEM
      unique_mountain_systems > 1 ~ min_elev_DEM,
      # If only one mountain system and both elevations available
      unique_mountain_systems == 1 & !is.na(min_elevation) & !is.na(max_elevation) ~ min_elevation,
      # If only one mountain system and min available but max is not
      unique_mountain_systems == 1 & !is.na(min_elevation) & is.na(max_elevation) ~ min_elevation,
      # If only one mountain system and max available but min is not
      unique_mountain_systems == 1 & is.na(min_elevation) & !is.na(max_elevation) ~ min_elev_DEM,
      # Otherwise
      TRUE ~ min_elev_DEM
    ),
    max_elevation_USE = case_when(
      # If more than one mountain system, always use max_elev_DEM
      unique_mountain_systems > 1 ~ max_elev_DEM,
      # If only one mountain system and both elevations available
      unique_mountain_systems == 1 & !is.na(min_elevation) & !is.na(max_elevation) ~ max_elevation,
      # If only one mountain system and max available but min is not
      unique_mountain_systems == 1 & is.na(max_elevation) & !is.na(min_elevation) ~ max_elev_DEM,
      # If only one mountain system and min available but max is not
      unique_mountain_systems == 1 & !is.na(max_elevation) & is.na(min_elevation) ~ max_elevation,
      # Otherwise
      TRUE ~ max_elev_DEM
    )
  ) |>
  # drop the temporary columns
  select(-unique_mountain_systems)

#-----------------------------------------------------------------------------------------------------------------------------#
# 2. Mutate the treeline elevations and calculate how much min elevation is below the treeline 
#------------------------------------------------------------------------------------------------------------------------------#
Treeline_Elevations <- readxl::read_excel(paste0(data_storage_path,"Mountains/Tree_Line/Treeline_Lapse_Rate_04_05.xlsx"))

# Join with treeline elevations
Data_mammals <- Data_mammals|>
  left_join(Treeline_Elevations,by =c("Mountain_range","Mountain_system")) |> 
  rename(Mean_elevation_treeline = Mean_elevation) |># calculate how many m of species min and max limit is above and below the treeline
  mutate(
    min_rel_treeline = min_elevation_USE - Mean_elevation_treeline,
    max_rel_treeline = max_elevation_USE - Mean_elevation_treeline
  )

# The column to use now is min/max elevation USE
species_richness_mammals <- Data_mammals |>
  group_by(Mountain_range) |>
  summarise(species_richness = n_distinct(sciname))

#--------------------------------------------------#
# 3. Mutate information about species endemism
#---------------------------------------------------#

Data_mammals <- Data_mammals |> 
  group_by(sciname)|> 
  mutate(unique_mountain_range = n_distinct(Mountain_range))|>
  ungroup()|>
  mutate(endemic = if_else(unique_mountain_range==1, "YES","NO"))

