

#----------------------------------------------------------#
#       Join elevational ranges with checklists
#----------------------------------------------------------#

# in this script we join the two previous step. joining for each region the species with their respective elevational limits


#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

# load checklists
gift_checklists <- RUtilpol::get_latest_file(
  "gift_checklists",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)

# load cleaned elevations
elevations_clean <- RUtilpol::get_latest_file(
  "elevations_clean",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)

#-----------------------------------------------------------------------#
# 1. Subset only the checklists which have elevational data available--
#------------------------------------------------------------------------#
ref_ids <- unique(elevations_clean$ref_ID)

# Subset each dataframe in the list based on the ref_ID values
gift_checklists_elevations <- gift_checklists |>
  map(~ filter(.x, ref_ID %in% ref_ids))

# this results in a list containing
# 1. the species checklists for each gift region 
# 2. metadata on each gift region 


checklists <- gift_checklists_elevations$checklists
metadata <- gift_checklists_elevations$lists

#----------------------------------------------------------#
# 2. join elevational ranges with checklists  --
#----------------------------------------------------------#

# first join elevational ranges with species in the checklists 
checklists_elevations <- checklists |>
  left_join(elevations_clean |>
              select(ref_ID,trait_ID, trait_value, work_species,geo_entity_ref,ref_long), 
            by = c("ref_ID","work_species"),
            relationship="many-to-many") |>
  select(ref_ID,
         entity_ID,
         work_species,
         trait_ID,
         trait_value,
         geo_entity_ref,
         ref_long)

# Pivot the trait values to columns and rename the trait IDs
checklists_elevations <- checklists_elevations|> 
  pivot_wider(                        
    names_from = trait_ID,
    values_from = trait_value,
    values_fill = list(trait_value = NA),
    values_fn = list(trait_value = mean)
  ) |>
  rename(
    min_elev = "6.1.1",
    max_elev = "6.1.2",
    mean_elev = "6.1.3")|>
  select(-"NA")


#----------------------------------------------------------------------------#
# 3. Calculate percentage of elevational data coverage for each gift region
#---------------------------------------------------------------------------#

# calculate the proportion of species for which elevational information is available per geo unit 
checklists_elevations <- checklists_elevations |>
  group_by(entity_ID) |>
  mutate(
    total_species = n(),
    count_min_elev = sum(!is.na(min_elev)),
    count_max_elev = sum(!is.na(max_elev)),
    count_both = sum(!is.na(min_elev) & !is.na(max_elev)),
    prop_min_elev = round(count_min_elev / total_species, 2),
    prop_max_elev = round(count_max_elev / total_species, 2),
    prop_both = round(count_both / total_species, 2)
  ) |>
  ungroup() |>
  select(-count_min_elev,-count_max_elev,-count_both)

#--------------------------------------------------------------------#
# 4. Filter those lists which have at least 70% elev coverage --
#--------------------------------------------------------------------#

checklists_elevations <- checklists_elevations|> 
  filter(prop_both>=0.7)


#------------------------------------------------------------------------------------
# Save the data
#-----------------------------------------------------------------------------------

RUtilpol::save_latest_file(
  object_to_save = checklists_elevations,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)
