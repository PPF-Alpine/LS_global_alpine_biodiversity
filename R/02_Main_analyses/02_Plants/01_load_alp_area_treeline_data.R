
#----------------------------------------------------------#
#           Get alpine area in GIFT regions
#----------------------------------------------------------#



#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#


# Load configuration
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
# Load GIFT Area Size and treeline data  -----
#----------------------------------------------------------#

# ❗ area extent and treeline elevation of the alpine biome have been calculated in ArcGISPRO 

# load the area size of GIFT 
area_size_GIFT <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/area_size_overlap_gift_ab.xlsx"))|>
  select(geo_entity,alpine_biome,area_size_gift)|>
  rename(Mountain_range=alpine_biome)

# load the size of alpine area in each geo entity
alpine_area_GIFT <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/alpine_area_GIFT.xlsx"))|>
  rename(alpine_area=area_size)|>
  rename(treeline_GIFT = mean_treeline)

# join the area sizes 
alpine_area_treeline_GIFT <- area_size_GIFT |> 
  dplyr::full_join(alpine_area_GIFT, by = "geo_entity")|>
  mutate(perc_alpine_area = alpine_area/area_size_gift*100)|>
  mutate(log_perc_alpine_area=log1p(perc_alpine_area))

# join treeline and lapse rates 
treeline_elevations <- openxlsx::read.xlsx(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/Treeline_Lapse_Rates_GIFT.xlsx"))|>
  select(geo_entity,
         treeline_GIFT_1_degree,
         treeline_GIFT_2_degree,
         treeline_GIFT_3_degree,
         treeline_GIFT_4_degree,
         treeline_GIFT_6_degree,
         treeline_GIFT_8_degree)


RUtilpol::save_latest_file(
  object_to_save = alpine_area_treeline_GIFT,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)


#----------------------------------------------------------#
# Load Checklist data  -----
#----------------------------------------------------------#

# load checklists with elevations
species_list <- RUtilpol::get_latest_file(
  "checklists_elevations_filtered",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)


# join the overlapping alpine biomes with the area sizes
species_list <- alpine_area_treeline_GIFT |> 
  dplyr::full_join(species_list, by = "geo_entity")|>
  distinct()


# join with the treeline 
species_list <- species_list|> 
  full_join(treeline_elevations,by="geo_entity")|>
  distinct()

# Remove rows with no treeline data
species_list <- species_list |>
  filter(!is.na(treeline_GIFT), !is.na(treeline_GIFT_1_degree))

#------------------------------------------------------------------------------------
# Save the results dataframe 
#-----------------------------------------------------------------------------------

RUtilpol::save_latest_file(
  object_to_save = species_list,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = alpine_area_treeline_GIFT,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)
