

#----------------------------------------------------------#
#       Source the GIFT polyogons for each checklist
#----------------------------------------------------------#

# polygons delineate each GIFT region. they are sourced in this script



#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

# load checklists with elevations
checklists_elevations <- RUtilpol::get_latest_file(
  "checklists_elevations",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)


#--------------------------------------------------------------------#
# 1. Download shape files of GIFT regions -----
#--------------------------------------------------------------------#

# Download shapes for all entity IDs
entity_ids <- unique(checklists_elevations$entity_ID)

gift_shapes <- map(entity_ids, ~ GIFT::GIFT_shapes(entity_ID = .x, 
                                                   GIFT_version = "beta", 
                                                   api = api_rst))



gift_shapes <- lapply(entity_ids, function(x) { GIFT::GIFT_shapes(entity_ID = x, GIFT_version = "beta",api=api_rst)})


gift_shapes_df <- do.call(rbind,gift_shapes)
gift_shapes_df <- sf::st_as_sf(gift_shapes_df)


#--------------------------------------------------------------------------#
# 2. Intersect with alpine biome and filter those which fall within AB -----
#---------------------------------------------------------------------------#

# load alpine biome 
alpine_biome <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/alpine_biome.shp", sep = "/"))|>
  rename(Mountain_range = Mntn_rn)|>
  rename(area_size = area_sz)|>
  rename(log_area = log_are)


intersections <- sf::st_intersects(gift_shapes_df,alpine_biome,sparse=FALSE)

#--------------------------------------------------------------------#
# 3. Filter out those shapes and lists which are not in alpine biome -----
#--------------------------------------------------------------------#

# filter the shapes
gift_shapes_filtered <- gift_shapes_df[rowSums(intersections) > 0, ]

# now filter also the lists
checklists_elevations_filtered <- checklists_elevations |>
  filter(entity_ID %in% gift_shapes_filtered$entity_ID)|>
  left_join(gift_shapes_filtered |> 
              select(entity_ID, geo_entity,geometry), 
            by = "entity_ID")|>
  select(ref_ID,
         ref_long,
         entity_ID,
         geo_entity,
         work_species,
         min_elev,
         max_elev,
         total_species,
         prop_both)

#------------------------------------------------------------------------------------
# Save the data
#-----------------------------------------------------------------------------------

RUtilpol::save_latest_file(
  object_to_save = gift_shapes_filtered,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/"),  
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = checklists_elevations_filtered,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/"),  
  prefered_format = "rds",
  use_sha = TRUE
)

