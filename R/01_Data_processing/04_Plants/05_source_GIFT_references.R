
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
checklists_elevations_filtered <- RUtilpol::get_latest_file(
  "checklists_elevations_filtered",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)


#--------------------------------------------------------------------#
# 4. Get references and regions from GIFT  -----
#--------------------------------------------------------------------#

# get the references 
gift_references <- GIFT::GIFT_references(api=api_rst,GIFT_version = "beta")
gift_regions <- GIFT::GIFT_regions(api=api_rst,GIFT_version = "beta")

#filter the references

gift_regions <- gift_regions|>
  filter(entity_ID%in%checklists_elevations_filtered$entity_ID)

gift_references <- gift_references|>
  filter(ref_ID%in%checklists_elevations_filtered$ref_ID)
