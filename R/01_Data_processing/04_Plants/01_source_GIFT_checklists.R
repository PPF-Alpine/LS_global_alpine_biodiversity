#----------------------------------------------------------#
#                 Source plant checklists from GIFT
#----------------------------------------------------------#

# Source plant checklists that fall within or overlap with the gmba shapefile
# check vignette for details
# https://biogeomacro.github.io/GIFT/articles/GIFT_advanced_users.html
# https://biogeomacro.github.io/GIFT/articles/GIFT.html

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

library(GIFT)


# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#❗❗❗ REMOVE RESTRICTED API ❗❗❗

api_rst <-"https://giftuser:Hepaticanobilis*@gift.uni-goettingen.de/api/restricted/"


#----------------------------------------------------------#
# 2. Download all checklists with vascular plants  -----
#----------------------------------------------------------#

# these checklists do not yet include information about elevation ranges

gift_checklists <- GIFT::GIFT_checklists(taxon_name = "Tracheophyta",
                                         complete_taxon = TRUE,
                                         floristic_group = "all",
                                         complete_floristic = TRUE, 
                                         geo_type = "All", 
                                         taxonomic_group = TRUE,
                                         GIFT_version = "beta",
                                         api = api_rst)


# to source only inidivual checklists run:
# gift <- GIFT_checklists_raw(ref_ID = c(10712, 10701, 10699), api = api_rst)

#------------------------------------------------------------------------------------
# Save the checklists
#-----------------------------------------------------------------------------------

RUtilpol::save_latest_file(
  object_to_save = gift_checklists,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)



