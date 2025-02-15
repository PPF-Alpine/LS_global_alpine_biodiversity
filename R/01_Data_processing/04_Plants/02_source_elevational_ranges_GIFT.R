#----------------------------------------------------------#
#       Source elevational ranges from GIFT
#----------------------------------------------------------#

# retrieve elevational information from GIFT database

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#❗❗❗ REMOVE RESTRICTED API ❗❗❗

api_rst <-"https://giftuser:Hepaticanobilis*@gift.uni-goettingen.de/api/restricted/"

#----------------------------------------------------------#
# 2. Get elevational ranges  -----
#----------------------------------------------------------#

# check out trait meta data
# elevational ranges are stored as "traits" in GIFT
trait_meta <- GIFT::GIFT_traits_meta(GIFT_version = "beta",
                                     api = api_rst)

# Elevational ranges
trait_meta[which(trait_meta$Trait2 %in% c("Elevational_range_min", 
                                          "Elevational_range_max", 
                                          "Elevational_range_mean")), ]


# GIFT_traits() returns aggregated trait value (mean) at the species level --> cannot use this one for elevational ranges
# GIFT_traits_raw () returns all trait values for a given species and a given trait.


elevations_raw <- GIFT::GIFT_traits_raw(trait_IDs = c("6.1.1","6.1.2","6.1.3"), 
                                        derived=TRUE,
                                        bias_ref = FALSE,
                                        bias_deriv = FALSE, 
                                        GIFT_version = "beta",
                                        api=api_rst)

# saveRDS(elevations_raw,paste0(data_storage_path,"Biodiversity_combined/Plants/GIFT_download/GIFT_elevations_raw_download.rds"))

elevations_raw<-readRDS(paste0(data_storage_path,"Biodiversity_combined/Plants/GIFT_download/GIFT_elevations_raw_download.rds"))

#----------------------------------------------------------#
# 3. Clean elevational ranges  -----
#----------------------------------------------------------#

elevations_clean <- elevations_raw |>
  distinct(work_species, 
           geo_entity_ref,
           trait_ID,
           trait_value, .keep_all = TRUE)|>
  arrange(work_species,
          geo_entity_ref)



# Some single elevation values are in character format (i.e., c("700","900")) so we need to clean these entries

elevations_clean$trait_value <- map_dbl(elevations_clean$trait_value, ~ {
  # If it's a list/vector
  if (length(.x) > 1) {
    return(mean(as.numeric(.x)))
  }
  
  # If it's a comma-separated string
  if (is.character(.x) && str_detect(.x, ",")) {
    return(mean(as.numeric(str_split(.x, ",")[[1]])))
  }
  
  # Otherwise, convert directly
  as.numeric(.x)
})

# clean obviously wrong rows (negative elevational limits or extremely high values)
elevations_clean <- elevations_clean |>
  filter(trait_value >= 0, trait_value <= 7000)


#------------------------------------------------------------------------------------
# Save the data
#-----------------------------------------------------------------------------------

RUtilpol::save_latest_file(
  object_to_save = elevations_raw,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = elevations_clean,  # Pass the object directly
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/"),  # Use file.path for paths
  prefered_format = "rds",
  use_sha = TRUE
)
