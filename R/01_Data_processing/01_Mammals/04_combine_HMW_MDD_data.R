#---------------------------------------------------------------------------------#
#         Match the cleaned Handbook of Mammals Dataset with MDD checklist
#--------------------------------------------------------------------------------#

# This script loads the cleaned handbook of mammals and the checklist (with overlaps mountain ranges) 
# and joins the available data by species names

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(readxl)


# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#------------------------#
# 2. Load the data
#-------------------------#

# The elevations
HMW_elevations <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/HMW_cleaned.xlsx"))

# The checklist
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/Checklist_Mammals.xlsx")


# bind sheets into one dataframe
MDD_checklist <- excel_sheets(file_path) |>
  map_df(~process_sheet(.x))

#---------------------------------------#
# 4. Join the HMW data to the checklist
#---------------------------------------#

# Join the mountain system. below a list of mountain ranges with respective names at different lefels
mountain_shapes <- readRDS(file.path(data_storage_path, 
                                     "subm_global_alpine_biodiversity/Data/Mountains/GMBA_names_level_03_04.rds"))|>
  filter(Hier_Lvl=="3")


# left join the mountain system 
MDD_checklist <- MDD_checklist|>
  left_join(mountain_shapes|>
              select(Mountain_range, Mountain_system),by = "Mountain_range") |>
  reorder(Mountain_system,Mountain_range,order,family,sciname,species_area,overlap_percentage_mountain,overlap_percentage_alpine)
  

# join the HMW data by species
HMW_MDD_match <- left_join(MDD_checklist,HMW_elevations, by = "sciname")


#---------------------------------------#
# 5. Save the checklist
#---------------------------------------#

writexl::write_xlsx(HMW_MDD_match,paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/Checklist_Mammals_elevations_HMW.xlsx"))




