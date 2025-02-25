

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


# STEP 1: compile all lists that have been returned by experts
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Reptiles/01_compile_returned_expertdata_reptiles.R")
)

# STEP 2: Clean all lists that have been returned
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Reptiles/02_clean_returned_expertdata_reptiles.R")
)

# STEP 3: Source all lists for mountain ranges that have not been validated
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Reptiles/03_combine_val_nonval_reptile_data.R")
)

reptiles_dfs <- ls(pattern = "^Reptiles")

combined_reptiles <- bind_rows(mget(reptiles_dfs), .id = "source")

# check if there is any NA that could later mess up figures
na_check_elev <- combined_reptiles |>
  filter(is.na(sciname)| is.na(min_elevation) | is.na(max_elevation))

na_check_treeline <- combined_reptiles |>
  filter(is.na(mean_treeline))


# save the removed species data frame
output_path <- paste0(data_storage_path, "/Biodiversity_combined/Final_lists/alpine_reptile_database.xlsx")

writexl::write_xlsx(combined_reptiles, output_path)
