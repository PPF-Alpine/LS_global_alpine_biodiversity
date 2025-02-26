
# combine rds files after DEM elevation extraction

# List all .rds files in the directory
rds_files <- list.files(paste0(data_storage_path,"/subm_global_alpine_biodiversity/Data/Mammals/processed/DEM"), pattern = "\\.rds$", full.names = TRUE)

# Load all .rds files into a list of dataframes
data_list <- lapply(rds_files, readRDS)

# Combine all dataframes 
combined_data <- bind_rows(data_list)

# Save the combined dataframe as an .rds file
saveRDS(combined_data, file = paste0(data_storage_path,"/subm_global_alpine_biodiversity/Data/Mammals/processed/DEM/Checklist_Mammals_elevations_DEM_all_mountains.rds"))
