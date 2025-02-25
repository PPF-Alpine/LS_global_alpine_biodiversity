

# In this script I compile all lists that have been validated by experts and combine with non validated 


#-------------
# Reptiles Folder
#-----------------

# load the validated lists (all lists in the folder) the files in the folder are named with the mountain range and the name of expert who validated

folder_path <- paste0(data_storage_path,"Biodiversity_combined/Expert_validation/Checklists_validated/Reptiles")


# mutate a column with the name who validated 

# List all .xlsx files in the directory
file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# extract the expert name from the file name
extract_expert_name <- function(file_name) {
  parts <- strsplit(file_name, "_")[[1]]
  expert_name <- paste(parts[(length(parts)-1):length(parts)], collapse = " ")
  return(expert_name)
}

# extract the mountain range name without expert name
extract_base_name <- function(file_name) {
  parts <- strsplit(file_name, "_")[[1]]
  base_name <- paste(parts[1:(length(parts)-2)], collapse = "_")
  return(base_name)
}

# Load each file into a data frame, add the expert name, and rename the data frame
for (file in file_list) {
  # Extract file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  # Extract expert name
  expert_name <- extract_expert_name(file_name)
  # Extract mountain range name
  base_name <- extract_base_name(file_name)
  df <- read_excel(file)
  
  # Add the expert name as a new column
  df <- df |>
    mutate(expert_name = expert_name)
  # Assign data frame to a variable with the base name
  assign(base_name, df)
}



