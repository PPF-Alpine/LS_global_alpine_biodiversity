

# In this file I combine validated and non validated data in one dataset



# Get the list of validated data frames 
mammals_dfs <- ls(pattern = "^Mammals")

folder_path_all <- paste0(data_storage_path,"Biodiversity_combined/Expert_validation/Checklists/Mammals/All_Lists")


# List all .xlsx files in the directory
all_files <- list.files(path = folder_path_all, pattern = "\\.xlsx$", full.names = TRUE)

# Extract the base names of the validated data frames
validated_basenames <- sapply(mammals_dfs, function(x) x)

# Filter out the files that are already validated
new_files <- all_files[!basename(tools::file_path_sans_ext(all_files)) %in% validated_basenames]

# Define columns to be removed
columns_to_remove <- c("mountain_range_corrected", "min_corrected", "max_corrected", "validated_elevation_data", "confidence_assessment", "alpine_status", "reviewer_comments")

# Load each new file into a data frame, remove specific columns if present, and assign it to a variable named after the file
for (file in new_files) {
  # Extract file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  # Read the Excel file
  df <- read_excel(file)
  # Remove columns 
  df <- df |> select(-one_of(columns_to_remove))|>
    mutate(expert_validated = "no")
  # Assign data frame to a variable with the file name
  assign(file_name, df)
}

# Check the newly created data frames
ls()
