
# In this file I clean returned expertdata for mammals


# Get the list of validated data frames 
reptiles_dfs <- ls(pattern = "^Reptiles")

# empty data frame to store removed species
removed_species <- data.frame()

# Loop through each data frame and clean  expert assessment
for (df_name in reptiles_dfs) {
  # Get the data frame
  df <- get(df_name)
  
  # Identify species to be removed
  species_to_remove <- df |> filter(mountain_range_corrected == "no" & !is.na(mountain_range_corrected))
  
  # there are some characters in elevation fields
  species_to_remove <- species_to_remove |> 
    mutate(across(everything(), as.character))
  
  # Add species to the removed_species data frame
  removed_species <- bind_rows(removed_species, species_to_remove)
  
  # Remove species that do not occur in that mountain range according to expert
  df <- df |> filter(!(mountain_range_corrected == "no" & !is.na(mountain_range_corrected)))
  
  # Replace corrected elevational info and store old values if corrected values are used
  df <- df |>
    mutate(
      old_min_elevation = ifelse(!is.na(min_corrected), min_elevation, NA),
      old_max_elevation = ifelse(!is.na(max_corrected), max_elevation, NA),
      min_elevation = ifelse(!is.na(min_corrected), min_corrected, min_elevation),
      max_elevation = ifelse(!is.na(max_corrected), max_corrected, max_elevation),
      source_min_elevation = ifelse(!is.na(min_corrected), "refined by expert", source_min_elevation),
      source_max_elevation = ifelse(!is.na(max_corrected), "refined by expert", source_max_elevation)
    )
  
  # Convert min_elevation and max_elevation columns to numeric
  df <- df |>
    mutate(min_elevation = as.numeric(replace_na(min_elevation, 0)),
           max_elevation = as.numeric(max_elevation),
           old_min_elevation = as.numeric(old_min_elevation),
           old_max_elevation = as.numeric(old_max_elevation),
           mean_treeline = as.numeric(mean_treeline))
  
  # Remove the columns that are not needed anymore and add expert validation flag
  df <- df |> 
    select(-mountain_range_corrected, -min_corrected, -max_corrected, -validated_elevation_data) |>
    mutate(expert_validated = "yes")
  
  # Assign the modified data frame back to the original name
  assign(df_name, df)
}

# save the removed species data frame
output_path <- paste0(data_storage_path, "/Biodiversity_combined/Expert_validation/Checklists_validated/removed_reptiles.xlsx")

writexl::write_xlsx(removed_species, output_path)
