
#---------------------------------------------#
# Extract Elevational Ranges with a DEM  -----
#----------------------------------------------#

# This function first filters the checklist to get species data for the mountain range in the loop for which minimum and maximum elevation is available (evaluation dataset)
# For each species in the evaluation dataset the function intersects species range polygon with each mountain range in which a species occurs
# it then retrieves elevation data for the intersected area for each mountain range
# it then calculates various elevation quantiles for the species in the evaluation dataset and compiles this data into a dataframe
# next, it compares the actual elevation data for each species in the eval dataset for each mountain range with the DEM-extracted elevation data calculating deviations for the various quantile
# based on these deviations the function calculates the mean deviation for each quantile and identifies the min and max quantile with the least average deviation
# Finally, it iterates over all species in the checklist and applies the quantile with the least mean average

extract_elevational_ranges_OLD <- function(Checklist_Elev_DEM, Focus_GMBA_systems) {
  # Initialize a list to store results from all systems
  all_systems_results_list <- list()
  
  # Loop through each focal mountain system
  for (GMBA_system in Focus_GMBA_systems) {
    
    # Filter mountain_shapes for the current system
    mountain_system <- mountain_shapes %>% filter(Mountain_system == GMBA_system)
    
    # Get a list of all mountain ranges within the specified mountain system
    GMBA_ranges <- unique(mountain_system$Mountain_range)
    print(GMBA_ranges)
    
    # Initialize an empty list to store the results for each mountain range in the current system
    all_ranges_elevations_list <- list()
    
    # Iterate over each mountain range within the current system
    for (GMBA_range in GMBA_ranges) {
      
      # Initialize an empty list to store the results for each species within the current range
      results_list <- list()
      
      mountain_range <- mountain_shapes %>% filter(Mountain_range == GMBA_range)
      
      # Filter the species in that mountain range which have recorded elevations (This is my validation dataframe)
      filtered_data <- Checklist_Elev_DEM %>%
        filter(!is.na(min_elevation) & !is.na(max_elevation)) %>%
        group_by(sciname) %>%
        mutate(num_systems = n_distinct(Mountain_system)) %>%
        dplyr::filter(num_systems == 1) %>%
        select(-num_systems)
      
      # Filter the species in that mountain range
      filtered_data <- data.table::data.table(filtered_data)[Mountain_range == GMBA_range, .SD[1], by = sciname] %>% 
        sf::st_as_sf()
      
      
      # If no species data is available for the mountain range, use default quantiles
      default_min_quantile <- "25"  # Default min quantile if no validation data
      default_max_quantile <- "95"  # Default max quantile if no validation data
      
      if(nrow(filtered_data) == 0) {
        message(paste("No species data available for mountain range:", GMBA_range, "Using default quantiles"))
        min_quantile <- default_min_quantile  # CHANGE: Set default min quantile
        max_quantile <- default_max_quantile  # CHANGE: Set default max quantile
      } else {
        message(paste("Processing mountain range:", GMBA_range))
        
        
        # Get unique species
        unique_species <- unique(filtered_data$sciname)
        
        
        # Extract the elevations
        for (i in seq_along(unique_species)) {
          
          tryCatch({
            
            species <- filter(filtered_data, sciname == unique_species[i])
            
            species <- sf::st_as_sf(species)
            
            if (!sf::st_is_valid(species)) {
              species <- sf::st_make_valid(species)
            }
            
            # Intersection of the species data with the mountain range
            cropped_species <- st_intersection(species, mountain_range)
            
            # If there's no data within the bbox skip 
            if (nrow(cropped_species) == 0) {
              next
            }
            
            # Get the elevation data DEM for the cropped region
            elevations_raster <- elevatr::get_elev_raster(cropped_species,
                                                          source = "gl3",
                                                          clip = "location",
                                                          z = 5,
                                                          neg_to_na = TRUE,
                                                          override_size_check = TRUE)
            
            # Convert raster to a vector
            elevations <- as.vector(elevations_raster)
            
            # Calculate quantiles
            quantiles <- quantile(elevations, c(0.05, 0.10, 0.15, 0.20, 0.25, 0.55, 0.60, 0.65, 0.70, 0.75, 0.95), na.rm = TRUE)
            quantiles <- round(quantiles)
            
            # Create data frame with the results
            species_df <- tibble(
              sciname = unique_species[i], 
              min_elev = min(elevations, na.rm = TRUE),
              min_perc_5 = quantiles['5%'],
              min_perc_10 = quantiles['10%'],
              min_perc_15 = quantiles['15%'],
              min_perc_20 = quantiles['20%'],
              min_perc_25 = quantiles['25%'],
              max_perc_55 = quantiles['55%'],
              max_perc_60 = quantiles['60%'],
              max_perc_65 = quantiles['65%'],
              max_perc_70 = quantiles['70%'],
              max_perc_75 = quantiles['75%'],
              max_perc_95 = quantiles['95%'],
              max_elev = max(elevations, na.rm = TRUE)
            )
            
            # Store species_df into the list
            results_list[[i]] <- species_df
            
          }, error = function(e) {
            message(paste("Error processing species:", unique_species[i], "; Error:", e$message))
          })
          
        }
        
        # Combine all species data frames into one data frame
        elevations_MDD <- bind_rows(results_list)
        
        # comparison of actual min and max elevation with the elevations extracted with the DEM
        comparison <- filtered_data %>% select(sciname,min_elevation,max_elevation,Mountain_range,Mountain_system)%>%
          left_join(elevations_MDD,by = "sciname")%>%
          sf::st_set_geometry(NULL)
        
        # Calculate the deviance for each quantile with the actual min and max elevation
        comparison <- comparison %>%
          mutate(
            min_dev_5 = abs(min_elevation - min_perc_5),
            min_dev_10 = abs(min_elevation - min_perc_10),
            min_dev_15 = abs(min_elevation - min_perc_15),
            min_dev_20 = abs(min_elevation - min_perc_20),
            min_dev_25 = abs(min_elevation - min_perc_25),
            max_dev_55 = abs(max_elevation - max_perc_55),
            max_dev_60 = abs(max_elevation - max_perc_60),
            max_dev_65 = abs(max_elevation - max_perc_65),
            max_dev_70 = abs(max_elevation - max_perc_70),
            max_dev_75 = abs(max_elevation - max_perc_75),
            max_dev_95 = abs(max_elevation - max_perc_95)
          )
        
        # Calculate the mean deviation for each quantile for the entire mountain range
        mean_deviation <- comparison %>%
          summarise(
            min_mean_dev_5 = mean(min_dev_5, na.rm = TRUE),
            min_mean_dev_10 = mean(min_dev_10, na.rm = TRUE),
            min_mean_dev_15 = mean(min_dev_15, na.rm = TRUE),
            min_mean_dev_20 = mean(min_dev_20, na.rm = TRUE),
            min_mean_dev_25 = mean(min_dev_25, na.rm = TRUE),
            max_mean_dev_55 = mean(max_dev_55, na.rm = TRUE),
            max_mean_dev_60 = mean(max_dev_60, na.rm = TRUE),
            max_mean_dev_65 = mean(max_dev_65, na.rm = TRUE),
            max_mean_dev_70 = mean(max_dev_70, na.rm = TRUE),
            max_mean_dev_75 = mean(max_dev_75, na.rm = TRUE),
            max_mean_dev_95 = mean(max_dev_95, na.rm = TRUE)
          )
        
        # Identfiy the min and max quantile which has the least average deviation
        # Min
        min_quantile <- names(which.min(select(mean_deviation, starts_with("min_mean_dev_"))))
        min_quantile <- gsub("min_mean_dev_", "", min_quantile)
        
        # Max
        max_quantile <- names(which.min(select(mean_deviation, starts_with("max_mean_dev_"))))
        max_quantile <- gsub("max_mean_dev_", "", max_quantile)
        
        # Calculate the best quantiles based on available data or use default ones
        if(exists("min_quantile") && exists("max_quantile")) {
          # If min_quantile and max_quantile are determined from the validation data, use them
          min_elev_value <- quantile(elevations, as.numeric(min_quantile) / 100, na.rm = TRUE)
          max_elev_value <- quantile(elevations, as.numeric(max_quantile) / 100, na.rm = TRUE)
        } else {
          # If not determined (no validation data), use the default quantiles
          min_elev_value <- quantile(elevations, as.numeric(default_min_quantile) / 100, na.rm = TRUE)
          max_elev_value <- quantile(elevations, as.numeric(default_max_quantile) / 100, na.rm = TRUE)
        }
        
        print(paste("For",GMBA_range,"The min quantile wih the least mean deviation is:", min_quantile))
        print(paste("For",GMBA_range,"The max quantile wih the least mean deviation is:", max_quantile))
      }
      #---------------------------------------------------------------------#
      # 3.2 Use the quantile that comes closest to the valid data --
      #---------------------------------------------------------------------#
      
      # Use this min and max quantile for the DEM extraction
      
      #Filter the species in that mountain range
      Checklist_Elev_DEM_Loop <- data.table::data.table(Checklist_Elev_DEM)[Mountain_range == GMBA_range] %>% 
        sf::st_as_sf()
      
      
      # Extract elevations for all species in Checklist_Elev_DEM based on the best quantiles
      all_species_elevations_list <- list()
      
      # Iterate over all species in Checklist_Elev_DEM
      for (i in seq_along(Checklist_Elev_DEM_Loop$sciname)) {
        
        tryCatch({
          
          species <- filter(Checklist_Elev_DEM_Loop, sciname == Checklist_Elev_DEM_Loop$sciname[i])
          
          # Intersection of the species data with the mountain range
          cropped_species <- st_intersection(species, mountain_range)
          
          # If there's no data within the bbox skip 
          if (nrow(cropped_species) == 0) {
            next
          }
          
          # Get the elevation data DEM for the cropped region
          elevations_raster <- elevatr::get_elev_raster(cropped_species,
                                                        source = "gl3",
                                                        clip = "location",
                                                        z = 5,
                                                        neg_to_na = TRUE,
                                                        override_size_check = TRUE)
          
          # Convert raster to a vector
          elevations <- as.vector(elevations_raster)
          
          # Calculate the best quantiles
          min_elev_value <- quantile(elevations, as.numeric(min_quantile) / 100, na.rm = TRUE)
          max_elev_value <- quantile(elevations, as.numeric(max_quantile) / 100, na.rm = TRUE)
          
          # Create a data frame with the results
          species_df <- tibble(
            sciname = Checklist_Elev_DEM_Loop$sciname[i], 
            Mountain_range = GMBA_range,
            min_elev_DEM = round(min_elev_value),
            max_elev_DEM = round(max_elev_value)
          )
          
          # Store species_df into the list
          all_species_elevations_list[[i]] <- species_df
          
        }, error = function(e) {
          message(paste("Error processing species:", Checklist_Elev_DEM_Loop$sciname[i], "; Error:", e$message))
        })
        
      }
      
      # At the end of processing each range, store results in all_ranges_elevations_list
      all_ranges_elevations_list[[GMBA_range]] <- bind_rows(all_species_elevations_list)
    }
    
    # Combine all mountain range data frames into one data frame for the current system
    all_ranges_elevations_DF <- bind_rows(all_ranges_elevations_list)
    
    # Store the combined result in all_systems_results_list
    all_systems_results_list[[GMBA_system]] <- all_ranges_elevations_DF
  }
  
  # Combine all system results into one data frame
  all_systems_results_DF <- bind_rows(all_systems_results_list, .id = "Mountain_system")
  
  return(all_systems_results_DF)
  
}
