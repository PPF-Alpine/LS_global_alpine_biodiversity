
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

library(dplyr)
library(sf)
library(purrr)
library(data.table)
library(elevatr)

#---------------------------#
# Function to filter species #
#---------------------------#
filter_species_in_range <- function(Checklist_Elev_DEM, GMBA_range) {
  Checklist_Elev_DEM |>
    filter(!is.na(min_elevation) & !is.na(max_elevation)) |>
    group_by(sciname) |>
    mutate(num_systems = n_distinct(Mountain_system)) |>
    filter(num_systems == 1) |>
    select(-num_systems) |>
    data.table::data.table() |>
    .[Mountain_range == GMBA_range, .SD[1], by = sciname] |>
    sf::st_as_sf()
}

#------------------------------------#
# Function to extract elevation data #
#------------------------------------#
extract_elevations <- function(species, mountain_range) {
  tryCatch({
    species <- sf::st_as_sf(species)
    if (!sf::st_is_valid(species)) {
      species <- sf::st_make_valid(species)
    }
    
    cropped_species <- st_intersection(species, mountain_range)
    
    if (nrow(cropped_species) == 0) {
      return(NULL)
    }
    
    elevations_raster <- elevatr::get_elev_raster(
      cropped_species, source = "gl3", clip = "location",
      z = 5, neg_to_na = TRUE, override_size_check = TRUE
    )
    
    as.vector(elevations_raster)
  }, error = function(e) {
    message(paste("Error processing species:", unique(species$sciname), "; Error:", e$message))
    return(NULL)
  })
}

#-------------------------------------------------#
# Function to calculate quantiles for elevation  #
#-------------------------------------------------#
calculate_elevation_quantiles <- function(elevations) {
  quantiles <- quantile(elevations, c(0.05, 0.10, 0.15, 0.20, 0.25, 0.55, 0.60, 0.65, 0.70, 0.75, 0.95), na.rm = TRUE)
  round(quantiles)
}

#------------------------------------------------------------#
# Function to calculate the best quantiles based on deviance #
#------------------------------------------------------------#
calculate_best_quantiles <- function(comparison) {
  mean_deviation <- comparison |>
    summarise(across(starts_with("min_dev_"), ~ mean(.x, na.rm = TRUE), .names = "min_mean_dev_{.col}"),
              across(starts_with("max_dev_"), ~ mean(.x, na.rm = TRUE), .names = "max_mean_dev_{.col}"))
  
  min_quantile <- names(which.min(select(mean_deviation, starts_with("min_mean_dev_"))))
  max_quantile <- names(which.min(select(mean_deviation, starts_with("max_mean_dev_"))))
  
  list(
    min = gsub("min_mean_dev_", "", min_quantile),
    max = gsub("max_mean_dev_", "", max_quantile)
  )
}

#----------------------------#
# Main Elevation Extraction #
#----------------------------#
extract_elevational_ranges <- function(Checklist_Elev_DEM, Focus_GMBA_systems) {
  all_systems_results_list <- list()
  
  for (GMBA_system in Focus_GMBA_systems) {
    mountain_system <- mountain_shapes |> filter(Mountain_system == GMBA_system)
    GMBA_ranges <- unique(mountain_system$Mountain_range)
    
    all_ranges_elevations_list <- list()
    
    for (GMBA_range in GMBA_ranges) {
      mountain_range <- mountain_shapes |> filter(Mountain_range == GMBA_range)
      filtered_data <- filter_species_in_range(Checklist_Elev_DEM, GMBA_range)
      
      if (nrow(filtered_data) == 0) {
        message(paste("No species data available for mountain range:", GMBA_range, "Using default quantiles"))
        min_quantile <- "25"
        max_quantile <- "95"
      } else {
        unique_species <- unique(filtered_data$sciname)
        results_list <- list()
        
        for (i in seq_along(unique_species)) {
          elevations <- extract_elevations(filter(filtered_data, sciname == unique_species[i]), mountain_range)
          if (is.null(elevations)) next
          
          quantiles <- calculate_elevation_quantiles(elevations)
          
          species_df <- tibble(
            sciname = unique_species[i],
            min_elev = min(elevations, na.rm = TRUE),
            min_perc_5 = quantiles["5%"],
            min_perc_10 = quantiles["10%"],
            min_perc_15 = quantiles["15%"],
            min_perc_20 = quantiles["20%"],
            min_perc_25 = quantiles["25%"],
            max_perc_55 = quantiles["55%"],
            max_perc_60 = quantiles["60%"],
            max_perc_65 = quantiles["65%"],
            max_perc_70 = quantiles["70%"],
            max_perc_75 = quantiles["75%"],
            max_perc_95 = quantiles["95%"],
            max_elev = max(elevations, na.rm = TRUE)
          )
          
          results_list[[i]] <- species_df
        }
        
        elevations_MDD <- bind_rows(results_list)
        comparison <- filtered_data |>
          select(sciname, min_elevation, max_elevation, Mountain_range, Mountain_system) |>
          left_join(elevations_MDD, by = "sciname") |>
          sf::st_set_geometry(NULL)
        
        comparison <- comparison |>
          mutate(across(starts_with("min_perc_"), ~ abs(min_elevation - .x), .names = "min_dev_{.col}"),
                 across(starts_with("max_perc_"), ~ abs(max_elevation - .x), .names = "max_dev_{.col}"))
        
        best_quantiles <- calculate_best_quantiles(comparison)
        
        min_quantile <- best_quantiles$min
        max_quantile <- best_quantiles$max
      }
      
      Checklist_Elev_DEM_Loop <- Checklist_Elev_DEM |> filter(Mountain_range == GMBA_range)
      all_species_elevations_list <- list()
      
      for (i in seq_along(Checklist_Elev_DEM_Loop$sciname)) {
        elevations <- extract_elevations(filter(Checklist_Elev_DEM_Loop, sciname == Checklist_Elev_DEM_Loop$sciname[i]), mountain_range)
        if (is.null(elevations)) next
        
        min_elev_value <- quantile(elevations, as.numeric(min_quantile) / 100, na.rm = TRUE)
        max_elev_value <- quantile(elevations, as.numeric(max_quantile) / 100, na.rm = TRUE)
        
        species_df <- tibble(
          sciname = Checklist_Elev_DEM_Loop$sciname[i],
          Mountain_range = GMBA_range,
          min_elev_DEM = round(min_elev_value),
          max_elev_DEM = round(max_elev_value)
        )
        
        all_species_elevations_list[[i]] <- species_df
      }
      
      all_ranges_elevations_list[[GMBA_range]] <- bind_rows(all_species_elevations_list)
    }
    
    all_ranges_elevations_DF <- bind_rows(all_ranges_elevations_list)
    all_systems_results_list[[GMBA_system]] <- all_ranges_elevations_DF
  }
  
  all_systems_results_DF <- bind_rows(all_systems_results_list, .id = "Mountain_system")
  return(all_systems_results_DF)
}
