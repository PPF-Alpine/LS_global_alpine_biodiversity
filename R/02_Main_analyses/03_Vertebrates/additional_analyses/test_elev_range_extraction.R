
#----------------------------------------------------------#
# Initial Code:  creates global elevational ranges -----
#----------------------------------------------------------#

# This code actually creates global elevation ranges
# has to be done for each mountain range seperately 
# See below.. 

# Get unique species
unique_species <- unique(mammals$sciname)

# Empty dataframe to store the results
results_list <- list()

for(i in seq_along(unique_species)) {
  
  # Use tryCatch to handle potential errors
  tryCatch({
    
    species <- mammals %>% filter(sciname == unique_species[i])
    
    # Get the elevation data DEM
    elevations_raster <- elevatr::get_elev_raster(species,
                                                  source="gl3",
                                                  clip="bbox",
                                                  z=5,
                                                  neg_to_na=TRUE,
                                                  override_size_check = TRUE)
    
    # Convert raster to a vector
    elevations <- as.vector(elevations_raster)
    
    min_elev <- min(elevations, na.rm = TRUE)
    max_elev <- max(elevations, na.rm = TRUE)
    mean_elev <- mean(elevations, na.rm = TRUE)
    median_elev <- median(elevations, na.rm = TRUE)
    perc_95 <- quantile(elevations, 0.95, na.rm = TRUE) # 95% of the extracted elevations is below the perc_90 value 
    perc_5 <- quantile(elevations, 0.05, na.rm = TRUE) # 95% of the extracted elevations is above the perc_10 value 
    
    # Create a data frame for this species
    species_df <- tibble::tibble(sciname = unique_species[i], min_elev, max_elev, mean_elev, median_elev, perc_95, perc_5)
    
    # Store species_df into the list
    results_list[[i]] <- species_df
    
  }, error = function(e) {
    message(paste("Error processing species:", unique_species[i], "; Error:", e$message))
  })
  
}

# Bind all dataframes together
elevations_MDD <- dplyr::bind_rows(results_list)


# senstitivity of percentiles --> needs testing!! 

##### mountain BBOX
mountain_bboxes <- lapply(1:nrow(mountain_shapes), function(i) {
  bbox_coords <- st_bbox(mountain_shapes[i, ])
  bbox_poly <- st_polygon(list(cbind(
    c(bbox_coords["xmin"], bbox_coords["xmin"], bbox_coords["xmax"], bbox_coords["xmax"], bbox_coords["xmin"]),
    c(bbox_coords["ymin"], bbox_coords["ymax"], bbox_coords["ymax"], bbox_coords["ymin"], bbox_coords["ymin"])
  )))
  
  list(
    GMBA_ID = mountain_shapes$GMBA_V2_ID[i],
    MapName = mountain_shapes$MapName[i],
    geometry = bbox_poly
  )
})

# Convert the list of bounding box polygons and associated attributes to a single sf object
mountain_bboxes_sf <- do.call(rbind, lapply(mountain_bboxes, function(x) {
  st_sf(GMBA_ID = x$GMBA_ID, MapName = x$MapName, geometry = st_sfc(x$geometry), crs=st_crs(mountain_shapes))
}))




##### This needs more work but could reduce computation time  ###############


mammals <- mammals %>% 
  filter(Mountain_system == "Central Asia") %>%
  distinct(sciname, .keep_all = TRUE)

unique_species <- unique(mammals$sciname)

Himalaya <- mountain_shapes %>% filter(MapName == "Himalaya") 


# Function to process a chunk of species
process_species_chunk <- function(chunk_species, mountain_shapes) {
  # Lists to store results and elevations
  results_list <- list()
  elevations_list <- list()
  
  for(i in seq_len(nrow(chunk_species))) {
    species_name <- chunk_species$sciname[i]
    
    # Use tryCatch to handle potential errors
    tryCatch({
      species <- chunk_species[i, ]
      
      # Crop to the mountain
      cropped_species <- sf::st_crop(species, mountain_shapes)
      
      # If there's no data within the bbox, skip 
      if (nrow(cropped_species) == 0) {
        next
      }
      
      # Get the elevation data DEM for the cropped region
      elevations_raster <- elevatr::get_elev_raster(cropped_species,
                                                    source="gl3",
                                                    clip="bbox",
                                                    z=5,
                                                    neg_to_na=TRUE,
                                                    override_size_check = TRUE)
      
      # Convert raster to a vector
      elevations <- as.vector(elevations_raster)
      
      # Store elevations for KDE plotting
      elevations_list[[unique_species[i]]] <- elevations
      
      # this is the absolute min and max 
      min_elev <- min(elevations, na.rm = TRUE)# can be removed.. 
      max_elev <- max(elevations, na.rm = TRUE)
      # Calculate the 95/5 quartile
      perc_95 <- quantile(elevations, 0.95, na.rm = TRUE)
      perc_5 <- quantile(elevations, 0.05, na.rm = TRUE)
      # Calculate mean and standard deviation
      mean_elev <- mean(elevations, na.rm = TRUE)
      sd_elev <- sd(elevations, na.rm = TRUE)
      #use mean and 2 SD (95% of the data)
      lower_bound_2sd <- mean_elev - 2 * sd_elev
      upper_bound_2sd <- mean_elev + 2 * sd_elev
      # Calculate 75th and 25th percentiles
      perc_75 <- quantile(elevations, 0.75, na.rm = TRUE)
      perc_25 <- quantile(elevations, 0.25, na.rm = TRUE)
      
      # Create a data frame for this species
      species_df <- tibble::tibble(
        sciname = species_name,  # <-- This is the correct assignment
        perc_95,
        perc_5,
        perc_75, 
        perc_25, 
        mean_elev, 
        sd_elev, 
        lower_bound_2sd,
        upper_bound_2sd
      )
      
      # Store species_df into the list
      results_list[[species_name]] <- species_df
      
    }, error = function(e) {
      message(paste("Error processing species:", species_name, "; Error:", e$message))
    })
  }
  
  list(
    results = results_list,
    elevations = elevations_list
  )
}

# Split the mammals dataset into chunks
# Splitting the dataset for each species can be thought of as chunking by species
chunks <- split(mammals, mammals$sciname)

# Process each chunk (each species)
chunk_results <- lapply(chunks, process_species_chunk, mountain_shapes = Himalaya)

# Consolidate the results
results_list <- do.call(c, lapply(chunk_results, `[[`, "results"))
elevations_list <- do.call(c, lapply(chunk_results, `[[`, "elevations"))
elevations_MDD <- dplyr::bind_rows(results_list)


# Function to plot KDE for a given species
plot_species_kde <- function(species_name, elev_list) {
  if (!is.null(elev_list[[species_name]])) {
    elevations <- elev_list[[species_name]]
    elev_kde <- density(elevations, na.rm = TRUE)
    
    plot(elev_kde, main = paste("Elevation Distribution for", species_name), 
         xlab = "Elevation", ylab = "Density")
  } else {
    cat("No elevation data found for", species_name)
  }
}

plot_species_kde("Ailurus fulgens", elevations_list)


