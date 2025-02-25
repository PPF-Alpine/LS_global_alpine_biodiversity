# This is the main Function to be applied in the workflow to overlap species shapes, mountain shapes and alpine biome shapes and get percentage of overlap

# This function is a helper function used in the workflow to intersect distribution ranges with alpine biomes

order_by_mountain_range <- function(mountain_shapes, alpine_biome) {
  ordered_mountain_shapes <- mountain_shapes[order(mountain_shapes$Mountain_range), ]
  ordered_alpine_biome <- alpine_biome[order(alpine_biome$Mountain_range), ]
  return(list(mountain_shapes = ordered_mountain_shapes, alpine_biome = ordered_alpine_biome))
}

# this function is a helper function in the workflow to intersect distribution and mountain ranges
# It is used to build bounding boxes around mountain polygons
create_bounding_boxes <- function(mountain_shapes) {
  message("Building bounding boxes around mountains...")
  
  mountain_bboxes <- lapply(1:nrow(mountain_shapes), function(i) {
    bbox_coords <- sf::st_bbox(mountain_shapes[i, ])
    sf::st_polygon(list(cbind(
      c(bbox_coords["xmin"], bbox_coords["xmin"], bbox_coords["xmax"], bbox_coords["xmax"], bbox_coords["xmin"]),
      c(bbox_coords["ymin"], bbox_coords["ymax"], bbox_coords["ymax"], bbox_coords["ymin"], bbox_coords["ymin"])
    )))
  })
  
  return(sf::st_sf(geometry = sf::st_sfc(mountain_bboxes), crs = sf::st_crs(mountain_shapes)))
}


# validate shapes ✅
validate_shapes <- function(species_shapes) {
  species_shapes <- sf::st_as_sf(species_shapes)
  message("Checking for invalid shapes...")
  invalids <- which(!sf::st_is_valid(species_shapes))
  
  if (length(invalids) > 0) {
    message("Invalid shapes found. Making invalid species valid.")
    species_shapes[invalids, ] <- sf::st_make_valid(species_shapes[invalids, ])
  } else {
    message("No invalid shapes found.")
  }
  
  return(species_shapes)
}

overlap_mountains_and_alpinebiome <- function(species_shapes, mountain_shapes, alpine_biome) {
  tryCatch({
    ordered_data <- order_by_mountain_range(mountain_shapes, alpine_biome)
  }, error = function(e) {
    stop("Error in order_by_mountain_range: ", e$message)
  })
  
  tryCatch({
    valid_species_shapes <- validate_shapes(species_shapes)
  }, error = function(e) {
    stop("Error in validate_shapes: ", e$message)
  })
  
  tryCatch({
    mountain_bboxes_sf <- create_bounding_boxes(ordered_data$mountain_shapes)
  }, error = function(e) {
    stop("Error in create_bounding_boxes: ", e$message)
  })
  
  total_species <- nrow(valid_species_shapes)
  processed_results <- list()
  not_processed_species <- list()
  
  for (i in seq_len(total_species)) {
    species <- valid_species_shapes[i, ]
    
    processing_result <- calculate_overlaps(species, ordered_data$mountain_shapes, ordered_data$alpine_biome, mountain_bboxes_sf)
    
    if (!is.null(processing_result$results) && !processing_result$error) {
      processed_results[[length(processed_results) + 1]] <- processing_result$results
    } else if (processing_result$error) {
      # Add to not_processed_species only if an actual error occurred
      not_processed_species[[length(not_processed_species) + 1]] <- species$sciname
    }
    
    message(sprintf("Yay! %d of %d species are done", i, total_species))
  }
  
  processed_df <- do.call(rbind, processed_results)
  not_processed_df <- data.frame(sciname = unlist(not_processed_species))
  
  return(list(processed = processed_df, not_processed = not_processed_df))
}

