
# This function first builds a bounding box around a species polygon 
# it  then overlaps bounding boxes species and mountains to identify 
# For the overlapping bounding boxes it calculates overlap percentage of the actual shapes
# Helper function to calculate overlap percentage
calculate_overlap_percentage <- function(intersection, species) {
  if (nrow(intersection) > 0) {
    intersection_area <- as.numeric(sf::st_area(intersection)) / 10^6
    species_area <- as.numeric(sf::st_area(species)) / 10^6
    return(round((intersection_area / species_area) * 100, 4))
  } else {
    return(0)
  }
}

## use the actual geometry: 

calculate_overlaps <- function(species, mountain_shapes, alpine_biome, mountain_bboxes_sf) {
  sciname <- species$sciname
  message("Processing species: ", sciname)
  
  species_results <- list()
  
  # Find Overlapping Mountain Ranges
  overlapping_indices <- which(sapply(sf::st_intersects(mountain_bboxes_sf, species), length) > 0)
  
  # Check if there are no overlapping mountain ranges
  if (length(overlapping_indices) == 0) {
    message("No overlapping mountain range for ", sciname)
    # Return an empty list to indicate no overlap but no error
    return(list(results = NULL, error = FALSE))
  }
  
  tryCatch({
    for (j in overlapping_indices) {
      mountain_range <- mountain_shapes[j, ]
      alpine <- alpine_biome[alpine_biome$Mountain_range == mountain_range$Mountain_range, ]
      
      # Calculate overlaps
      message(sprintf("Calculating overlap for %s with mountain range %s", sciname, mountain_range$Mountain_range))
      mountain_intersection <- sf::st_intersection(species, mountain_range)
      overlap_percentage_mountain <- calculate_overlap_percentage(mountain_intersection, species)
      
      message(sprintf("Calculating overlap for %s with alpine biome of mountain range %s", sciname, mountain_range$Mountain_range))
      alpine_intersection <- sf::st_intersection(species, alpine)
      overlap_percentage_alpine <- calculate_overlap_percentage(alpine_intersection, species)
      
      # Store results
      result <- data.frame(
        sciname = sciname,
        Mountain_range = mountain_range$Mountain_range,
        overlap_percentage_mountain = overlap_percentage_mountain,
        overlap_percentage_alpine = overlap_percentage_alpine,
        species_area = round(as.numeric(sf::st_area(species)) / 10^6, 2)
      )
      species_results[[length(species_results) + 1]] <- result
    }
    # Return processing results and indicate no error
    return(list(results = do.call(rbind, species_results), error = FALSE))
  }, error = function(e) {
    message("Error processing species: ", sciname, ". Error: ", e$message)
    # Return NULL in results to signal an error
    return(list(results = NULL, error = TRUE))
  })
}
