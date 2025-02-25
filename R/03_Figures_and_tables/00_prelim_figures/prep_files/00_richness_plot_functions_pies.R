# Function to generate and save pie charts for each mountain range
generate_pie_charts <- function(data, filter_condition) {
  # Filter data based on the provided filter condition
  filtered_data <- data |>
    filter(filter_condition == !!filter_condition)
  
  # Loop through each  mountain range
  for (mountain_range in unique(filtered_data$Mountain_range)) {
    # 
    test <- filtered_data |>
      filter(Mountain_range == mountain_range)
    
    # 
    pie_chart <- ggplot(test, aes(x = "", y = richness_group, fill = richness_group_log)) +
      geom_bar(stat = "identity", width = 1) + 
      coord_polar(theta = "y") +
      geom_text(aes(label = group, y = richness_group), position = position_stack(vjust = 0.6), 
                color = "white", size = 2, angle = 0, hjust = 0.8) +
      theme_void() +
      scale_fill_gradientn(colors = colors, limits = c(0, 6)) +
      labs(title = paste(mountain_range),
           x = NULL, y = NULL,
           fill = NULL) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.1, size = 6))
    
    # 
    safe_filename <- gsub("[/ ]", "_", mountain_range)  # Replace slashes and spaces with underscores
    
    # 
    filename <- paste0("~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Pie_Charts/col_richness/", filter_condition, "/species_richness_", safe_filename, ".svg")
    
    # 
    svg(filename, width = 0.8, height = 0.8, bg = "transparent")
    
    # 
    print(pie_chart)
    
    # 
    dev.off()
  }
}

############################################################## mutate asterisks ###########################

library(rlang)


#  run and save pie charts for each mountain range
generate_pie_charts <- function(data, filter_condition) {
  # Filter data based on the provided filter condition
  filtered_data <- data |>
    filter(filter_condition == !!filter_condition)
  
  # 
  for (mountain_range in unique(filtered_data$Mountain_range)) {
    # 
    test <- filtered_data |>
      filter(Mountain_range == mountain_range)
    
    # get validated mountain regions
    validated_count <- unique(test$validated_count)
    # number of asterisks
    asterisks <- paste(rep("*", validated_count), collapse = "")
    
    #
    pie_chart <- ggplot(test, aes(x = "", y = richness_group, fill = richness_group_log)) +
      geom_bar(stat = "identity", width = 1) + 
      coord_polar(theta = "y") +
      geom_text(aes(label = group, y = richness_group), position = position_stack(vjust = 0.6), 
                color = "white", size = 2, angle = 0, hjust = 0.8) +
      theme_void() +
      scale_fill_gradientn(colors = colors, limits = c(0, 6)) +
      labs(title = paste(mountain_range, asterisks),
           x = NULL, y = NULL,
           fill = NULL) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.1, size = 6))
    
    #
    safe_filename <- gsub("[/ ]", "_", mountain_range) 
    
    # Define the filename with path
    filename <- paste0("~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Pie_Charts/col_richness/", filter_condition, "/species_richness_", safe_filename, ".svg")
    
    # save as svg
    svg(filename, width = 0.8, height = 0.8, bg = "transparent")
    
    # 
    print(pie_chart)
    
    # 
    dev.off()
  }
}


