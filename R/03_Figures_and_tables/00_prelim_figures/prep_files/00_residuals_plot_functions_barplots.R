# Function to generate and save bar plots for each mountain range
generate_bar_plots <- function(data, filter_conditions) {
  for (filter_condition in filter_conditions) {
    # filter data for the different alpine categories
    filtered_data <- data |>
      filter(filter_condition == !!filter_condition)
    
    #  through each unique mountain range
    for (mountain_range in unique(filtered_data$Mountain_range)) {
      # Filter data for mountain range
      test <- filtered_data |>
        filter(Mountain_range == mountain_range)
      
      #  the validated count for the current mountain range
      validated_count <- unique(test$validated_count)
      
      # asterisks for validated mountain ranges
      asterisks <- paste(rep("*", validated_count), collapse = "")
      
      #  group order
      test$group <- factor(test$group, levels = c("birds", "mammals", "reptiles"))
      
      # Create the bar plot 
      bar_plot <- ggplot(test, aes(x = group, y = residuals_log, fill = residuals_log)) +
        geom_bar(stat = "identity", width = 0.8) +  # bar spacing
        geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +  # Adds dashed line at y=0
        scale_y_continuous(limits = c(-1.55, 1.55)) +  # Set y-axis limits
        scale_fill_gradientn(colors = colors, limits = c(-1.55, 1.55)) +  
        labs(title = paste(unique(test$Mountain_range),asterisks),
             x = NULL, y = NULL,
             fill = NULL) +
        theme(
          axis.text.x = element_blank(),  
          axis.text.y = element_text(size = 6),  
          axis.title.x = element_blank(),        
          axis.title.y = element_blank(),        
          axis.ticks = element_line(size = 0.2),  
          axis.ticks.x = element_blank(),
          axis.line = element_line(size = 0.2),
          legend.position = "none",
          plot.title = element_text(hjust = 0.1, size = 5),
          panel.background = element_rect(fill = "transparent", color = NA), #  panel background transparent
          plot.background = element_rect(fill = "transparent", color = NA)   
        )
      
      # 
      safe_filename <- gsub("[/ ]", "_", mountain_range)  # some mountainrange name have long names --> replace slashes and spaces with underscores
      
      #  
      filename <- paste0("~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Bar_graphs/", filter_condition, "/residuals_", safe_filename, ".svg")
      
      # Open the SVG device with specified settings
      svg(filename, width = 0.65, height = 0.8,bg = "transparent")
      
      # save as svg
      print(bar_plot)
      
      # 
      dev.off()
    }
  }
}
