

# Create the pie chart with correct labels
pie_chart <- ggplot(test, aes(x = "", y = n_group, fill = n_group_log)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  geom_text(aes(label = group, y = n_group ), position = position_stack(vjust = 0.6), 
            color = "white", size = 4, angle = 0, hjust = 0.8) +
  theme_void()+
  scale_fill_gradientn(colors = colors, limits = c(0, 6)) +
  labs(title = paste("Species Richness", unique(test$Mountain_range)),
       x = NULL, y = NULL,
       fill = NULL) 

# Print the pie chart
print(pie_chart)


#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gridExtra)
library(svglite)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

# Run the test Preparation file 
# this script reformats the mammal test
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/00_Biodiversity_data_preparation.R")
)

# load the table with the area size of each mountain range
area_size <- readxl::read_excel(paste0(data_storage_path, "Mountains/Suzette_Alpine_biome/Alpine_Biome_Area_size.xlsx")) |>
  select(Mountain_range,area_size)|>
  mutate(log_area=log1p(area_size))


#----------------------------------------------------------#
# Calculate richness for each mountain range  -----
#----------------------------------------------------------#
checklist_selected <- checklist_combined|>
  filter(Mountain_system!="East Siberian Mountains",
         Mountain_system!="Central and Northern Siberian Mountains",
         Mountain_system!="Svalbard",
         Mountain_system!="North America Arctic Islands",
         Mountain_system!="North America Plains",
         Mountain_range!="Alaska-Yukon Ranges")

# calculate the total richness per mountain range
total_richness_per_mountain <- checklist_selected |>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_4_degree)|>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_treeline)|>
  filter(max_elevation_USE >= Mean_elevation_treeline) |>
  group_by(Mountain_range) |>
  summarise(total_richness = n_distinct(sciname), .groups = "drop")

#  number of species per group for each mountain range
species_per_group <- checklist_selected |>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_4_degree)|>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_treeline)|>
  filter(max_elevation_USE >= Mean_elevation_treeline) |>
  group_by(Mountain_range, group) |>
  summarise(n_group = n_distinct(sciname), .groups = "drop")

# calculate the proportion of each group in overall richness
species_richness_combined <- species_per_group |>
  left_join(total_richness_per_mountain, by = "Mountain_range") |>
  mutate(proportion = round((n_group / total_richness)*100),0) |>
  mutate(n_group_log = log1p(n_group))|>
  left_join(area_size, by = "Mountain_range") |>
  select(Mountain_range, group, n_group, total_richness, proportion, everything()) |>
  drop_na()


#-------------------
# linear model for all groups
#-------------------------------

# Function to fit model, predict richness, and calculate residuals for a specific group
sar_for_groups <- function(data, group_name) {
  # Filter data for the group
  test_group <- filter(data, group == group_name)
  
  # Fit the linear model
  lm_mod <- lm(log1p(n_group) ~ log1p(area_size), data = test_group, na.action = na.exclude)
  
  # Calculate predicted richness and transform back
  test_group$predicted_richness <- exp(predict(lm_mod, newdata = test_group))
  
  
  # Calculate residuals
  test_group$residuals <- insight::get_residuals(lm_mod, type = "response")
  
  # Log the residuals and richness
  test_group <- test_group |>
    mutate(residuals_log = sign(residuals) * log1p(abs(residuals)),
           richness_log = log1p(total_richness))
  
  # Return the modified group test
  test_group
}

# List of groups
groups <- unique(species_richness_combined$group)

# Apply the function to each group and bind the results into a single testframe
species_richness_modified <- map_df(groups, ~sar_for_groups(species_richness_combined, .x))

###########################
# Do bar graphs for the residuals 

test <- species_richness_modified |> filter(Mountain_range=="Northern Andes")


# Define custom color gradient
colors <- c("#1A2A5A", 
            "#256C85", 
            "#37AF91", 
            "#44BF2F", 
            "#A3970E", 
            "#F2C12E",
            "#F2994A", 
            "#D8461B", 
            "#AD2E24", 
            "#4E1703")



# Create the plot
test$group <- factor(test$group, levels = c("Birds", "Mammals", "Reptiles"))

# Create the plot
circular_barplot <- ggplot(test, aes(x = group, y = residuals_log, fill = residuals_log)) +
  geom_bar(stat = "identity", width = 0.8) +  # Increased width for less spacing
  scale_y_continuous(limits = c(-2, 2)) +  # Set y-axis limits
  scale_fill_gradientn(colors = colors, limits = c(-2, 2)) +  # Adjusted color scale limits
  labs(title = paste(unique(test$Mountain_range)),
       x = NULL, y = NULL,
       fill = NULL) 

# test Lollipop plots 
circular_barplot <- ggplot(test, aes(x = group, y = residuals_log, color = residuals_log)) +
  geom_segment(aes(x = group, xend = group, y = 0, yend = residuals_log), size = 3) +  # Add segments
  geom_point(size = 10) +  # Add points
  scale_y_continuous(limits = c(-1, 1)) +  # Set y-axis limits
  scale_color_gradientn(colors = colors, limits = c(-1,1)) +  # Adjusted color scale for points and segments
  labs(title = paste(unique(test$Mountain_range)),
       x = NULL, y = NULL,
       color = NULL) +  # Adjust labels
  theme_minimal()  # Optional: Adds a minimal theme for aesthetics
x11()
plot(circular_barplot)

#--------------------------------------------------------
# Loop through all geo entities and save as one pdf
#-----------------------------------------------------------

test$group <- factor(test$group, levels = c("Birds", "Mammals", "Reptiles"))

# Initialize an empty list to store plots
plot_list <- list()

# Loop through each unique mountain range
for (mountain_range in unique(species_richness_modified$Mountain_range)) {
  # Filter data for the current mountain range
  test <- species_richness_modified |> 
    filter(Mountain_range == mountain_range)
  
  # 
  bar_plot <- ggplot(test, aes(x = group, y = residuals_log, fill = residuals_log)) +
    geom_bar(stat = "identity", width = 0.8) +  # Increased width for less spacing
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +  # Adds a thin dashed line at y=0
    scale_y_continuous(limits = c(-1.1, 1.4)) +  # Set y-axis limits
    scale_fill_gradientn(colors = colors, limits = c(-1.1, 1.4)) +  # Adjusted color scale limits
    labs(title = paste(unique(test$Mountain_range)),
         x = NULL, y = NULL,
         fill = NULL) +
    theme(
      axis.text.x = element_text(size = 6),  # Smaller x-axis labels
      axis.text.y = element_text(size = 6),  # Smaller y-axis labels
      axis.title.x = element_blank(),        # Remove x-axis title
      axis.title.y = element_blank(),        # Remove y-axis title
      axis.ticks = element_line(size = 0.25),  # Thinner axis ticks
      legend.position = "none",
      plot.title = element_text(hjust = 0.1, size = 6)) 
  
  # Add the plot to the list
  plot_list[[mountain_range]] <- bar_plot
}


# 
pdf("~/Desktop/Datasets/output_shared/Vertebrates/bar_plots/4_degree.pdf",width=14,height=8)
marrangeGrob(grobs = plot_list, nrow = 2, ncol = 5, pages = ceiling(length(plot_list) / 10)) 
dev.off()


###################
# Save as single svgs:

# Assuming combined_results is your dataset
# Loop through each unique geo_entity and save plots as individual SVG files
for (mountain_range in unique(species_richness_modified$Mountain_range)) {
  # Filter data for the current mountain range
  test <- species_richness_modified |> 
    filter(Mountain_range == mountain_range)
  
  # Factor the 'scenario' with a specific order
  test$group <- factor(test$group, levels = c("Birds", "Mammals", "Reptiles"))
  
  # Create the plot
  bar_plot <- ggplot(test, aes(x = group, y = residuals_log, fill = residuals_log)) +
    geom_bar(stat = "identity", width = 0.6) +  # Increased width for less spacing
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +  # Adds a thin dashed line at y=0
    scale_y_continuous(limits = c(-1.3, 1.1)) +  # Set y-axis limits
    scale_fill_gradientn(colors = colors, limits = c(-1.3, 1.1)) +  # Adjusted color scale limits
    labs(title = paste(unique(test$Mountain_range)),
         x = NULL, y = NULL,
         fill = NULL) +
    theme(
      axis.text.x = element_blank(),  # Smaller x-axis labels
      axis.text.y = element_blank(),  # Smaller y-axis labels
      axis.title.x = element_blank(),        # Remove x-axis title
      axis.title.y = element_blank(),        # Remove y-axis title
      axis.ticks = element_blank(),
      axis.line.x = element_line(size=0.2),# Thinner axis ticks
      axis.line.y = element_line(size=0.2),# Thinner axis ticks
      panel.background = element_rect(fill = "transparent", colour = NA),
      panel.border = element_rect(fill = "transparent", colour = NA),
      legend.position = "none",
      plot.title = element_text(hjust = 0.1, size = 5)) 
  
  # Replace slashes and spaces for a suitable filename
  safe_filename <- gsub("/", "_", mountain_range)  # Replace slashes with underscores
  safe_filename <- gsub(" ", "_", safe_filename)  # Replace spaces with underscores
  
  filename <- paste0("~/Desktop/Datasets/output_shared/Vertebrates/bar_plots/svgs_no_lim/residuals_", safe_filename, ".svg")
  
  svg(filename, width = 0.7, height = 0.8, bg = "transparent")
  
  # Print the plot to the SVG device
  print(bar_plot)
  
  # Close the SVG device to finalize the file
  dev.off()
}
