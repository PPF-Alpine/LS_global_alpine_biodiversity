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
  filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_4_degree)|>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_treeline)|>
  #filter(max_elevation_USE >= Mean_elevation_treeline) |>
  group_by(Mountain_range) |>
  summarise(total_richness = n_distinct(sciname), .groups = "drop")

#  number of species per group for each mountain range
species_per_group <- checklist_selected |>
  filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_4_degree)|>
  #filter(max_elevation_USE >= Mean_elevation_treeline) |>
  #filter(max_elevation_USE >= Mean_elevation_treeline & min_elevation_USE >= Mean_elevation_treeline)|>
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

#--------------------------------------------------------
# Plot for single mountain range
#-----------------------------------------------------------

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
circular_barplot <- ggplot(test, aes(x = group, y = n_group, fill = n_group_log)) +
  geom_col() +
  coord_polar(theta = "y") +
  scale_y_continuous(
    limits = c(-(max(test$n_group)/10), max(test$n_group)),
    expand = c(0, 0))+
  scale_fill_gradientn(colors = colors, limits = c(0, 6)) +
  labs(title = paste("Species Richness", unique(test$Mountain_range)),
       x = NULL, y = NULL,
       fill = NULL) +
  theme_void() +  # Use theme_void() to remove all axes and grid lines
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = "black"),  # Re-add group labels
        legend.position = "none",  # Remove the legend
        plot.title = element_text(hjust = 0.1, size = 10))


# Define the full path for the filename using the unique Mountain_range value
filename <- paste0("~/Desktop/Datasets/Biodiversity_combined/Visuals/Circle_plots/Species_Richness_", unique(test$Mountain_range), ".svg")

# Save the plot as SVG
ggsave(filename, plot = circular_barplot, device = "svg", bg = "transparent", width = 8, height = 6)

#--------------------------------------------------------
# Loop through all mountains and save as single svgs
#-----------------------------------------------------------

# Define custom color gradient
colors <- c("#1A2A5A", "#256C85", "#37AF91", "#44BF2F", "#A3970E", "#F2C12E",
            "#F2994A", "#D8461B", "#AD2E24", "#4E1703")

# Loop through each unique mountain range
for (mountain_range in unique(species_richness_modified$Mountain_range)) {
  # Filter data for the current mountain range
  test <- species_richness_modified %>% 
    filter(Mountain_range == mountain_range)
  
  # Create the plot
  circular_barplot <- ggplot(test, aes(x = factor(group, levels = c("Reptiles", "Mammals", "Birds")), y = n_group, fill = n_group_log)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "x") +
    scale_y_continuous(
      limits = c(-(max(test$n_group)/10), max(test$n_group)),
      expand = c(0, 0)) +
    scale_fill_gradientn(colors = colors, limits = c(0, 6)) +
    labs(title = paste("Species Richness", mountain_range),
         x = NULL, y = NULL,
         fill = NULL) +
    theme_void() +  # Use theme_void to remove all axes and grid lines
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = "black"),  # Re-add group labels
          legend.position = "none",  # Remove the legend
          plot.title = element_text(hjust = 0.1, size = 10))
  
  # Print the plot (optional, useful for checking in interactive session)
  print(circular_barplot)
  
  # Replace slashes and spaces for a suitable filename
  safe_filename <- gsub("/", "_", mountain_range)  # Replace slashes with underscores
  safe_filename <- gsub(" ", "_", safe_filename)  # Replace spaces with underscores
  
  filename <- paste0("~/Desktop/Datasets/Biodiversity_combined/Visuals/Circle_plots/Species_Richness_", safe_filename, ".svg")
  ggsave(filename, plot = circular_barplot, device = "svg", bg = "transparent", width = 4, height = 4)
}


#--------------------------------------------------------
# Loop through all mountains and save as one pdf
#-----------------------------------------------------------

# Initialize an empty list to store plots
plot_list <- list()

# Loop through each unique mountain range
for (mountain_range in unique(species_richness_modified$Mountain_range)) {
  # Filter data for the current mountain range
  test <- species_richness_modified %>% 
    filter(Mountain_range == mountain_range)
  
  # 
  circular_barplot <- ggplot(test, aes(x = factor(group, levels = c("Reptiles", "Mammals", "Birds")), y = n_group, fill = n_group_log)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "x") +
    scale_y_continuous(
      limits = c(-(max(test$n_group)/10), max(test$n_group)),
      expand = c(0, 0)) +
    scale_fill_gradientn(colors = colors, limits = c(0, 6)) +
    labs(title = paste(mountain_range),
         x = NULL, y = NULL,
         fill = NULL) +
    theme_void() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6, color = "black"),
          legend.position = "none",
          plot.title = element_text(hjust = 0.1, size = 6))
  
  # Add the plot to the list
  plot_list[[mountain_range]] <- circular_barplot
}


# 
pdf("~/Desktop/Datasets/Biodiversity_combined/Visuals/Circle_plots/Species_Richness_4_degree.pdf",width=14,height=8)
marrangeGrob(grobs = plot_list, nrow = 2, ncol = 5, pages = ceiling(length(plot_list) / 10)) 
dev.off()



