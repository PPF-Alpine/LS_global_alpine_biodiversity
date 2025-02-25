
#----------------------------------------------------------#
#         Visualizations for Mammal Richness using different thresholds
#----------------------------------------------------------#


#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
# Visualizations IBS

# Map Mammal Richness

library(tidyverse)
library(purrr)
library(plotly)
library(viridis)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

Checklist_Elev <- readxl::read_xlsx("Outputs/Tables/Checklist_Mammals_elevations_Shiny.xlsx")

mountain_shapes <- sf::st_read(paste("Data/Input/mr_with_biomes.shp", sep = "/"),options = "ENCODING=ISO-8859-1",quiet=TRUE)
# select columns to keep
mountain_shapes <- mountain_shapes %>%
  dplyr::select(1, 14, 15, 16, 25, 26) %>% # GMBA ID, GMBA levels 1-3, level 3 is the name of our unit, countries and ISO, geometry
  dplyr::rename(GMBA_ID = 1,
                Continent = 2,
                Mountain_system = 3,
                Mountain_range = 4)

treeline_thresholds <- seq(0, 100, by = 0.1) # Adjust by to your desired increment

# Define a function to perform the filtering and summarizing for a single threshold
calculate_species <- function(threshold, data) {
  filteredData <- data %>%
    dplyr::filter(range_above_treeline > threshold) %>%
    group_by(Mountain_range) %>%
    summarise(
      threshold = threshold,
      number_species = n_distinct(sciname),
      .groups = 'drop'
    )
  return(filteredData)
}

# Use map_df to apply the function across all thresholds and bind the rows into one dataframe
results <- map_df(treeline_thresholds, calculate_species, data = Checklist_Elev)

#-----------------------------#
# 2. Richness Map -----
#------------------------------#

# Get world data excluding Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$continent != "Antarctica",]

# Thresholds
results_subset <- results%>% filter(threshold == 99)

mountain_ranges_to_remove <- unique(results_subset$Mountain_range)

# Subset mountain_shapes to remove the mountain ranges present in results_m
mountain_shapes_subset <- mountain_shapes %>%
  filter(!(Mountain_range %in% mountain_ranges_to_remove))

results_subset <- results_subset%>% left_join(mountain_shapes,by="Mountain_range")

# plot 
p <- ggplot(data = results_subset) + 
  geom_sf(data = world, fill = "gray95", color = "gray95") + 
  geom_sf(aes(fill = number_species, geometry = geometry), color = NA) +
  geom_sf(data = mountain_shapes_subset, fill = NA, color = "grey80", size = 0.5, alpha = 0.5) +
  scale_fill_gradientn(
    name = "Species Richness",
    colours = c("blue3", "lightblue", "chartreuse2", "yellow", "darkorange", "red"),
    limits = c(1, 92),
    breaks = c(1, 25, 50, 75, 92),
    labels = c("1", "25", "50", "75", "92"),
    guide = guide_colourbar(ticks = FALSE, title.position = "top", title.hjust = 0.5, barwidth = 15, barheight = 0.8)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.direction = "horizontal",
    legend.title = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)  # Center the main title
  ) +
  ggtitle(paste("Mammal richness if", 100, "% of a species elevational distribution is above the treeline"))

# Print the plot
print(p)

ggsave(filename = paste("~/Desktop/Datasets/Visualizations/Map/Rplots_threshold_", 100, ".pdf", sep = ""), plot = p, width = 9, height = 5.5)

#-----------------------------#
# 3. Line Plot-----
#------------------------------#

# Plotly
mountain_ranges <- unique(results$Mountain_range)

# Generate a color for each unique mountain range using the viridis color palette
colors <- viridis(length(mountain_ranges))

# Create a named vector of colors for mapping
color_mapping <- setNames(colors, mountain_ranges)

plot_ly(results, x = ~threshold, y = ~number_species, type = 'scatter', mode = 'markers+lines',
        text = ~Mountain_range, hoverinfo = 'text+y+x', color = ~Mountain_range, colors = color_mapping) %>%
  plotly::layout(
    xaxis = list(title = 'Range Above Treeline'),
    yaxis = list(title = 'Number of Species'),
    showlegend = FALSE)

#-------------------------------------------------------------#
# 3. Static Line Plot ordered by max number of species-----
#-------------------------------------------------------------#

# Define the mountain ranges of interest
selected_ranges <- c("Intermountain West", "Tian Shan",
                     "Tibetan Plateau", "Central Andes","Southern Andes","Himalaya", "Rocky Mountains", 
                     "Central European Highlands","South European Highlands", "Northern Andes", "Ethiopian Highlands","Albertine Rift Mountains")



# Create a subset of results with only the selected mountain ranges
subset_mranges <- results %>%
  filter(Mountain_range %in% selected_ranges)

# Assuming results is your data frame and it's already loaded
mountain_ranges <- unique(subset_mranges$Mountain_range)

# Create a custom color palette 
color_palette <- c("#E41A1C", "#377EB8", "forestgreen", "#984EA3", "darkorange", "goldenrod1",
                   "orange4", "#F781BF", "#999999", "#66C2A5", "lightsteelblue2","blue3")


# Create a named vector of colors for mapping
color_mapping <- setNames(color_palette, mountain_ranges)

# Calculate max number of species for each mountain range
max_species <- subset_mranges %>%
  group_by(Mountain_range) %>%
  summarize(max_number_species = max(number_species)) %>%
  ungroup() %>%
  arrange(desc(max_number_species)) %>%
  pull(Mountain_range)

# Reorder the factor levels of Mountain_range according to max_number_species
subset_mranges$Mountain_range <- factor(subset_mranges$Mountain_range, levels = max_species)

# Create a new variable with the continent for each mountain range
mountain_continents <- tibble(
  Mountain_range = c("Albertine Rift Mountains", "Ethiopian Highlands",
                     "Central Andes", "Southern Andes", "Northern Andes",
                     "Central European Highlands","South European Highlands","Intermountain West",
                     "Rocky Mountains", "Himalaya", "Tian Shan", "Tibetan Plateau"),
  Continent = c("Africa", "Africa",
                "South America", "South America", "South America",
                "Europe", "Europe","North America",
                "North America", "Asia", "Asia", "Asia")
)

# Merge this with your subset_mranges data
subset_mranges <- left_join(subset_mranges, mountain_continents, by = "Mountain_range")

final_plot <- ggplot(subset_mranges, aes(x = threshold, y = number_species, group = Mountain_range, color = Mountain_range)) +
  geom_smooth(method = "gam", se = FALSE, linewidth = 0.9) +
  scale_color_manual(values = color_mapping) +
  labs(x = 'Elevational Range Above Treeline (%)', y = 'Number of Species', color = NULL) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),  # Remove the grey shading in the legend keys
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  scale_x_continuous(expand = c(0, 0)) +  # Remove the gap before x = 0
  scale_y_continuous(breaks = seq(0, 100, by = 10))  # Set y-axis breaks

# Display the plot
x11()
plot(final_plot)

resultsfil <- results %>% filter(threshold== 0.0)

#-------------------------------------------------------------#
# 3. Static Line Plot ordered by Continent----
#-------------------------------------------------------------#

# Modify the Mountain_range column to include the continent in brackets
selected_ranges <- c("Intermountain West", "Tian Shan",
                     "Tibetan Plateau", "Central Andes","Southern Andes","Himalaya", "Rocky Mountains", 
                     "Central European Highlands","South European Highlands", "Northern Andes", "Ethiopian Highlands","Albertine Rift Mountains")



# Create a subset of results with only the selected mountain ranges
subset_mranges <- results %>%
  filter(Mountain_range %in% selected_ranges)

# Assuming results is your data frame and it's already loaded
mountain_ranges <- unique(subset_mranges$Mountain_range)

# Create a custom color palette 
color_palette <- c("#E41A1C", "#377EB8", "forestgreen", "#984EA3", "darkorange", "goldenrod1",
                   "orange4", "#F781BF", "#999999", "#66C2A5", "lightsteelblue2","blue3")


# Create a named vector of colors for mapping
color_mapping <- setNames(color_palette, mountain_ranges)

# Calculate max number of species for each mountain range
max_species <- subset_mranges %>%
  group_by(Mountain_range) %>%
  summarize(max_number_species = max(number_species)) %>%
  ungroup() %>%
  arrange(desc(max_number_species)) %>%
  pull(Mountain_range)

# Create a new variable with the continent for each mountain range
mountain_continents <- tibble(
  Mountain_range = c("Albertine Rift Mountains", "Ethiopian Highlands",
                     "Central Andes", "Southern Andes", "Northern Andes",
                     "Central European Highlands","South European Highlands","Intermountain West",
                     "Rocky Mountains", "Himalaya", "Tian Shan", "Tibetan Plateau"),
  Continent = c("Africa", "Africa",
                "South America", "South America", "South America",
                "Europe", "Europe","North America",
                "North America", "Asia", "Asia", "Asia")
)

# Merge this with your subset_mranges data
subset_mranges <- left_join(subset_mranges, mountain_continents, by = "Mountain_range")

subset_mranges <- subset_mranges %>%
  arrange(Continent, Mountain_range)

# Then, create a new factor for Mountain_range that includes the continent in brackets
subset_mranges$Mountain_range <- with(subset_mranges, paste(Mountain_range, " (", Continent, ")", sep = ""))

# Convert the Mountain_range to a factor and specify the levels based on the sorted order
subset_mranges$Mountain_range <- factor(subset_mranges$Mountain_range, levels = unique(subset_mranges$Mountain_range))

# Update the color mapping to use the new factor levels
color_mapping <- setNames(color_palette, levels(subset_mranges$Mountain_range))

# Plot with ggplot using the updated Mountain_range
final_plot <- ggplot(subset_mranges, aes(x = threshold, y = number_species, group = Mountain_range, color = Mountain_range)) +
  geom_smooth(method = "gam", se = FALSE, linewidth = 0.9) +
  scale_color_manual(values = color_mapping) +
  labs(x = 'Elevational Range Above Treeline (%)', y = 'Number of Species', color = NULL) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),  # Remove the grey shading in the legend keys
    legend.text = element_text(size = 12),  
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  scale_x_continuous(expand = c(0, 0)) +  # Remove the gap before x = 0
  scale_y_continuous(breaks = seq(0, 100, by = 10))  # Set y-axis breaks

# Display the plot
x11()
plot(final_plot)


ggsave(filename = paste("~/Desktop/Datasets/Visualizations/Mammal_Richness_Map/RPlots/Line_plot_final.png"), plot = final_plot, width = 9, height = 5.5, bg = "transparent", device = "png")

#-------------------------------------------------------------#
# 3. Static Line Plot Animation----
#-------------------------------------------------------------#
# Define the mountain ranges of interest
selected_ranges <- c("Intermountain West", "Tian Shan",
                     "Tibetan Plateau", "Central Andes","Southern Andes","Himalaya", "Rocky Mountains", 
                     "Central European Highlands","South European Highlands", "Northern Andes", "Ethiopian Highlands","Albertine Rift Mountains")



# Create a subset of results with only the selected mountain ranges
subset_mranges <- results %>%
  filter(Mountain_range %in% selected_ranges)

# Assuming results is your data frame and it's already loaded
mountain_ranges <- unique(subset_mranges$Mountain_range)


# Create a new variable with the continent for each mountain range
mountain_continents <- tibble(
  Mountain_range = c("Albertine Rift Mountains", "Ethiopian Highlands",
                     "Central Andes", "Southern Andes", "Northern Andes",
                     "Central European Highlands","South European Highlands","Intermountain West",
                     "Rocky Mountains", "Himalaya", "Tian Shan", "Tibetan Plateau"),
  Continent = c("Africa", "Africa",
                "South America", "South America", "South America",
                "Europe", "Europe","North America",
                "North America", "Asia", "Asia", "Asia")
)

# Merge this with your subset_mranges data
subset_mranges <- left_join(subset_mranges, mountain_continents, by = "Mountain_range")

# Convert the Mountain_range to a factor and specify the levels based on the sorted order
subset_mranges$Mountain_range <- factor(subset_mranges$Mountain_range, levels = unique(subset_mranges$Mountain_range))

# Then, create a new factor for Mountain_range that includes the continent in brackets
subset_mranges$Mountain_range <- with(subset_mranges, paste(Mountain_range, " (", Continent, ")", sep = ""))

library(ggplot2)
library(dplyr)

# Assuming subset_mranges and color_mapping are already defined
color_palette <- c("#D73027", "#4575B4", "#238443", "#762A83", "#E08214", "#FFD92F",
                   "#A6761D", "#F46D43", "#666666", "#1F78B4", "#99D594", "#08306B")


for(range_name in unique(subset_mranges$Mountain_range)) {
  specific_range_data <- subset_mranges %>%
    filter(Mountain_range == range_name) 
  
  # Create the plot
  plot_for_specific_range <- ggplot(specific_range_data, aes(x = threshold, y = number_species, group = Mountain_range, color = Mountain_range)) +
    geom_smooth(method = "gam", se = FALSE, linewidth = 0.7) +
    scale_color_manual(values = color_mapping) +
    labs(x = 'Elevational Range Above Treeline (%)', y = 'Number of Species', color = NULL) +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    ) +
    scale_x_continuous(limits = c(0, 100),expand =c(0,0)) +
    scale_y_continuous(limits = c(0, 95), breaks = seq(0, 95, by = 10))
  
  # Save the plot
  file_name <- paste0("~/Desktop/Datasets/Visualizations/Mammal_Richness_Map/RPlots/Line_Plot/", gsub("[[:punct:]]| ", "_", range_name), ".png")
  ggsave(filename = file_name, plot = plot_for_specific_range, width = 9, height = 5.5, bg = "transparent", device = "png")
}

#-------------------------------------------------------------#
# 3. Static Line Plot Animation without axes----
#-------------------------------------------------------------#

library(ggplot2)
library(dplyr)

# Assuming subset_mranges and color_mapping are already defined
color_palette <- c("#D73027", "#4575B4", "#238443", "#762A83", "#E08214", "#FFD92F",
                   "#A6761D", "#F46D43", "#666666", "#1F78B4", "#99D594", "#08306B")


for(range_name in unique(subset_mranges$Mountain_range)) {
  specific_range_data <- subset_mranges %>%
    filter(Mountain_range == range_name) 
  
  # Create the plot
  plot_for_specific_range <- ggplot(specific_range_data, aes(x = threshold, y = number_species, group = Mountain_range, color = Mountain_range)) +
    geom_smooth(method = "gam", se = FALSE, linewidth = 0.7) +
    scale_color_manual(values = color_mapping) +
    labs(x = 'Elevational Range Above Treeline (%)', y = 'Number of Species', color = NULL) +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),  # Remove axis lines
      axis.ticks.y = element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.x = element_blank(),  # Remove x axis title
      axis.title.y = element_blank(),  # Remove y axis title
      axis.text.x = element_blank(),   # Remove x axis tick labels
      axis.text.y = element_blank(),   # Remove y axis tick labels
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    ) +
    scale_x_continuous(limits = c(0, 100),expand =c(0,0)) +
    scale_y_continuous(limits = c(0, 95), breaks = seq(0, 95, by = 10))
  
  # Save the plot
  file_name <- paste0("~/Desktop/Datasets/Visualizations/Mammal_Richness_Map/RPlots/Line_Plot/Removed/", gsub("[[:punct:]]| ", "_", range_name), ".png")
  ggsave(filename = file_name, plot = plot_for_specific_range, width = 9, height = 5.5, bg = "transparent", device = "png")
}




#-------------------------------------------------------------#
# 3. Relative Change Line Plot ----
#-------------------------------------------------------------#
subset_mranges <- subset_mranges %>%
  group_by(Mountain_range) %>%
  mutate(relative_number_species = number_species / max(number_species))

# Now, the y-axis in your plot should be relative_number_species instead of number_species
final_plot <- ggplot(subset_mranges, aes(x = threshold, y = relative_number_species, group = Mountain_range, color = Mountain_range)) +
  geom_line()+
  scale_color_manual(values = color_mapping) +
  labs(x = 'Elevational Range Above Treeline (%)', y = 'Relative Number of Species', color = NULL) +
  theme_minimal() +  # Simplified theme, you can customize as needed
  scale_x_continuous(expand = c(0, 0)) +  # Optionally remove the gap before x = 0
  scale_y_continuous(
    breaks = c(0, 0.5, 1),  # Set y-axis breaks to 0, 0.5, and 1
    labels = c("0", "0.5", "1")  # Set y-axis labels to 0, 0.5, and 1
  )

x11()
plot(final_plot)


# Create the ggplot object
final_plot <- ggplot(subset_mranges, aes(x = threshold, y = relative_number_species, group = Mountain_range, color = Mountain_range)) +
  geom_line() +
  scale_color_manual(values = color_mapping) +
  labs(x = 'Elevational Range Above Treeline (%)', y = 'Relative Number of Species', color = NULL) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1), 
    labels = c("0", "0.5", "1")
  )

# Convert the ggplot object to a plotly object
# Convert the ggplot object to a plotly object
interactive_plot <- ggplotly(final_plot,text = ~Mountain_range, hoverinfo = 'text')

# If you're running this in an interactive R session (e.g., RStudio), simply print interactive_plot
print(interactive_plot)

htmlwidgets::saveWidget(interactive_plot, "~/Desktop/Datasets/Visualizations/Mammal_Richness_Map/RPlots/relativelineplot.html", selfcontained = TRUE)


#-----------------------------#
# 3. Heatmap -----
#------------------------------#

# Bin the threshold values into discrete intervals (for example, 10% intervals)
binned_data <- subset_mranges %>%
  mutate(threshold_bin = cut(threshold, breaks=seq(0, 100, by=10),
                             labels=paste(seq(0, 90, by=10), seq(10, 100, by=10), sep='-'))) %>%
  group_by(Mountain_range, threshold_bin) %>%
  summarize(number_species = mean(number_species, na.rm = TRUE)) %>%
  ungroup()

# Reshape the data into a wide format suitable for heatmapping
# Ensure the columns are ordered correctly
heatmap_data <- binned_data %>%
  pivot_wider(names_from = threshold_bin, values_from = number_species) %>%
  select(Mountain_range, paste(seq(0, 90, by=10), seq(10, 100, by=10), sep='-'))

# Generate the heatmap data in long format
heatmap_data_long <- heatmap_data %>%
  pivot_longer(-Mountain_range, names_to = "threshold_bin", values_to = "number_species")

# Generate the heatmap
heatmap <- ggplot(heatmap_data_long, aes(x = threshold_bin, y = Mountain_range, fill = number_species)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue4", "orchid", "indianred3", "#FDE725")) +
  theme_minimal() +
  labs(x = 'Elevational Range Above Treeline (%)', y = 'Mountain Range', fill = 'Number of Species') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for better readability

# Display the heatmap
print(heatmap)
ggsave(filename = paste("~/Desktop/Datasets/Visualizations/Mammal_Richness_Map/RPlots/heatmap.jpg"), plot = heatmap, width = 9, height = 5.5)

#-----------------------------#
# 4. Bubble Plot -----
#------------------------------#

# Define the mountain ranges of interest
selected_ranges <- c("Intermountain West", "Tian Shan",
                     "Tibetan Plateau", "Central Andes","Southern Andes","Himalaya", "Rocky Mountains", 
                     "Central European Highlands","South European Highlands", "Northern Andes", "Ethiopian Highlands","Albertine Rift Mountains")


# Create a subset of results with only the selected mountain ranges
subset_mranges <- results %>%
  filter(Mountain_range %in% selected_ranges)

# Make sure 'Mountain_range' and 'threshold' are factors
subset_mranges$Mountain_range <- factor(subset_mranges$Mountain_range)
subset_mranges$threshold <- factor(subset_mranges$threshold, levels = c("0", "9", "24", "49", "74", "99"))

# Filter out rows where 'threshold' is NA
subset_mranges <- subset_mranges %>% filter(!is.na(threshold))


# Create a new variable with the continent for each mountain range
mountain_continents <- tibble(
  Mountain_range = c("Albertine Rift Mountains", "Ethiopian Highlands",
                     "Central Andes", "Southern Andes", "Northern Andes",
                     "Central European Highlands","South European Highlands","Intermountain West",
                     "Rocky Mountains", "Himalaya", "Tian Shan", "Tibetan Plateau"),
  Continent = c("Africa", "Africa",
                "South America", "South America", "South America",
                "Europe", "Europe","North America",
                "North America", "Asia", "Asia", "Asia")
)

# Merge this with your subset_mranges data
subset_mranges <- left_join(subset_mranges, mountain_continents, by = "Mountain_range")


# Create a discrete viridis color palette with a number of colors equal to the number of continents
num_continents <- length(unique(subset_mranges$Continent))
continent_palette <- viridis::viridis_pal(option = "D", direction = 1, end = 0.9)(num_continents)

# Map continents to this palette
names(continent_palette) <- unique(subset_mranges$Continent)

# Order mountain ranges based on continents
subset_mranges$Mountain_range <- factor(subset_mranges$Mountain_range, 
                                        levels = mountain_continents$Mountain_range[order(mountain_continents$Continent)])


# Now use the continent_colors to color the points in the plot
bubble_plot <- ggplot(subset_mranges, aes(x = Mountain_range, y = threshold, size = number_species, color = Continent)) +
  geom_point(alpha = 0.6, stroke = 1.5, na.rm = TRUE) + 
  scale_size(range = c(1,26), 
             breaks = c(1,25,50,75,91), 
             labels = c(1,25,50,75,92)) + 
  scale_color_manual(values = continent_palette) + 
  theme_minimal() +
  labs(x = 'Mountain Range', y = 'Elevational Range Above Treeline (%)', size = 'Number of Species') +
  theme(
    #legend.position = c(1.2,0.5),  # X, Y coordinates where (1,1) is top-right corner and (1, 0.5) is middle-right plus some extra padding
    #legend.justification = c(1.2, 0.5),
    legend.title = element_text(), # Remove the legend title
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # Rotate x axis labels
    axis.title.x = element_blank(),
    axis.line = element_line(colour = "black"), # Add axis lines
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.key = element_blank(), # Remove the grey background in the legend keys
    legend.text = element_text(size = 8), # Adjust legend text size if necessary
    legend.margin = margin(t = 0, r = 60, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 60, b = 0, l = 0)
  ) +
  guides(size = guide_legend(title = "Number of Species"), color = guide_legend(title = "Continent")) +
  scale_y_discrete(labels = c("> 0", "> 10", "> 25", "> 50", "> 75", "100"), expand = expansion(add = 1.5))

# Print the plot
x11()
plot(bubble_plot)
ggsave(filename = paste("~/Desktop/Datasets/Visualizations/Mammal_Richness_Map/RPlots/bubbleplot_new.jpg"), plot = bubble_plot, width = 14, height = 9)

#-----------------------------#
# 5. Violin Plots -----
#------------------------------#


treeline_thresholds <- seq(0, 100, by = 1) # Adjust by to your desired increment

# Define a function to perform the filtering and summarizing for a single threshold
calculate_species <- function(threshold, data) {
  filteredData <- data %>%
    dplyr::filter(range_above_treeline > threshold) %>%
    group_by(Mountain_range) %>%
    summarise(
      threshold = threshold,
      number_species = n_distinct(sciname),
      .groups = 'drop'
    )
  return(filteredData)
}

# Use map_df to apply the function across all thresholds and bind the rows into one dataframe
results <- map_df(treeline_thresholds, calculate_species, data = Checklist_Elev)
northern_andes_df <- results %>%
  dplyr::filter(Mountain_range == "Northern Andes")

# Now, create the violin plot with the specified aesthetics
p_NA <- ggplot(northern_andes_df, aes(y = number_species, x = factor(1))) +
  geom_violin(aes(x = factor(1)), fill = "lightgrey", alpha = 0.5, trim = FALSE) +
  geom_jitter(aes(x = factor(1), color = threshold), width = 0.2,size=3) +
  scale_colour_gradientn(colours = c("brown3", "yellow", "chartreuse3"),  
                         values = scales::rescale(c(0, 50, 100)),
                         limits = c(0, 100),
                         breaks = c(0, 25, 50, 75, 100), # Optional: to show specific breaks on the legend
                         labels = c("0", "25", "50", "75", "100")) + # Labels for the breaks
  labs(y = "Species Richness") +
  ggtitle("Species Richness for Northern Andes") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

x11()
plot(p_NA)

europe <- results %>%
  dplyr::filter(Mountain_range == "Central European Highlands")

# Now, create the violin plot with the specified aesthetics
p_eur <- ggplot(europe, aes(y = number_species, x = factor(1))) +
  geom_violin(aes(x = factor(1)), fill = "lightgrey", alpha = 0.5, trim = FALSE) +  # Semi-transparent light green fill for the violin
  geom_jitter(aes(x = factor(1), color = threshold), width = 0.2,size=3) +
  scale_colour_gradientn(colours = c("brown3", "yellow", "chartreuse3"),  
                         values = scales::rescale(c(0, 50, 100)),
                         limits = c(0, 100),
                         breaks = c(0, 25, 50, 75, 100), # Optional: to show specific breaks on the legend
                         labels = c("0", "25", "50", "75", "100")) + # Labels for the breaks
  labs(y = "Species Richness") +
  ggtitle("Species Richness for Central European Highlands") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),  # Remove background
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),  # Hide the x-axis text
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),  # Remove panel background
    axis.line = element_line(color = "black"),  # Add axis line
    axis.ticks = element_line(color = "black")  # Add axis ticks
  )
x11()
plot(p_eur)

tshan <- results %>%
  dplyr::filter(Mountain_range == "Tian Shan")

# Now, create the violin plot with the specified aesthetics
p_tshan <- ggplot(tshan, aes(y = number_species, x = factor(1))) +
  geom_violin(aes(x = factor(1)), fill = "lightgrey", alpha = 0.5, trim = FALSE) +  # Semi-transparent light green fill for the violin
  geom_jitter(aes(x = factor(1), color = threshold), width = 0.2,size=3) +
  scale_colour_gradientn(colours = c("brown3", "yellow", "chartreuse3"),  
                         values = scales::rescale(c(0, 50, 100)),
                         limits = c(0, 100),
                         breaks = c(0, 25, 50, 75, 100), # Optional: to show specific breaks on the legend
                         labels = c("0", "25", "50", "75", "100")) + # Labels for the breaks
  labs(y = "Species Richness") +
  ggtitle("Species Richness for Tian Shan") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),  # Remove background
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),  # Hide the x-axis text
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),  # Remove panel background
    axis.line = element_line(color = "black"),  # Add axis line
    axis.ticks = element_line(color = "black")  # Add axis ticks
  )

x11()
plot(p_tshan)

imw <- results %>%
  dplyr::filter(Mountain_range == "Intermountain West")

# Now, create the violin plot with the specified aesthetics
p_imw <- ggplot(imw, aes(y = number_species, x = factor(1))) +
  geom_violin(aes(x = factor(1)), fill = "lightgrey", alpha = 0.5, trim = FALSE) +  # Semi-transparent light green fill for the violin
  geom_jitter(aes(x = factor(1), color = threshold), width = 0.2,size=3) +
  scale_colour_gradientn(colours = c("brown3", "yellow", "chartreuse3"),  
                         values = scales::rescale(c(0, 50, 100)),
                         limits = c(0, 100),
                         breaks = c(0, 25, 50, 75, 100), # Optional: to show specific breaks on the legend
                         labels = c("0", "25", "50", "75", "100")) + # Labels for the breaks
  labs(y = "Species Richness") +
  ggtitle("Species Richness for Intermountain West (North America)") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),  # Remove background
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),  # Hide the x-axis text
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),  # Remove panel background
    axis.line = element_line(color = "black"),  # Add axis line
    axis.ticks = element_line(color = "black")  # Add axis ticks
  )
x11()
plot(p_imw)



ggsave(filename = paste("~/Desktop/Datasets/Visualizations/Mammal_Richness_Map/RPlots/violin_europe.jpg"), plot = p_eur, width = 10, height = 9)

ggsave(filename = paste("~/Desktop/Datasets/Visualizations/Mammal_Richness_Map/RPlots/violin_tshan.jpg"), plot = p_tshan, width = 10, height = 9)

ggsave(filename = paste("~/Desktop/Datasets/Visualizations/Mammal_Richness_Map/RPlots/violin_imw.jpg"), plot = p_imw, width = 10, height = 9)
