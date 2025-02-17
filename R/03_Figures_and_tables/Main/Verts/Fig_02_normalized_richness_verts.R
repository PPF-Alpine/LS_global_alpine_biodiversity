#----------------------------#
#     Set up and load data
#----------------------------#

source(here::here("R/00_Config_file.R"))

library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(insight)
library(sf)
library(ggplot2)
library(ggrepel)


# Load checklist of dataframes
centroids_ric_sar <- RUtilpol::get_latest_file(
  "centroids_richness_sar",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),
  verbose = TRUE
)

mountan_info <- RUtilpol::get_latest_file(
  "mountain_range_ID",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains"),
  verbose = TRUE
)


centroid_richness_vert <- centroid_richness_vert|>
  left_join(mountain_info|>
              select(Mountain_range,mountain_range_ID),by="Mountain_range")


#--------------------------------------------------------
# Normalize rel and abs richness to scales 0-1
#--------------------------------------------------------

centroid_richness_vert$latitude <- as.numeric(centroid_richness_vert$latitude)
centroid_richness_vert$total_richness_log1p <- as.numeric(centroid_richness_vert$total_richness_log1p)
centroid_richness_vert$total_residuals <- as.numeric(centroid_richness_vert$total_residuals)
centroid_richness_vert$total_richness <- as.numeric(centroid_richness_vert$total_richness)



normalized_vert_data <- centroid_richness_vert|>
  mutate(
    norm_residuals = (total_residuals - min(total_residuals)) / (max(total_residuals) - min(total_residuals)),
    norm_log1p_richness = (total_richness_log1p - min(total_richness_log1p)) / (max(total_richness_log1p) - min(total_richness_log1p)),
    norm_richness = (total_richness - min(total_richness)) / (max(total_richness) - min(total_richness))
  )

#--------------------------------------------------------
# Plot vertebrates
#--------------------------------------------------------



plot1 <- ggplot(normalized_vert_data, aes(x = norm_log1p_richness, y = norm_residuals)) +
  #geom_point(aes(color = latitude, size = area_size), alpha = 1) +  # Add color and size aesthetics
  geom_point(aes(color = latitude, size = 5), alpha = 1) + 
  scale_color_gradientn(
    colors = c("lightgrey", "darkgrey", "black"),
    limits = c(-55, 65)  
  ) +
  #scale_size_continuous(range = c(2.5, 15), guide = "none") + # Scale for point size
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + # Add diagonal line
  geom_text_repel(
    aes(label = mountain_range_ID), 
    size = 4, 
    max.overlaps = 20,
    segment.size = 0.3,             # Thinner line
    segment.linetype = "dashed"     # Dashed line
  ) +  
  labs(
    x = "Normalized absolute richness",
    y = "Normalized relative richness",
    color = "Latitude",            # Add legend label for latitude
    size = "Log Area"              # Add legend label for size
  ) +
  theme_classic() +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",      # Show legend
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(plot1)

# Define the desktop path
output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Main/normalized_verts.pdf")

# Save the richness plot
ggsave(
  filename = paste0(output_file), 
  plot = plot1, 
  width = 4.5,  # Increase width to ensure better aspect ratio
  height = 3, # Keep height equal to width for a square
  dpi = 500
)

