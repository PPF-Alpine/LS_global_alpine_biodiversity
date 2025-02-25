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
centroid_richness <- RUtilpol::get_latest_file(
  "centroids_richness_sar",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),
  verbose = TRUE
)


#--------------------------------------------------------
# Plotting
#--------------------------------------------------------


# Plot 1: Total Richness (Log) vs. Latitude
# Plot 2: Total Residuals vs. Latitude
centroid_richness$latitude <- as.numeric(centroid_richness$latitude)
centroid_richness$total_richness_log1p <- as.numeric(centroid_richness$total_richness_log1p)
centroid_richness$total_residuals <- as.numeric(centroid_richness$total_residuals)


# Plot 1: Absolute Richness vs. Latitude
plot1 <- ggplot(centroid_richness, aes(x = total_richness_log1p, y = latitude)) +
  geom_point(color = "black", size = 3, alpha = 0.8) +  
  geom_text_repel(
    aes(label = Mountain_range), 
    size = 2.5, 
    max.overlaps = 20,
    segment.size = 0.3,             # Thinner line
    segment.linetype = "dashed"     # Dashed line
  ) +  
  labs(
    x = "Absolute vertebrate richness",
    y = "Latitude (centroids)"
  ) +
  theme_classic() +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot1



# Plot 2: Relative Richness vs. Latitude
plot2 <- ggplot(centroid_richness, aes(x = total_residuals, y = latitude)) +
  geom_point(color = "black", size = 3, alpha = 0.8) +  
  geom_text_repel(
    aes(label = Mountain_range), 
    size = 2.5, 
    max.overlaps = 20,
    segment.size = 0.3,             # Thinner line
    segment.linetype = "dashed"     # Dashed line
  ) +  
  labs(
    x = "Relative vertebrate richness",
    y = "Latitude (centroids)"
  ) +
  theme_classic() +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot2



