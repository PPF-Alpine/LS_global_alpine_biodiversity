#----------------------------#
#     Set up 
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


#----------------------------------------------------------#
# Load data
#----------------------------------------------------------#

# Load the latest files
alpine_biome_richness <- RUtilpol::get_latest_file(
  "alpine_biome_richness",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)

# meta information about GIFT regions (for labelling)
gift_info <- readxl::read_xlsx(
  path = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/GIFT_info_table.xlsx")
)

# join gift info
alpine_biome_richness <- alpine_biome_richness|>
  left_join(gift_info|>
            select(geo_entity,region_ID),by="geo_entity")|>
  rename(GIFT_ID = region_ID)

#--------------------------------------------------------
# Normalize rel and abs richness to scales 0-1
#--------------------------------------------------------

alpine_biome_richness$latitude <- as.numeric(alpine_biome_richness$latitude)
alpine_biome_richness$richness_log <- as.numeric(alpine_biome_richness$richness_log)
alpine_biome_richness$residuals <- as.numeric(alpine_biome_richness$residuals)


normalized_plant_data <- alpine_biome_richness|>
  mutate(
    norm_residuals = (residuals - min(residuals)) / (max(residuals) - min(residuals)),
    norm_log1p_richness = (richness_log - min(richness_log)) / (max(richness_log) - min(richness_log))
  )

#--------------------------------------------------------
# Plot normalized richness
#--------------------------------------------------------
plot1 <- ggplot(normalized_plant_data, aes(x = norm_log1p_richness, y = norm_residuals)) +
  #geom_point(aes(color = latitude, size = alpine_area), alpha = 0.8) +  # Add color gradient based on latitude
  geom_point(aes(color = latitude, size = 3), alpha = 1) +  # Add color gradient based on latitude
  scale_color_gradientn(
    colors = c("lightgrey","darkgrey","black"),
    limits = c(-55, 65)  
  ) +
  #scale_size_continuous(range = c(2.5, 15), guide = "none") + # Scale for point size
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + # Add diagonal line
  geom_text_repel(
    aes(label = GIFT_ID), 
    size = 3.5, 
    max.overlaps = 10,
    segment.size = 0.2,             # Thinner line
    segment.linetype = "dashed"     # Dashed line
  ) +  
  labs(
    x = "Normalized absolute richness",
    y = "Normalized relative richness",
    color = "Latitude"              # Add legend label for latitude
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

#--------------------------------------------------------
# Save  as PDF
#--------------------------------------------------------

output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Main/normalized_plants.pdf")


# Save the map to a PDF file
pdf(output_file, width = 4.5, height = 3)
print(plot1)
dev.off()



