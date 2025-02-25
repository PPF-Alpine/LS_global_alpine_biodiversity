
# Combine all groups and show species richness in alpine biomes
# I have differently sized polygons of the alpine biome - find the relationship between area size and species richness

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gridExtra)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

# Run the Data Preparation file 
# this script reformats the mammal data
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
  filter(max_elevation_USE >= Mean_elevation_treeline) |>
  group_by(Mountain_range) |>
  summarise(total_richness = n_distinct(sciname), .groups = "drop")

#  number of species per group for each mountain range
species_per_group <- checklist_selected |>
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


#----------------------------------------------------------#
# Fit a linear model specis richness ~ area size -----
#----------------------------------------------------------#

# Fit a lm
lm_mod <- lm(log1p(total_richness)~log1p(area_size),data=species_richness_combined,na.action=na.exclude)

# Calculate predicted richness
# model predicts value of log(1+ richness) use exp to transform predictions back to the original scale
species_richness_combined$predicted_richness <- exp(predict(lm_mod, newdata = species_richness_combined)) 

# Calculate residuals
species_richness_combined$residuals <- insight::get_residuals(lm_mod,type="response")

# log the residuals and richness 
species_richness_combined <- species_richness_combined|> 
  mutate(residuals_log = sign(residuals) * log1p(abs(residuals)))|> # to keep the sign of residuals
  mutate(richness_log = log1p(total_richness))

plot(log1p(total_richness)~log1p(area_size),data=species_richness_combined)
abline(lm)
title(main = "Biodiversity combined - lm")

#-------------------------------------------------------------------------#
# Plot Residuals of Species Area Relationship Linear Model 
#------------------------------------------------------------------------#

# Colour by proportion of group contributing to overall richness 


## Use the mountain ranges on y scale
p_res_log_comb <- species_richness_combined |>
  mutate(mountain_range_ordered_reslog = reorder(Mountain_range, residuals_log)) |>
  arrange(desc(residuals_log)) |>
  ggplot(aes(x = mountain_range_ordered_reslog, y = residuals_log, fill = residuals_log, 
             text = paste("Mountain Range:", Mountain_range))) +
  geom_bar(stat = "identity", width = 0.5,position = "dodge") +
  scale_fill_gradientn(colors = c("#1A2A5A",
                                  "#256C85", 
                                  "#37AF91", 
                                  "#44BF2F",
                                  "#A3970E",
                                  "#F2C12E",
                                  "#F2994A",
                                  "#D8461B", 
                                  "#AD2E24",
                                  "#4E1703")) +
  coord_flip() +  # Flips the x and y axes
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        legend.position = "none") +
  labs(title = "Vertebrates Residuals SAR - no lower limit", x = "Alpine Biome", y = "Residuals")

# To hover over the bars
p_res_log_interactive <- ggplotly(p_res_log_comb, tooltip = "text")|>  layout(showlegend = FALSE)

# Print interactive plot
p_res_log_interactive


#-------------------------------------------------------------------------#
# Plot  species richness in Alpine Biomes ---
#------------------------------------------------------------------------#

p_ric_comb <- species_richness_combined |>
  mutate(mountain_range_ordered_ric = reorder(Mountain_range, total_richness)) |>
  arrange(desc(total_richness)) |>
  ggplot(aes(x = mountain_range_ordered_ric, y = total_richness, 
             fill = total_richness, text = paste("Mountain Range:", Mountain_range, 
                                               "<br>Richness:", total_richness))) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  scale_fill_gradientn(colors = c("#1A2A5A",
                                  "#256C85", 
                                  "#37AF91", 
                                  "#44BF2F",
                                  "#A3970E",
                                  "#F2C12E",
                                  "#F2994A",
                                  "#D8461B", 
                                  "#AD2E24",
                                  "#4E1703")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        legend.position = "none") +
  labs(title = "Vertebrate Richness - no lower limit", x = "Alpine Biome", y = "Richness")

# Convert to an interactive plotly object
p_ric_interactive <- ggplotly(p_ric_comb, tooltip = "text") |>
  layout(showlegend = FALSE)

# Print the interactive plot without the legend
p_ric_interactive

#-------------------------------------------------------------------------#
# Plot logged vertebrate species richness in Alpine Biomes ---
#------------------------------------------------------------------------#

# species richness logged
p_ric_log <- species_richness_combined |>
  mutate(mountain_range_ordered_riclog = reorder(Mountain_range, richness_log)) |>
  arrange(desc(richness_log)) |>
  ggplot(aes(x = mountain_range_ordered_riclog, y = richness_log, 
             fill = richness_log, text = paste("Mountain Range:", Mountain_range, 
                                               "<br>Richness:", total_richness))) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  scale_fill_gradientn(colors = c("#1A2A5A",
                                  "#256C85", 
                                  "#37AF91", 
                                  "#44BF2F",
                                  "#A3970E",
                                  "#F2C12E",
                                  "#F2994A",
                                  "#D8461B", 
                                  "#AD2E24",
                                  "#4E1703")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        legend.position = "none") +
  labs(title = "Vertebrate Richness - no lower limit", x = "Alpine Biome", y = "Richness (log)")

# Convert to an interactive plotly object
p_ric_log_interactive <- ggplotly(p_ric_log, tooltip = "text") |>
  layout(showlegend = FALSE)

# Print the interactive plot without the legend
p_ric_log_interactive

#-------------------------------------------------------------------------#
# Plot logged vertebrate species richness for individual groups ---
#------------------------------------------------------------------------#

p_ric_group <- species_richness_combined |>
  mutate(mountain_range_ordered_ric = reorder(Mountain_range, total_richness)) |>
  arrange(desc(total_richness)) |>
  ggplot(aes(x = mountain_range_ordered_ric, y = n_group_log, fill = group,
             text = paste("Mountain Range:", Mountain_range))) +
  geom_bar(stat = "identity", width = 0.7, position = "dodge") +
  scale_fill_manual(values = c("Birds" = "#256C85", "Mammals" = "#AD2E24", "Reptiles" = "#44BF2F")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        legend.position = "right") +
  labs(title = "Vertebrates Richness - no lower limit",
       x = "Alpine Biome", y = "Richness (logx+1)")

# Print the plot
print(p_ric_group)

#-------------------------------------------------------------------------#
# Plot logged vertebrate species residuals for individual groups ---
#------------------------------------------------------------------------#

# Function to fit model, predict richness, and calculate residuals for a specific group
sar_for_groups <- function(data, group_name) {
  # Filter data for the group
  data_group <- filter(data, group == group_name)
  
  # Fit the linear model
  lm_mod <- lm(log1p(n_group) ~ log1p(area_size), data = data_group, na.action = na.exclude)
  
  # Calculate predicted richness and transform back
  data_group$predicted_richness <- exp(predict(lm_mod, newdata = data_group))
  
  # Calculate residuals
  data_group$residuals <- insight::get_residuals(lm_mod, type = "response")
  
  # Log the residuals and richness
  data_group <- data_group |>
    mutate(residuals_log = sign(residuals) * log1p(abs(residuals)),
           richness_log = log1p(total_richness))
  
  # Return the modified group data
  data_group
}

# List of groups
groups <- unique(species_richness_combined$group)

# Apply the function to each group and bind the results into a single dataframe
species_richness_modified <- map_df(groups, ~sar_for_groups(species_richness_combined, .x))


# plot residuals per group
p_res_group <- species_richness_modified |>
  mutate(mountain_range_ordered_ric = reorder(Mountain_range, total_richness)) |>
  arrange(desc(total_richness)) |>
  ggplot(aes(x = mountain_range_ordered_ric, y = residuals, fill = group,
             text = paste("Mountain Range:", Mountain_range))) +
  geom_bar(stat = "identity", width = 0.7, position = "dodge") +
  scale_fill_manual(values = c("Birds" = "#256C85", "Mammals" = "#AD2E24", "Reptiles" = "#44BF2F")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        legend.position = "right") +
  labs(title = "Vertebrates Residuals - no lower limit",
       x = "Alpine Biome", y = "Residuals")

# Print the plot
print(p_res_group)


p_ric_group <- p_ric_group + theme(legend.position = "none")
pdf("~/Desktop/Datasets/Biodiversity_combined/Visuals/Species_Area_Relationship/ric_res_combined.pdf", width = 16, height = 11) 
grid.arrange(p_ric_group,p_res_group,ncol = 2)
# Close the PDF device
dev.off()
