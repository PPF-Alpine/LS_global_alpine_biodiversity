
#--------------------------------------------------------
# Set up 
#--------------------------------------------------------

source(here::here("R/00_Config_file.R"))

library(mgcv)
library(ggplot2)
library(purrr)
library(dplyr)
library(performance)
library(patchwork)

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


#-----------------------------------------------------------#
#   Richness: comparison models with and without lat range
#-------------------------------------------------------------#

# Model without lat range included
no_lat_range_richness <- gam(
  generalist_richness_log ~ s(latitude),
  data = alpine_biome_richness
)

plot(no_lat_range_richness, pages = 1, main = "richness ~ s(latitude)")
summary(no_lat_range_richness)


# GAM with a linear fixed effect for latitudinal range
with_lat_range_fe_richness <- gam(
  generalist_richness_log ~ s(latitude) + log_lat_range,
  data = alpine_biome_richness
)

summary(with_lat_range_fe_richness)
plot(with_lat_range_fe_richness, pages = 2, main = "richness ~ s(latitude) + log_lat_range")

AIC(no_lat_range_richness,with_lat_range_fe_richness)


#-----------------------------------------------------------#
#   Residuals: comparison models with and without lat range
#-------------------------------------------------------------#

# Model without log_lat_range
model_no_lat_range_residuals <- gam(
  generalist_residuals_log ~ s(latitude),
  data = alpine_biome_richness
)
plot(model_no_lat_range_residuals, pages = 1, main = "residuals ~ s(latitude)")
summary(model_no_lat_range_residuals)


# no smoother but lm
# linear model: latitude smooth + log_lat_range as a fixed effect
model_linear_residuals <- gam(
  generalist_residuals_log ~ s(latitude) + log_lat_range,
  data = alpine_biome_richness
)
summary(model_linear_residuals)

plot(model_linear_residuals, pages = 1, main = "total_residuals ~ s(latitude) + log_lat_range")



#-----------------------------------------------------------#
#  Absolute Richness Plot: richness~latitude
#-------------------------------------------------------------#

# The richness plot
# Create a regular grid of latitude values
latitude_grid <- data.frame(
  latitude = seq(-55,65, length.out = 1000),  # Adjust x-axis range
  log_lat_range = mean(alpine_biome_richness$log_lat_range)  # Hold log_lat_range constant
)

# Predict values on the grid with confidence intervals
predictions <- predict(with_lat_range_fe_richness, newdata = latitude_grid, type = "link", se.fit = TRUE)
latitude_grid$predicted <- predictions$fit
latitude_grid$lower <- predictions$fit - 1.96 * predictions$se.fit  # Lower CI
latitude_grid$upper <- predictions$fit + 1.96 * predictions$se.fit  # Upper CI


# Create the ggplot with smoother GAM curve and confidence intervals
p_latitude <- ggplot() +
  # Plot data points from centroids_ric_sar
  geom_point(data = alpine_biome_richness, aes(x = latitude, y = generalist_richness_log), 
             alpha = 0.6, color = "black", size = 2) +
  # Plot the GAM curve from latitude_grid
  geom_line(data = latitude_grid, aes(x = latitude, y = predicted), 
            color = "burlywood4", size = 1) +
  # Add the confidence interval ribbon
  geom_ribbon(data = latitude_grid, aes(x = latitude, ymin = lower, ymax = upper), 
              fill = "burlywood4", alpha = 0.2) +
  # Add labels and themes
  labs(
    x = "Latitude",
    y = "Absolute richness (log1p)"
  ) +
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set background to white
    plot.background = element_rect(fill = "white", color = NA),   # Set overall plot background to white
    panel.grid.major = element_blank(),                           # Remove major gridlines
    panel.grid.minor = element_blank(),                           # Remove minor gridlines                  
    axis.text.x = element_text(color = "black", size = 12),       # Increase x-axis tick label size
    axis.text.y = element_text(color = "black", size = 12),       # Increase y-axis tick label size
    axis.title = element_text(color = "black", size = 14),        # Adjust axis title size
    axis.line = element_line(color = "black"),                    # Add axis lines
    axis.ticks = element_line(color = "black"),                   # Add ticks on x and y axes
    axis.ticks.length = unit(0.15, "cm")                          # Adjust length of the ticks
  ) +
  scale_y_continuous(
    breaks = c(3,5,7,9),  # Set custom y-axis breaks
    limits = c(2,9))       # Ensure the y-axis limits match the range

#-----------------------------------------------------------#
# Absolute Richness Plot: Inset for latitudinal range
#-------------------------------------------------------------#

# Create a grid of log_lat_range values
log_lat_range_grid <- data.frame(
  log_lat_range = seq(min(alpine_biome_richness$log_lat_range), max(alpine_biome_richness$log_lat_range), length.out = 100),
  latitude = mean(alpine_biome_richness$latitude)  # Hold latitude constant
)

# Predict the effect of log_lat_range
predictions <- predict(with_lat_range_fe_richness, newdata = log_lat_range_grid, type = "link", se.fit = TRUE)
log_lat_range_grid$predicted <- predictions$fit
log_lat_range_grid$lower <- predictions$fit - 1.96 * predictions$se.fit  # Lower CI
log_lat_range_grid$upper <- predictions$fit + 1.96 * predictions$se.fit  # Upper CI

# Create the ggplot
p_lat_range <- ggplot(log_lat_range_grid, aes(x = log_lat_range, y = predicted)) +
  geom_line(color = "burlywood4", size = 0.7) +  # Linear effect line
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "burlywood4", alpha = 0.2) +  # Confidence interval
  labs(
    x = "Lat. Range",
    y = "Absolute richness (log1p)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set background to white
    plot.background = element_rect(fill = "white", color = NA),   # Set overall plot background to white
    panel.grid.major = element_blank(),                           # Remove major gridlines
    panel.grid.minor = element_blank(),                           # Remove minor gridlines                  
    axis.text.x = element_text(color = "black", size = 10),       # Increase x-axis tick label size
    axis.text.y = element_text(color = "black", size = 10),       # Increase y-axis tick label size
    axis.title = element_text(color = "black", size = 12),        # Adjust axis title size
    axis.line = element_line(color = "black"),                    # Add axis lines
    axis.ticks = element_line(color = "black"),                   # Add ticks on x and y axes
    axis.ticks.length = unit(0.15, "cm")                          # Adjust length of the ticks
  )+
  scale_y_continuous(
    breaks = c(3,5,7,9),  # Set custom y-axis breaks
    limits = c(2,9))+
  scale_x_continuous(
    breaks = c(0,1,2,3),  # Set custom y-axis breaks
    limits = c(0,3))       # Ensure the y-axis limits match the range


#-----------------------------------------------------------#
#  Relative Richness Plot: Combine Plots and save
#-------------------------------------------------------------#

# Combine the plots with the latitudinal range plot as an inset
combined_plot_ric <- p_latitude + 
  inset_element(
    p_lat_range +
      coord_fixed(ratio = 1),  # Ensure a square aspect ratio
    left = 0.60,
    right = 0.95,
    bottom = 0.7,
    top = 0.98
  )

# Display the combined plot
print(combined_plot_ric)

#--------------------------------------------------------
# Save  as PDF
#--------------------------------------------------------
output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Main/")

# Save the richness plot
ggsave(
  filename = paste0(output_file, "combined_gam_richness_plants.pdf"), 
  plot = combined_plot_ric, 
  width = 5,  # Increase width to ensure better aspect ratio
  height = 5, # Keep height equal to width for a square
  dpi = 500
)


#-----------------------------------------------------------#
#   Residuals plots: residuals~latitude
#-------------------------------------------------------------#

# Create a regular grid of latitude values
latitude_grid <- data.frame(
  latitude = seq(-55, 65, length.out = 1000),  # Adjust x-axis range
  log_lat_range = mean(alpine_biome_richness$log_lat_range)  # Hold log_lat_range constant
)

# Predict values on the grid with confidence intervals
predictions <- predict(model_linear_residuals, newdata = latitude_grid, type = "link", se.fit = TRUE)
latitude_grid$predicted <- predictions$fit
latitude_grid$lower <- predictions$fit - 1.96 * predictions$se.fit  # Lower CI
latitude_grid$upper <- predictions$fit + 1.96 * predictions$se.fit  # Upper CI

# Create the ggplot with smoother GAM curve and confidence intervals
p_latitude <- ggplot() +
  # Plot data points from centroids_ric_sar
  geom_point(data = alpine_biome_richness, aes(x = latitude, y = generalist_residuals_log), 
             alpha = 0.6, color = "black", size = 2,shape=17) +
  # Plot the GAM curve from latitude_grid
  geom_line(data = latitude_grid, aes(x = latitude, y = predicted), 
            color = "burlywood4", size = 1) +
  # Add the confidence interval ribbon
  geom_ribbon(data = latitude_grid, aes(x = latitude, ymin = lower, ymax = upper), 
              fill = "burlywood4", alpha = 0.2) +
  # Add labels and themes
  labs(
    x = "Latitude",
    y = "Relative richness (log1p)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set background to white
    plot.background = element_rect(fill = "white", color = NA),   # Set overall plot background to white
    panel.grid.major = element_blank(),                           # Remove major gridlines
    panel.grid.minor = element_blank(),                           # Remove minor gridlines                  
    axis.text.x = element_text(color = "black", size = 12),       # Increase x-axis tick label size
    axis.text.y = element_text(color = "black", size = 12),       # Increase y-axis tick label size
    axis.title = element_text(color = "black", size = 14),        # Adjust axis title size
    axis.line = element_line(color = "black"),                    # Add axis lines
    axis.ticks = element_line(color = "black"),                   # Add ticks on x and y axes
    axis.ticks.length = unit(0.15, "cm")                          # Adjust length of the ticks
  )+
  scale_y_continuous(
    breaks = c(-1,-0.5,0,0.5,1),  # Set custom y-axis breaks
    limits = c(-1.5,1.5))

#-----------------------------------------------------------#
#  Relative Richness Plot: add latitudinal range as inset
#-------------------------------------------------------------#

# Create a grid of log_lat_range values
log_lat_range_grid <- data.frame(
  log_lat_range = seq(min(alpine_biome_richness$log_lat_range), max(alpine_biome_richness$log_lat_range), length.out = 100),
  latitude = mean(alpine_biome_richness$latitude)  # Hold latitude constant
)

# Predict the effect of log_lat_range
predictions <- predict(model_linear_residuals, newdata = log_lat_range_grid, type = "link", se.fit = TRUE)
log_lat_range_grid$predicted <- predictions$fit
log_lat_range_grid$lower <- predictions$fit - 1.96 * predictions$se.fit  # Lower CI
log_lat_range_grid$upper <- predictions$fit + 1.96 * predictions$se.fit  # Upper CI

# Create the ggplot
p_lat_range <- ggplot(log_lat_range_grid, aes(x = log_lat_range, y = predicted)) +
  geom_line(color = "burlywood4", size = 0.7) +  # Linear effect line
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "burlywood4", alpha = 0.2) +  # Confidence interval
  labs(
    x = "Lat. Range",
    y = "Relative richness (log1p)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set background to white
    plot.background = element_rect(fill = "white", color = NA),   # Set overall plot background to white
    panel.grid.major = element_blank(),                           # Remove major gridlines
    panel.grid.minor = element_blank(),                           # Remove minor gridlines                  
    axis.text.x = element_text(color = "black", size = 10),       # Increase x-axis tick label size
    axis.text.y = element_text(color = "black", size = 10),       # Increase y-axis tick label size
    axis.title = element_text(color = "black", size = 12),        # Adjust axis title size
    axis.line = element_line(color = "black"),                    # Add axis lines
    axis.ticks = element_line(color = "black"),                   # Add ticks on x and y axes
    axis.ticks.length = unit(0.15, "cm")                          # Adjust length of the ticks
  )+
  scale_y_continuous(
    breaks = c(-0.5,0,0.5,1),  # Set custom y-axis breaks
    limits = c(-0.5,2.4))  +
  scale_x_continuous(
    breaks = c(0,1,2,3),  # Set custom y-axis breaks
    limits = c(0,3))       # Ensure the y-axis limits match the range

#-----------------------------------------------------------#
#  Combine plots and save
#-------------------------------------------------------------#

# Combine the plots with the latitudinal range plot as an inset
combined_plot_res <- p_latitude + 
  inset_element(
    p_lat_range +
      coord_fixed(ratio = 1),  # Ensure a square aspect ratio
    left = 0.60,
    right = 0.95,
    bottom = 0.7,
    top = 0.98
  )

# Display the combined plot
print(combined_plot_res)


#--------------------------------------------------------
# Save  as PDF
#--------------------------------------------------------
output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Main/")

# Save the combined residuals plot
ggsave(
  filename = paste0(output_file, "combined_gam_residuals_plants.pdf"), 
  plot = combined_plot_res, 
  width = 5, 
  height = 5, 
  dpi = 500
)










