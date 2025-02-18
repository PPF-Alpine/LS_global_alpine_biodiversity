#----------------------------#
#     Set up and load data
#----------------------------#

source(here::here("R/00_Config_file.R"))

library(mgcv)
library(ggplot2)
library(purrr)
library(dplyr)
library(performance)
library(patchwork)


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


centroids_ric_sar <- centroids_ric_sar|>
  left_join(mountain_info|>
              select(Mountain_range,mountain_range_ID),by="Mountain_range")

#-----------------------------------------------------------#
#   Richness: comparison models with and without lat range
#-------------------------------------------------------------#

# Model without lat range included
no_lat_range_richness <- gam(
  total_richness_log1p ~ s(latitude),
  data = centroids_ric_sar
)

plot(no_lat_range_richness, pages = 1, main = "richness ~ s(latitude)")
summary(no_lat_range_richness)


# GAM with a linear fixed effect for latitudinal range
with_lat_range_fe_richness <- gam(
  total_richness_log1p ~ s(latitude) + log_lat_range,
  data = centroids_ric_sar
)

summary(with_lat_range_fe_richness)
plot(with_lat_range_fe_richness, pages = 2, main = "richness ~ s(latitude) + log_lat_range")

AIC(no_lat_range_richness,with_lat_range_fe_richness)


#-----------------------------------------------------------#
#   Residuals: comparison models with and without lat range
#-------------------------------------------------------------#

# Model without log_lat_range
model_no_lat_range_residuals <- gam(
  total_residuals ~ s(latitude),
  data = centroids_ric_sar
)
plot(model_no_lat_range_residuals, pages = 1, main = "residuals ~ s(latitude)")
summary(model_no_lat_range_residuals)


# no smoother but lm
# linear model: latitude smooth + log_lat_range as a fixed effect
model_linear_residuals <- gam(
  total_residuals ~ s(latitude) + log_lat_range,
  data = centroids_ric_sar
)
summary(model_linear_residuals)

plot(model_linear_residuals, pages = 1, main = "total_residuals ~ s(latitude) + log_lat_range")


#-----------------------------------------------------------#
#  Plot models
#-------------------------------------------------------------#

library(ggplot2)
library(patchwork)  # For combining plots with insets

# The richness plot
# Create a regular grid of latitude values
latitude_grid <- data.frame(
  latitude = seq(-55, 65, length.out = 1000),  # Adjust x-axis range
  log_lat_range = mean(centroids_ric_sar$log_lat_range)  # Hold log_lat_range constant
)

# Predict values on the grid with confidence intervals
predictions <- predict(with_lat_range_fe_richness, newdata = latitude_grid, type = "link", se.fit = TRUE)
latitude_grid$predicted <- predictions$fit
latitude_grid$lower <- predictions$fit - 1.96 * predictions$se.fit  # Lower CI
latitude_grid$upper <- predictions$fit + 1.96 * predictions$se.fit  # Upper CI

# Do not exponentiate predictions, as the response is already log-transformed
# Create the ggplot with smoother GAM curve and confidence intervals
p_latitude <- ggplot() +
  # Plot data points as triangles
  geom_point(data = centroids_ric_sar, aes(x = latitude, y = total_richness_log1p), 
             alpha = 0.6, color = "black", size = 2) +  # Shape 17 is triangle
  # Plot the GAM curve
  geom_line(data = latitude_grid, aes(x = latitude, y = predicted), 
            color = "#9932CC", size = 1) +
  # Add the confidence interval ribbon
  geom_ribbon(data = latitude_grid, aes(x = latitude, ymin = lower, ymax = upper), 
              fill = "#9932CC", alpha = 0.2) +
  # Labels and themes
  labs(
    x = "Latitude",
    y = "Absolute richness (log1p)"
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
  ) +
  scale_y_continuous(
    breaks = c(3,5,7,9),  # Set custom y-axis breaks
    limits = c(2,9))       # Ensure the y-axis limits match the range


# Create a grid of log_lat_range values
log_lat_range_grid <- data.frame(
  log_lat_range = seq(min(centroids_ric_sar$log_lat_range), max(centroids_ric_sar$log_lat_range), length.out = 100),
  latitude = mean(centroids_ric_sar$latitude)  # Hold latitude constant
)

# Predict the effect of log_lat_range
predictions <- predict(with_lat_range_fe_richness, newdata = log_lat_range_grid, type = "link", se.fit = TRUE)
log_lat_range_grid$predicted <- predictions$fit
log_lat_range_grid$lower <- predictions$fit - 1.96 * predictions$se.fit  # Lower CI
log_lat_range_grid$upper <- predictions$fit + 1.96 * predictions$se.fit  # Upper CI



# Modify the inset plot
p_lat_range <- ggplot(log_lat_range_grid, aes(x = log_lat_range, y = predicted)) +
  geom_line(color = "#9932CC", size = 0.7) +  # Linear effect line
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9932CC", alpha = 0.2) +  # Confidence interval
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
  ) +
  scale_y_continuous(
    breaks = c(3,5,7,9),  # Set custom y-axis breaks
    limits = c(2,9))

# Combine the plots with the latitudinal range plot as an inset
combined_plot_ric <- p_latitude + 
  inset_element(
    p_lat_range +
      coord_fixed(ratio = 1),  # Ensure a square aspect ratio
    left = 0.60,
    right = 0.95,
    bottom = 0.7,  # Move inset plot further up
    top = 1.00      # Adjust top position
  )



# Define the desktop path
desktop_path <- "~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Latitude_plots/"

# Save the richness plot
ggsave(
  filename = paste0(desktop_path, "combined_gam_richness.pdf"), 
  plot = combined_plot_ric, 
  width = 5,  # Increase width to ensure better aspect ratio
  height = 5, # Keep height equal to width for a square
  dpi = 500
)



#-----------------------------------------------------------#
#  Plot models for Total Residuals
#-------------------------------------------------------------#

# The residuals plot
# Create a regular grid of latitude values
latitude_grid <- data.frame(
  latitude = seq(-55, 65, length.out = 1000),  # Adjust x-axis range
  log_lat_range = mean(centroids_ric_sar$log_lat_range)  # Hold log_lat_range constant
)

# Predict values on the grid with confidence intervals
predictions <- predict(model_linear_residuals, newdata = latitude_grid, type = "link", se.fit = TRUE)
latitude_grid$predicted <- predictions$fit
latitude_grid$lower <- predictions$fit - 1.96 * predictions$se.fit  # Lower CI
latitude_grid$upper <- predictions$fit + 1.96 * predictions$se.fit  # Upper CI

# Do not exponentiate predictions, as the response is already log-transformed
# Create the ggplot with smoother GAM curve and confidence intervals
p_latitude <- ggplot() +
  # Plot data points from centroids_ric_sar
  geom_point(data = centroids_ric_sar, aes(x = latitude, y = total_residuals), 
             alpha = 0.6, color = "black", size = 2, shape = 17) +
  # Plot the GAM curve from latitude_grid
  geom_line(data = latitude_grid, aes(x = latitude, y = predicted), 
            color = "#9932CC", size = 1) +
  # Add the confidence interval ribbon
  geom_ribbon(data = latitude_grid, aes(x = latitude, ymin = lower, ymax = upper), 
              fill = "#9932CC", alpha = 0.2) +
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
    limits = c(-1,1))

# Create a grid of log_lat_range values
log_lat_range_grid <- data.frame(
  log_lat_range = seq(min(centroids_ric_sar$log_lat_range), max(centroids_ric_sar$log_lat_range), length.out = 100),
  latitude = mean(centroids_ric_sar$latitude)  # Hold latitude constant
)

# Predict the effect of log_lat_range
predictions <- predict(model_linear_residuals, newdata = log_lat_range_grid, type = "link", se.fit = TRUE)
log_lat_range_grid$predicted <- predictions$fit
log_lat_range_grid$lower <- predictions$fit - 1.96 * predictions$se.fit  # Lower CI
log_lat_range_grid$upper <- predictions$fit + 1.96 * predictions$se.fit  # Upper CI

# Create the ggplot
p_lat_range <- ggplot(log_lat_range_grid, aes(x = log_lat_range, y = predicted)) +
  geom_line(color = "#9932CC", size = 0.7) +  # Linear effect line
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9932CC", alpha = 0.2) +  # Confidence interval
  labs(
    x = "Lat. Range",
    y = "Total Residuals"
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
  ) +
  scale_y_continuous(
    breaks = c(-1,-0.5,0,0.5,1),  # Set custom y-axis breaks
    limits = c(-1,1))

# Combine the plots with the latitudinal range plot as an inset
combined_plot_res <- p_latitude + 
  inset_element(
    p_lat_range +
      coord_fixed(ratio = 1),  # Ensure a square aspect ratio
    left = 0.60,
    right = 0.95,
    bottom = 0.7,  # Move inset plot further up
    top = 1.00 
  )

# Define the desktop path
output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Main/combined_gam_residuals.pdf")

# Save the richness plot
ggsave(
  filename = paste0(output_file), 
  plot = combined_plot_res, 
  width = 5,  # Increase width to ensure better aspect ratio
  height = 5, # Keep height equal to width for a square
  dpi = 500
)


