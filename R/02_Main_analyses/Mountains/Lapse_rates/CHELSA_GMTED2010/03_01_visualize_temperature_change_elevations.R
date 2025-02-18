
# Load configuration
source(
  here::here("R/00_Config_file.R")
)


library(tidyverse)
library(plotly)
library(ggplot2)


# read in lapse rates

elev_change <- readxl::read_xlsx(file.path(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Treeline_Lapse_Rate_04_05.xlsx"))

#----------------------------------------------------------#
# 2. Create a plot using mean lapserate 
#----------------------------------------------------------#
# create a line plot out of this: 
# Order the data frame by Mean_elevation
elev_change <- elev_change |>
  arrange(Mean_elevation)

# Convert Mountain_range to an ordered factor based on Mean_elevation
elev_change$Mountain_range <- factor(elev_change$Mountain_range, levels = unique(elev_change$Mountain_range))

elev_change_long <- elev_change |>
  dplyr::select(Mountain_range, 
                Mean_elevation, 
                Mean_elevation_1_degree, 
                Mean_elevation_2_degree, 
                Mean_elevation_3_degree, 
                Mean_elevation_4_degree, 
                Mean_elevation_5_degree, 
                Mean_elevation_6_degree, Mean_elevation_8_degree,lapse_rate_mean_04,elev_change_1_degree)|>
  gather(key = "Condition", 
         value = "Elevation", 
         -Mountain_range, -lapse_rate_mean_04,-elev_change_1_degree)

# Create the interactive plot 
p <- plot_ly(data = elev_change_long, 
             x = ~Mountain_range, 
             y = ~Elevation, 
             type = 'scatter', 
             mode = 'lines+markers',
             hoverinfo = 'text', 
             text = ~paste('Mountain Range: ', 
                           Mountain_range, 
                           '<br>Elevation: ', Elevation,
                           'm<br>Elev change 1: ', elev_change_1_degree,
                           'm<br>Lapse Rate: ', lapse_rate_mean_04),
             color = ~Condition) |>
  layout(title = 'Elevation Change with temperature Across Mountain Ranges',
         xaxis = list(title = 'Mountain Range'),
         yaxis = list(title = 'Elevation (m)'))
p


#----------------------------------------------------------#
# 2. Create a plot using median lapserate
#----------------------------------------------------------#

# create a line plot out of this: 
# Order the data frame by Mean_elevation
elev_change <- elev_change |>
  arrange(Mean_elevation)

# Convert Mountain_range to an ordered factor based on Mean_elevation
elev_change$Mountain_range <- factor(elev_change$Mountain_range, levels = unique(elev_change$Mountain_range))

elev_change_long_med <- elev_change|>
  dplyr::select(Mountain_range, 
                Mean_elevation, 
                Mean_elevation_1_degree_med, 
                Mean_elevation_2_degree_med, 
                Mean_elevation_3_degree_med, 
                Mean_elevation_4_degree_med, 
                Mean_elevation_5_degree_med, 
                Mean_elevation_6_degree_med, 
                Mean_elevation_8_degree_med,
                lapse_rate_median_04,
                elev_change_1_degree_med) |>
  pivot_longer(cols = -c(Mountain_range, lapse_rate_median_04, elev_change_1_degree_med),
               names_to = "Condition", 
               values_to = "Elevation")

# Create the interactive plot 
p <- plot_ly(data = elev_change_long_med, 
             x = ~Mountain_range, 
             y = ~Elevation, 
             type = 'scatter', 
             mode = 'lines+markers',
             hoverinfo = 'text', 
             text = ~paste('Mountain Range: ', 
                           Mountain_range, 
                           '<br>Elevation: ', Elevation,
                           'm<br>Elev change 1: ', elev_change_1_degree_med,
                           'm<br>Lapse Rate: ', lapse_rate_median_04),
             color = ~Condition) |>
  layout(title = 'Elevation Change with temperature Across Mountain Ranges',
         xaxis = list(title = 'Mountain Range'),
         yaxis = list(title = 'Elevation (m)'))
p


#----------------------------------------------------------#
# Deviation median or mean
#----------------------------------------------------------#
# comparison lapse rate
lapse_rate_comp <- elev_change|>
  dplyr::select(Mountain_range, lapse_rate_median_04, lapse_rate_mean_04) |>
  pivot_longer(cols = -Mountain_range, 
               names_to = "Condition", 
               values_to = "Lapse_rate")
x11()
ggplot(lapse_rate_comp, aes(x = Mountain_range, y = Lapse_rate, fill = Condition)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.4) + 
  theme_minimal() + 
  labs(title = "Level 04: Comparison of Mean and Median over Level 03",
       x = "Mountain Range", 
       y = "Lapse rate") +
  scale_fill_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# comparison lapse rate
lapse_rate_comp <- elev_change|>
  dplyr::select(Mountain_range, lapse_rate_median_05, lapse_rate_mean_05) |>
  pivot_longer(cols = -Mountain_range, 
               names_to = "Condition", 
               values_to = "Lapse_rate")
x11()
ggplot(lapse_rate_comp, aes(x = Mountain_range, y = Lapse_rate, fill = Condition)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.4) + 
  theme_minimal() + 
  labs(title = "Level 05: Comparison of Mean and Median over Level 03",
       x = "Mountain Range", 
       y = "Lapse rate") +
  scale_fill_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


deviations <- elev_change|>
  mutate(Deviation_lr= lapse_rate_mean_04 - lapse_rate_median_04)|>
  mutate(Deviation_elev= elev_change_1_degree - elev_change_1_degree_med)


# Create the plot
x11()
ggplot(deviations, aes(x = reorder(Mountain_range, Deviation_lr), y = Deviation_lr)) +
  geom_bar(stat = "identity", aes(fill = Deviation_lr > 0),alpha=0.5, position = position_dodge(width = 1)) +
  geom_text(aes(label = Mountain_range), hjust = 0.5, vjust = 0, size = 3, position = position_dodge(width = 1),check_overlap = TRUE) +
  labs(x = "Mountain Range", y = "Deviation (Mean LR - Median LR)", fill = "Higher lapse rate") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "grey"),  # Use hex colors for better readability
                    labels = c("Median", "Mean")) +
  theme_minimal() +
  coord_flip() + # Flips the axes so the mountain ranges are on the y-axis
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


#comparison effect of mean median on elevation change

elev_chagne_comp <- elev_change|>
  dplyr::select(Mountain_range,elev_change_1_degree,elev_change_1_degree_med) |>
  pivot_longer(cols = -Mountain_range, 
               names_to = "Condition", 
               values_to = "Lapse_rate")

# Create the plot
x11()
ggplot(deviations, aes(x = reorder(Mountain_range, Deviation_elev), y = Deviation_elev)) +
  geom_bar(stat = "identity", aes(fill = Deviation_lr > 0),alpha=0.5, position = position_dodge(width = 1)) +
  geom_text(aes(label = Mountain_range), hjust = 0.5, vjust = 0, size = 3, position = position_dodge(width = 1),check_overlap = TRUE) +
  labs(x = "Mountain Range", y = "Deviation elevation change 1 degree (Mean LR - Median LR)", fill = "Higher lapse rate") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "grey"),  # Use hex colors for better readability
                    labels = c("Median", "Mean")) +
  theme_minimal() +
  coord_flip() + # Flips the axes so the mountain ranges are on the y-axis
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))

#----------------------------------------------------------#
# Deviation Level 04 or Level 05
#----------------------------------------------------------#

deviations_04_05 <- elev_change|>
  mutate(Deviation_lr= abs(lapse_rate_mean_04 - lapse_rate_mean_05))|>
  mutate(Deviation_elev= abs(elev_change_1_degree - elev_change_1_05))

deviations_04_05 <- elev_change|>
  mutate(Deviation_lr= (lapse_rate_mean_04*-1) - (lapse_rate_mean_05*-1))|>
  mutate(Deviation_elev= elev_change_1_degree - elev_change_1_05)|>
  dplyr::select(Deviation_elev,Deviation_lr,Mountain_range,lapse_rate_mean_04,lapse_rate_mean_05,elev_change_1_degree,elev_change_1_05)

x11()
ggplot(deviations_04_05, aes(x = reorder(Mountain_range, Deviation_lr), y = Deviation_lr)) +
  geom_bar(stat = "identity", aes(fill = Deviation_lr > 0),alpha=0.5, position = position_dodge(width = 1)) +
  geom_text(aes(label = Mountain_range), hjust = 0.5, vjust = 0, size = 3, position = position_dodge(width = 1),check_overlap = TRUE) +
  labs(x = "Mountain Range", y = "Deviation Level 04 and 05 (LR 04 - LR 05)", fill = "Higher lapse rate (i.e., closer to 0)") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "grey"),  # Use hex colors for better readability
                    labels = c("04", "05")) +
  theme_minimal() +
  coord_flip() + # Flips the axes so the mountain ranges are on the y-axis
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))

# elev change
# Open a new graphics window
x11()

# Calculate the range for Deviation_elev
min_dev <- min(deviations_04_05$Deviation_elev, na.rm = TRUE)
max_dev <- max(deviations_04_05$Deviation_elev, na.rm = TRUE)

# Create a sequence of 10 breaks from min to max
breaks <- seq(min_dev, max_dev, length.out = 11)  # length.out should be 11 to create 10 steps

# Generate the plot
x11()
ggplot(deviations_04_05, aes(x = reorder(Mountain_range, Deviation_elev), y = Deviation_elev)) +
  geom_bar(stat = "identity", aes(fill = Deviation_elev > 0), alpha = 0.5, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Mountain_range), hjust = 0.5, vjust = -0.5, size = 3, position = position_dodge(width = 0.9), check_overlap = TRUE) +
  labs(x = "Mountain Range", y = "Deviation Level 04 and 05 (elev change 04 - 05)", fill = "Greater elev change with temp") +
  scale_fill_manual(values = c("TRUE" = "grey", "FALSE" = "darkgreen"), labels = c("04","05")) +
  theme_minimal() +
  coord_flip() + # Flips the axes so the mountain ranges are on the y-axis
  scale_y_continuous(breaks = breaks) +  # Set y-axis breaks based on the sequence
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))

