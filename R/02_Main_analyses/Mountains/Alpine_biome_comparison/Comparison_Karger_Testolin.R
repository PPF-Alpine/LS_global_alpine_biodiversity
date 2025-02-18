
library(readxl)
library(ggplot2)
library(tidyverse)
library(openxlsx)

testolin <- read_xlsx("~/Desktop/Datasets/Mountains/Tree_Line/Treeline_Testolin_Complete.xlsx")

karger <- read_xlsx("~/Desktop/Datasets/Mountains/Tree_Line/Treeline_Karger_Complete_New.xlsx")

suzette <- read_xls("~/Desktop/Datasets/Mountains/Tree_Line/Treeline_Suzette.xls")

# calculate deviations of mean elevation of the treeline
deviations <- karger %>%
  inner_join(testolin, by = "Mountain_range") %>%
  rename(Mean_elevation_karger = Mean_elevation.x, Mean_elevation_testolin = Mean_elevation.y) %>%
  # Calculate the absolute deviation between the two mean elevations
  mutate(Deviation = abs(Mean_elevation_karger - Mean_elevation_testolin)) %>%
  dplyr::select(Mountain_range, Mountain_system = Mountain_system.x, Mean_elevation_karger, Mean_elevation_testolin, Deviation)

write.xlsx(deviations, "~/Desktop/Datasets/Mountains/Tree_Line/Treeline_Karger_Testolin_Comparison.xlsx")

# Get the top 30 mountain ranges with the highest deviations
top_deviations <- deviations %>%
  arrange(desc(Deviation)) %>%
  slice(1:30)

# Create the plot
x11()
ggplot(top_deviations, aes(x = reorder(Mountain_range, -Deviation))) +
  geom_col(aes(y = Mean_elevation_karger, fill = "Karger"), position = position_dodge(width = 0.9), alpha = 1) +
  geom_col(aes(y = Mean_elevation_testolin, fill = "Testolin"), position = position_dodge(width = 0.9), alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, max(top_deviations$Mean_elevation_karger, top_deviations$Mean_elevation_testolin), by = 250)) +
  scale_fill_manual(values = c("Karger" = "darkblue", "Testolin" = "lightblue")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Rotate x-axis text to display mountain range names
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Mountain Range",
       y = "Mean Elevation (m)",
       fill = "Estimate",
       title = "Top 30 Mountain Ranges with the Highest Deviation in Mean Elevation")

# Print the plot
x11()
plot


# Deviations to show which are the higher estimates and where 

deviations <- karger %>%
  inner_join(testolin, by = "Mountain_range") %>%
  # Rename columns to have specific names
  rename(Mean_elevation_karger = Mean_elevation.x, Mean_elevation_testolin = Mean_elevation.y) %>%
  # Calculate the absolute deviation between the two mean elevations
  mutate(Deviation = Mean_elevation_testolin - Mean_elevation_karger) %>%
  # Select the relevant columns
  dplyr::select(Mountain_range, Mountain_system = Mountain_system.x, Mean_elevation_karger, Mean_elevation_testolin, Deviation)



# Create the plot
x11()
ggplot(deviations, aes(x = reorder(Mountain_range, Deviation), y = Deviation)) +
  geom_bar(stat = "identity", aes(fill = Deviation > 0),alpha=0.5, position = position_dodge(width = 1)) +
  geom_text(aes(label = Mountain_range), hjust = 0.8, vjust = 0, size = 3, position = position_dodge(width = 1),check_overlap = TRUE) +
  labs(x = "Mountain Range", y = "Deviation Mean Treeline Elevation (Testolin - Karger)", fill = "Higher Estimate") +
  scale_fill_manual(values = c("TRUE" = "#1F77B4", "FALSE" = "#FF7F0E"),  # Use hex colors for better readability
                    labels = c("Karger", "Testolin")) +
  theme_minimal() +
  coord_flip() + # Flips the axes so the mountain ranges are on the y-axis
  scale_y_continuous(breaks = seq(-2000, max(deviations$Deviation), by = 500)) +  # Add y-axis breaks in 500m steps
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


#---------------------
# Calculate deviation of the mean elevation of the Alpine Biom

testolin <- read_xls("~/Desktop/Datasets/Mountains/Tree_Line/ArcGIS_Mean_Elevation_AB_Testolin.xls")

karger <- read_xls("~/Desktop/Datasets/Mountains/Tree_Line/ArcGIS_Mean_Elevation_AB_Karger.xls")

unique_to_karger <- setdiff(karger$Mountain_range, testolin$Mountain_range)

# Values in testolinn but not in karger
unique_to_testolin <- setdiff(testolin$Mountain_range, karger$Mountain_range)

# Values in both testolinn and karger
common_values <- intersect(testolin$Mountain_range, karger$Mountain_range)
# Print the results
print("Unique to karger:")
print(unique_to_karger)

print("Unique to testolin:")
print(unique_to_testolin)

print("Common values:")
print(common_values)


deviations <- karger %>%
  inner_join(testolin, by = "Mountain_range") %>%
  # Rename columns to have specific names
  rename(Mean_elevation_karger = MEAN.x, Mean_elevation_testolin = MEAN.y) %>%
  # Calculate the absolute deviation between the two mean elevations
  mutate(Deviation = Mean_elevation_testolin - Mean_elevation_karger) %>%
  # Select the relevant columns
  dplyr::select(Mountain_range, Mean_elevation_karger, Mean_elevation_testolin, Deviation)



# Create the plot
x11()
ggplot(deviations, aes(x = reorder(Mountain_range, Deviation), y = Deviation)) +
  geom_bar(stat = "identity", aes(fill = Deviation > 0),alpha=0.5, position = position_dodge(width = 1)) +
  geom_text(aes(label = Mountain_range), hjust = 0.8, vjust = 0, size = 3, position = position_dodge(width = 1),check_overlap = TRUE) +
  labs(x = "Mountain Range", y = "Deviation Mean Treeline Elevation (Testolin - Karger)", fill = "Higher Estimate") +
  scale_fill_manual(values = c("TRUE" = "#1F77B4", "FALSE" = "#FF7F0E"),  # Use hex colors for better readability
                    labels = c("Karger", "Testolin")) +
  theme_minimal() +
  coord_flip() + # Flips the axes so the mountain ranges are on the y-axis
  scale_y_continuous(breaks = seq(-2000, max(deviations$Deviation), by = 500)) +  # Add y-axis breaks in 500m steps
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


# Create dataframes for unique mountain ranges
unique_karger_df <- data.frame(Mountain_range = unique_to_karger, 
                               Mean_elevation_karger = karger$MEAN[karger$Mountain_range %in% unique_to_karger],
                               Mean_elevation_testolin = NA,
                               Deviation = NA)

unique_testolin_df <- data.frame(Mountain_range = unique_to_testolin, 
                                 Mean_elevation_karger = NA,
                                 Mean_elevation_testolin = testolin$MEAN[testolin$Mountain_range %in% unique_to_testolin],
                                 Deviation = NA)

# Combine with existing deviations dataframe
deviations <- rbind(deviations, unique_karger_df, unique_testolin_df)

# Create the plot
# Add mountain ranges that are unique to only on AB estimate
deviations$TextColor <- ifelse(deviations$Mountain_range %in% unique_to_karger, 'Karger', 
                               ifelse(deviations$Mountain_range %in% unique_to_testolin, 'Testolin', 'Common'))

# Now plot with the new TextColor column to specify the color of the text labels
x11()
ggplot(deviations, aes(x = reorder(Mountain_range, Deviation), y = ifelse(is.na(Deviation), 0, Deviation))) +
  geom_bar(stat = "identity", aes(fill = !is.na(Deviation) & Deviation > 0), alpha=0.5, position = position_dodge(width = 1)) +
  geom_text(aes(label = Mountain_range, color = TextColor), hjust = 0.8, vjust = 0, size = 3, position = position_dodge(width = 1), check_overlap = TRUE) +
  labs(x = "Mountain Range", y = "Deviation Mean Elevation Alpine Biome (Testolin - Karger)") +
  scale_fill_manual(values = c("TRUE" = "#1F77B4", "FALSE" = "#FF7F0E"),
                    labels = c("Karger", "Testolin")) +
  scale_color_manual(values = c("Karger" = "#FF7F0E", "Testolin" = "#1F77B4", "Common" = "black")) +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(breaks = seq(-2000, max(deviations$Deviation, na.rm = TRUE), by = 500)) +
  theme(legend.position = "bottom", axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  guides(fill = guide_legend(title = "Higher Estimate", override.aes = list(color = c("#FF7F0E","#1F77B4"))), color = "none")  # Removing color legend



# Get the top 30 mountain ranges with the highest deviations
top_deviations <- deviations %>%
  arrange(desc(Deviation)) %>%
  slice(1:30)

# Create the plot
x11()
ggplot(top_deviations, aes(x = reorder(Mountain_range, -Deviation))) +
  geom_col(aes(y = Mean_elevation_karger, fill = "Karger"), position = position_dodge(width = 0.9), alpha = 1) +
  geom_col(aes(y = Mean_elevation_testolin, fill = "Testolin"), position = position_dodge(width = 0.9), alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, max(top_deviations$Mean_elevation_karger, top_deviations$Mean_elevation_testolin), by = 250)) +
  scale_fill_manual(values = c("Karger" = "darkblue", "Testolin" = "lightblue")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Rotate x-axis text to display mountain range names
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Mountain Range",
       y = "Mean Elevation (m)",
       fill = "Estimate",
       title = "Deviation in Mean Elevation of Alpine Biome")

