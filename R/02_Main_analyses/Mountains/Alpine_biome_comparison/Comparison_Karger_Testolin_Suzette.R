library(readxl)
library(ggplot2)
library(tidyverse)
library(openxlsx)

testolin <- read_xlsx("~/Desktop/Datasets/Mountains/Tree_Line/Treeline_Testolin_Complete.xlsx")|> 
  mutate(estimate = "testolin")|>
  select(Mountain_range,estimate,Mean_elevation)

karger <- read_xlsx("~/Desktop/Datasets/Mountains/Tree_Line/Treeline_Karger_Complete_New.xlsx") |> 
  mutate(estimate="karger")|>
  select(Mountain_range,estimate,Mean_elevation)

suzette <- read_xls("~/Desktop/Datasets/Mountains/Tree_Line/Treeline_Suzette.xls")|> 
  mutate(estimate="suzette")|>
  select(Mountain_range,estimate,Mean_elevation)

# Combine the datasets into one dataframe
combined_df <- bind_rows(testolin, karger, suzette)|>
  group_by(Mountain_range) |>
  filter(n_distinct(estimate) == 3) |>
  ungroup()


x11()
ggplot(complete_data_df, aes(x = Mountain_range, y = Mean_elevation, fill = estimate)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) + 
  theme_minimal() + 
  labs(title = "Comparison of Treeline Elevation Estimates",
       x = "Mountain Range", 
       y = "Mean Elevation (m)") +
  scale_fill_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calculate the differences between the estimates
differences_df <- complete_data_df |>
  spread(estimate, Mean_elevation) |>
  mutate(Difference_Testolin_Karger = abs(testolin - karger),
         Difference_Karger_Suzette = abs(karger - suzette),
         Difference_Suzette_Testolin = abs(suzette - testolin)) |>
  gather(key = "Estimate_Difference", value = "Difference", 
         Difference_Testolin_Karger, Difference_Karger_Suzette, Difference_Suzette_Testolin)

# Creating the plot with differences
ggplot(differences_df, aes(x = Mountain_range, y = Difference, group = Estimate_Difference, color = Estimate_Difference)) + 
  geom_point(position = position_dodge(width = 0.75), size = 3) +
  geom_line(position = position_dodge(width = 0.75), aes(group = interaction(Mountain_range, Estimate_Difference))) +
  theme_minimal() +
  labs(title = "Differences in Treeline Elevation Estimates",
       x = "Mountain Range",
       y = "Difference in Elevation (m)") +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#########
# Alpine biomes unique to the datasets

unique_to_karger <- setdiff(karger$Mountain_range, union(testolin$Mountain_range, suzette$Mountain_range))
unique_to_testolin <- setdiff(testolin$Mountain_range, union(karger$Mountain_range, suzette$Mountain_range))
unique_to_suzette <- setdiff(suzette$Mountain_range, union(karger$Mountain_range, testolin$Mountain_range))

# Create a new dataframe for the unique mountain ranges
unique_ranges_df <- bind_rows(
  data.frame(Mountain_range = unique_to_karger, Source = 'Karger'),
  data.frame(Mountain_range = unique_to_testolin, Source = 'Testolin'),
  data.frame(Mountain_range = unique_to_suzette, Source = 'Suzette')
)


# Pairwise unique mountain ranges
unique_karger_vs_testolin <- setdiff(karger$Mountain_range, testolin$Mountain_range)
unique_karger_vs_suzette <- setdiff(karger$Mountain_range, suzette$Mountain_range)

unique_testolin_vs_karger <- setdiff(testolin$Mountain_range, karger$Mountain_range)
unique_testolin_vs_suzette <- setdiff(testolin$Mountain_range, suzette$Mountain_range)

unique_suzette_vs_karger <- setdiff(suzette$Mountain_range, karger$Mountain_range)
unique_suzette_vs_testolin <- setdiff(suzette$Mountain_range, testolin$Mountain_range)

# Combine into a single dataframe for plotting pairwise unique mountain ranges
pairwise_unique_df <- bind_rows(
  data.frame(Mountain_range = unique_karger_vs_testolin, Source = 'Karger_vs_Testolin'),
  data.frame(Mountain_range = unique_karger_vs_suzette, Source = 'Karger_vs_Suzette'),
  data.frame(Mountain_range = unique_testolin_vs_karger, Source = 'Testolin_vs_Karger'),
  data.frame(Mountain_range = unique_testolin_vs_suzette, Source = 'Testolin_vs_Suzette'),
  data.frame(Mountain_range = unique_suzette_vs_karger, Source = 'Suzette_vs_Karger'),
  data.frame(Mountain_range = unique_suzette_vs_testolin, Source = 'Suzette_vs_Testolin')
)

