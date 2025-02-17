#--------------------------------------------------------
# Set up and load data
#-----------------------------------------------------------

# no need to run config file for this script

# Load configuration
# source(here::here("R/00_Config_file.R"))

library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gridExtra)
library(svglite)

# load the data either by running 
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Results_and_Figures/01_calculate_richness_and_SAR.R")
)
# or load file
# species_richness_sar_df <- readxl::read_xlsx(paste0(data_storage_path,"/Biodiversity_combined/Final_lists/alpine_biodiversity_database.xlsx"))


#---------------------------------------------------------------------------------------------#
# Get a summary dataframe to report results across individual groups   ---
#----------------------------------------------------------------------------------------------#

# across individual groups 
richness_summary <- species_richness_sar_df |>
  select(Mountain_range,group,filter_condition,richness_group) |>
  pivot_wider(
    names_from = filter_condition,   # This will create new columns based on filter_condition
    values_from = richness_group,    # The values that will populate these new columns
    names_prefix = "richness_"       # Prefix for the new columns
  )


# Convert the dataframe to a long format for easier plotting
long_richness <- richness_summary |>
  pivot_longer(
    cols = starts_with("richness_"),
    names_to = "richness_category",
    values_to = "richness_value"
  )|>
  mutate(richness_category = factor(richness_category, 
                                    levels = c("richness_generalists", 
                                               "richness_degree_6", 
                                               "richness_degree_4", 
                                               "richness_degree_2", 
                                               "richness_specialists", 
                                               "richness_expert_opinion")))

# Create the bar graph
ggplot(long_richness, aes(x = Mountain_range, y = richness_value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ richness_category, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Species Richness across Mountain Ranges and Groups",
       x = "Mountain Range",
       y = "Richness Value",
       fill = "Group")

#---------------------------------------------------------------------------------------------#
# Get a summary dataframe to report results for total richness    ---
#----------------------------------------------------------------------------------------------#
# for total richness
total_richness_summary <- species_richness_sar_df |>
  group_by(Mountain_range,filter_condition)|>
  summarise(total_richness=mean(total_richness),.groups = "drop")|>
  select(Mountain_range,filter_condition,total_richness) |>
  pivot_wider(
    names_from = filter_condition,   # This will create new columns based on filter_condition
    values_from = total_richness,    # The values that will populate these new columns
    names_prefix = "richness_"       # Prefix for the new columns
  )|>
  select(Mountain_range, richness_generalists, richness_degree_6, richness_degree_4, richness_degree_2, richness_specialists)

# 
long_richness_total <- total_richness_summary |>
  pivot_longer(
    cols = starts_with("richness_"),
    names_to = "richness_category",
    values_to = "richness_value"
  )|>
  mutate(richness_category = factor(richness_category, 
                                    levels = c("richness_generalists", 
                                               "richness_degree_6", 
                                               "richness_degree_4", 
                                               "richness_degree_2", 
                                               "richness_specialists", 
                                               "richness_expert_opinion")))

# Create the bar graph
ggplot(long_richness_total, aes(x = Mountain_range, y = richness_value)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ richness_category, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Species Richness across Mountain Ranges and Groups",
       x = "Mountain Range",
       y = "Richness Value")

#---------------------------------------------------------------------------------------------#
# Get the richest and poorest alpine biomes per group    ---
#----------------------------------------------------------------------------------------------#
# Define the function to get top and bottom 5 mountain ranges
get_top_bottom_7 <- function(df, category) {
  df |>
    arrange(desc(!!sym(category))) |>
    filter(!is.na(!!sym(category))) |>
    slice_head(n = 7) |>
    bind_rows(
      df |>
        arrange(!!sym(category)) |>
        filter(!is.na(!!sym(category))) |>
        slice_head(n = 7)
    )
}

# Define richness categories
richness_categories <- c("richness_generalists", "richness_specialists", "richness_degree_2", "richness_degree_4", "richness_degree_6")

# Create an empty list to store separate dataframes
category_results <- list()

# Loop through each richness category
for (category in richness_categories) {
  category_df_list <- list()
  
  for (group_name in unique(richness_summary$group)) {
    top_bottom_7 <- richness_summary |>
      filter(group == group_name) |>
      get_top_bottom_7(category)
    
    top_bottom_7$group <- group_name
    top_bottom_7$richness_category <- category
    
    category_df_list[[group_name]] <- top_bottom_7
  }
  
  # Combine the results for this category into a single dataframe
  category_results[[category]] <- bind_rows(category_df_list)
}

# Each category now has its own dataframe in the category_results list
# You can access them as follows:
richness_generalists_df <- category_results[["richness_generalists"]]|>select(Mountain_range,group,richness_generalists)
richness_specialists_df <- category_results[["richness_specialists"]]|>select(Mountain_range,group,richness_specialists)
richness_degree_2_df <- category_results[["richness_degree_2"]]|>select(Mountain_range,group,richness_degree_2)
richness_degree_4_df <- category_results[["richness_degree_4"]]|>select(Mountain_range,richness_degree_4)
richness_degree_6_df <- category_results[["richness_degree_6"]]|>select(Mountain_range,richness_degree_6)


# Example: View the results for "richness_generalists"
print(richness_generalists_df)


