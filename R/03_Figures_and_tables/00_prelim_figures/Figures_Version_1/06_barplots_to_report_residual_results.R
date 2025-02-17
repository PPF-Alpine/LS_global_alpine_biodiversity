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
residual_summary <- species_richness_sar_df |>
  select(Mountain_range,group,filter_condition,residuals) |>
  pivot_wider(
    names_from = filter_condition,   # This will create new columns based on filter_condition
    values_from = residuals,    # The values that will populate these new columns
    names_prefix = "res_"       # Prefix for the new columns
  )


# Convert the dataframe to a long format for easier plotting
long_residuals <- residual_summary |>
  pivot_longer(
    cols = starts_with("res_"),
    names_to = "residuals_category",
    values_to = "residuals_value"
  )|>
  mutate(residuals_category = factor(residuals_category, 
                                    levels = c("res_generalists", 
                                               "res_degree_6", 
                                               "res_degree_4", 
                                               "res_degree_2", 
                                               "res_specialists", 
                                               "res_expert_opinion")))

# Create the bar graph
ggplot(long_residuals, aes(x = Mountain_range, y = residuals_value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ residuals_category, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "residuals across Mountain Ranges and Groups",
       x = "Mountain Range",
       y = "residuals Value",
       fill = "Group")


#---------------------------------------------------------------------------------------------#
# Get the richest and poorest alpine biomes per group RICHNESS   ---
#----------------------------------------------------------------------------------------------#
# Define the function to get top and bottom 5 mountain ranges
get_top_bottom_7 <- function(df, category) {
  df |>
    arrange(desc(!!sym(category))) |>
    filter(!is.na(!!sym(category))) |>
    slice_head(n = 9) |>
    bind_rows(
      df |>
        arrange(!!sym(category)) |>
        filter(!is.na(!!sym(category))) |>
        slice_head(n = 9)
    )
}

# Define richness categories
res_categories <- c("res_generalists", "res_specialists", "res_degree_2", "res_degree_4", "res_degree_6", "res_expert_opinion")

# Create an empty list to store separate dataframes
category_results <- list()

# Loop through each richness category
for (category in res_categories) {
  category_df_list <- list()
  
  for (group_name in unique(residual_summary$group)) {
    top_bottom_7 <- residual_summary |>
      filter(group == group_name) |>
      get_top_bottom_7(category)
    
    top_bottom_7$group <- group_name
    top_bottom_7$res_category <- category
    
    category_df_list[[group_name]] <- top_bottom_7
  }
  
  # Combine the results for this category into a single dataframe
  category_results[[category]] <- bind_rows(category_df_list)
}

# Each category now has its own dataframe in the category_results list
# You can access them as follows:
res_generalists_df <- category_results[["res_generalists"]]|>select(Mountain_range,group,res_generalists)
res_specialists_df <- category_results[["res_specialists"]]|>select(Mountain_range,group,res_specialists)
res_degree_2_df <- category_results[["res_degree_2"]]|>select(Mountain_range,group,richness_degree_2)
res_degree_4_df <- category_results[["res_degree_4"]]|>select(Mountain_range,group,richness_degree_4)
res_degree_6_df <- category_results[["res_degree_6"]]|>select(Mountain_range,group,richness_degree_6)
res_expert_opinion_df <- category_results[["res_expert_opinion"]]

# Example: View the results for "richness_generalists"
print(richness_generalists_df)


