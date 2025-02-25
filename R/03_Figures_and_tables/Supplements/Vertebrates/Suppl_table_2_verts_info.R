
#----------------------------#
#     Set up and load data
#----------------------------#

# libraries
library(here)
library(tidyverse)
library(RUtilpol)
library(readxl)
library(gt)


source(here::here("R/00_Config_file.R"))


mountain_info <- readxl::read_excel(paste0(data_storage_path,"subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Mountains//mountain_info_table.xlsx"))


# join richness values for each mountain range 
# Load checklist of dataframes
richness_sar <- RUtilpol::get_latest_file(
  "richness_sar",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),
  verbose = TRUE
)

#----------------------------#
#   Reshape data 
#----------------------------#

reshaped_ric_table <- richness_sar |>
  filter(group=="birds")|>
  select(Mountain_range, filter_condition, total_richness, total_residuals) |>
  pivot_wider(
    names_from = filter_condition,
    values_from = c(total_richness, total_residuals)
  )|>
  left_join(mountain_info|>select(Mountain_range,mountain_range_ID),by="Mountain_range")|>
  relocate(mountain_range_ID, Mountain_range, 
           total_richness_generalists,total_residuals_generalists,
           total_richness_degree_6,total_residuals_degree_6,
           total_richness_degree_4,total_residuals_degree_4,
           total_richness_degree_2,total_residuals_degree_2,
           total_richness_specialists,total_residuals_specialists) 



gt_table <- reshaped_ric_table|>
  arrange(mountain_range_ID)|>
  gt() |>
  fmt_number(
    columns = c(total_residuals_generalists, 
                total_residuals_specialists,
                total_residuals_degree_2,
                total_residuals_degree_4,
                total_residuals_degree_6), # Specify longitude and latitude columns
    decimals = 2 # Retain 2 decimal places for these columns
  ) |>
  fmt_number(
    columns = c(total_richness_generalists, total_richness_specialists, 
                total_richness_degree_2,total_richness_degree_4,total_richness_degree_6), # Other numeric columns
    decimals = 0 # Round to 0 decimals for these columns
  ) |>
  cols_label(
    mountain_range_ID = "Mountain range ID",
    Mountain_range = "Mountain range",
    total_richness_generalists = "AR generalists",
    total_richness_specialists = "AR specialists",
    total_residuals_generalists = "RR generalists",
    total_residuals_specialists ="RR specialists",
    total_richness_degree_2 = "AR UFL-alpine",
    total_residuals_degree_2 = "RR UFL-alpine",
    total_richness_degree_4 = "AR mid-montane",
    total_residuals_degree_4 = "RR mid-montane",
    total_richness_degree_6 = "AR broad-montane",
    total_residuals_degree_6 = "RR broad-montane"
  ) |>
  tab_style(
    style = cell_borders(sides = "left", color = "lightgrey", weight = px(0.1)), # Adds column borders
    locations = cells_body(columns = everything()) # Applies to all body columns
  )
# Print the table
gt_table



#----------------------------#
# GT with in between headers 
#----------------------------#

gt_table_headers <- reshaped_ric_table |>
  arrange(mountain_range_ID) |>
  gt() |>
  fmt_number(
    columns = c(total_residuals_generalists, 
                total_residuals_specialists,
                total_residuals_degree_2,
                total_residuals_degree_4,
                total_residuals_degree_6), # Relative Richness (RR) columns
    decimals = 2 # Retain 2 decimal places for these columns
  ) |>
  fmt_number(
    columns = c(total_richness_generalists, 
                total_richness_specialists, 
                total_richness_degree_2,
                total_richness_degree_4,
                total_richness_degree_6), # Absolute Richness (AR) columns
    decimals = 0 # Round to 0 decimals for these columns
  ) |>
  cols_label(
    mountain_range_ID = "Mountain range ID",
    Mountain_range = "Mountain range",
    total_richness_generalists = "absolute richness",
    total_richness_specialists = "absolute richness",
    total_residuals_generalists = "relative richness",
    total_residuals_specialists = "relative richness",
    total_richness_degree_2 = "absolute richness",
    total_residuals_degree_2 = "relative richness",
    total_richness_degree_4 = "absolute richness",
    total_residuals_degree_4 = "relative richness",
    total_richness_degree_6 = "absolute richness",
    total_residuals_degree_6 = "relative richness"
  ) |>
  tab_style(
    style = cell_borders(sides = "left", color = "lightgrey", weight = px(0.1)), # Adds column borders
    locations = cells_body(columns = everything()) # Applies to all body columns
  ) |>
  tab_spanner(
    label = "Generalists", # Spanner for Generalists
    columns = c(total_richness_generalists, total_residuals_generalists)
  ) |>
  tab_spanner(
    label = "Specialists", # Spanner for Specialists
    columns = c(total_richness_specialists, total_residuals_specialists)
  ) |>
  tab_spanner(
    label = "UFL-alpine", # Spanner for UFL-alpine
    columns = c(total_richness_degree_2, total_residuals_degree_2)
  ) |>
  tab_spanner(
    label = "Mid-montane", # Spanner for mid-montane
    columns = c(total_richness_degree_4, total_residuals_degree_4)
  ) |>
  tab_spanner(
    label = "Broad-montane", # Spanner for broad-montane
    columns = c(total_richness_degree_6, total_residuals_degree_6)
  )

# Print the table
gt_table_headers


gtsave(gt_table_headers,paste0(data_storage_path,"subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Verts/verts_richness_table.html"))
