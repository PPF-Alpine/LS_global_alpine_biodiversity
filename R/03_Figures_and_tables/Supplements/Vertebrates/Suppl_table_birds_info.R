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
  select(Mountain_range, filter_condition, richness_group, group_residuals) |>
  pivot_wider(
    names_from = filter_condition,
    values_from = c(richness_group, group_residuals)
  )|>
  left_join(mountain_info|>select(Mountain_range,mountain_range_ID),by="Mountain_range")|>
  relocate(mountain_range_ID, Mountain_range, 
           richness_group_generalists,group_residuals_generalists,
           richness_group_degree_6,group_residuals_degree_6,
           richness_group_degree_4,group_residuals_degree_4,
           richness_group_degree_2,group_residuals_degree_2,
           richness_group_specialists,group_residuals_specialists) 

#----------------------------#
# GT with in between headers 
#----------------------------#

gt_table <- reshaped_ric_table |>
  arrange(mountain_range_ID) |>
  gt() |>
  fmt_number(
    columns = c(group_residuals_generalists, 
                group_residuals_specialists,
                group_residuals_degree_2,
                group_residuals_degree_4,
                group_residuals_degree_6), # Relative Richness (RR) columns
    decimals = 2 # Retain 2 decimal places for these columns
  ) |>
  fmt_number(
    columns = c(richness_group_generalists, 
                richness_group_specialists, 
                richness_group_degree_2,
                richness_group_degree_4,
                richness_group_degree_6), # Absolute Richness (AR) columns
    decimals = 0 # Round to 0 decimals for these columns
  ) |>
  cols_label(
    mountain_range_ID = "Mountain range ID",
    Mountain_range = "Mountain range",
    richness_group_generalists = "absolute richness",
    richness_group_specialists = "absolute richness",
    group_residuals_generalists = "relative richness",
    group_residuals_specialists = "relative richness",
    richness_group_degree_2 = "absolute richness",
    group_residuals_degree_2 = "relative richness",
    richness_group_degree_4 = "absolute richness",
    group_residuals_degree_4 = "relative richness",
    richness_group_degree_6 = "absolute richness",
    group_residuals_degree_6 = "relative richness"
  ) |>
  tab_style(
    style = cell_borders(sides = "left", color = "lightgrey", weight = px(0.1)), # Adds column borders
    locations = cells_body(columns = everything()) # Applies to all body columns
  ) |>
  tab_spanner(
    label = "Generalists", # Spanner for Generalists
    columns = c(richness_group_generalists, group_residuals_generalists)
  ) |>
  tab_spanner(
    label = "Specialists", # Spanner for Specialists
    columns = c(richness_group_specialists, group_residuals_specialists)
  ) |>
  tab_spanner(
    label = "UFL-alpine", # Spanner for UFL-alpine
    columns = c(richness_group_degree_2, group_residuals_degree_2)
  ) |>
  tab_spanner(
    label = "Mid-montane", # Spanner for mid-montane
    columns = c(richness_group_degree_4, group_residuals_degree_4)
  ) |>
  tab_spanner(
    label = "Broad-montane", # Spanner for broad-montane
    columns = c(richness_group_degree_6, group_residuals_degree_6)
  )

# Print the table
gt_table



gtsave(gt_table,paste0(data_storage_path,"subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Verts/birds_info_table.html"))


