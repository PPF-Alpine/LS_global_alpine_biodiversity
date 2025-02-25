# libraries
library(here)
library(tidyverse)
library(RUtilpol)
library(readxl)
library(gt)

# Load configuration
source(here::here("R/00_Config_file.R"))


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



proportional_richness_results <- proportional_richness_results|>
  left_join(gift_info|>
              select(geo_entity,region_ID),by="geo_entity")|>
  rename(GIFT_ID = region_ID)


proportional_richness_results <- proportional_richness_results|>
  select(GIFT_ID,
         geo_entity,
         condition,
         richness,
         residuals_log)



#----------------------------#
#   Reshape data 
#----------------------------#

reshaped_ric_table <- proportional_richness_results |>
  select(GIFT_ID,geo_entity,condition,richness,residuals_log) |>
  pivot_wider(
    names_from = condition,
    values_from = c(richness, residuals_log)
  )|>
  relocate(GIFT_ID, geo_entity, 
           richness_alp_generalist,residuals_log_alp_generalist,
           richness_degr_6,residuals_log_degr_6,
           richness_degr_4,residuals_log_degr_4,
           richness_degr_2,residuals_log_degr_2,
           richness_alp_specialist,residuals_log_alp_specialist) 



#----------------------------#
#   Make a nice table 
#----------------------------#

gt_table <- reshaped_ric_table|>
  arrange(GIFT_ID)|>
  gt() |>
  fmt_number(
    columns = c(residuals_log_alp_generalist, 
                residuals_log_alp_specialist,
                residuals_log_degr_6,
                residuals_log_degr_4,
                residuals_log_degr_2), # Specify longitude and latitude columns
    decimals = 2 # Retain 2 decimal places for these columns
  ) |>
  fmt_number(
    columns = c(richness_alp_generalist, 
                richness_alp_specialist,
                richness_degr_6,
                richness_degr_4,
                richness_degr_2), # Other numeric columns
    decimals = 0 # Round to 0 decimals for these columns
  ) |>
  cols_label(
    GIFT_ID = "Checklist regional ID",
    geo_entity = "GIFT region",
    richness_alp_generalist = "AR generalists",
    richness_alp_specialist = "AR specialists",
    residuals_log_alp_generalist = "RR generalists",
    residuals_log_alp_specialist ="RR specialists",
    richness_degr_2 = "AR UFL-alpine",
    residuals_log_degr_2 = "RR UFL-alpine",
    richness_degr_4 = "AR mid-montane",
    residuals_log_degr_4 = "RR mid-montane",
    richness_degr_6 = "AR broad-montane",
    residuals_log_degr_6 = "RR broad-montane"
  ) |>
  tab_style(
    style = cell_borders(sides = "left", color = "lightgrey", weight = px(0.1)), # Adds column borders
    locations = cells_body(columns = everything()) # Applies to all body columns
  ) |>
  tab_spanner(
    label = "Generalists", # Spanner for Generalists
    columns = c(richness_alp_generalist, residuals_log_alp_generalist)
  ) |>
  tab_spanner(
    label = "Specialists", # Spanner for Specialists
    columns = c(richness_alp_specialist, residuals_log_alp_specialist)
  ) |>
  tab_spanner(
    label = "UFL-alpine", # Spanner for UFL-alpine
    columns = c(richness_degr_2, residuals_log_degr_2)
  ) |>
  tab_spanner(
    label = "Mid-montane", # Spanner for mid-montane
    columns = c(richness_degr_4, residuals_log_degr_4)
  ) |>
  tab_spanner(
    label = "Broad-montane", # Spanner for broad-montane
    columns = c(richness_degr_6, residuals_log_degr_6)
  )


#
# Print the table
gt_table

#----------------------------------------------------------#
# save tables
#----------------------------------------------------------#

# Define the base output directory
output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Plants")

# Save the infor table
gtsave(gt_table, file.path(output_file, "plants_richness_table.html"))


