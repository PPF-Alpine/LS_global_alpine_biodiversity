

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


checklist <- get_latest_file("alpine_categories_list", 
                             dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"), 
                             verbose = TRUE)


treelines <- read_excel(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Treeline_Lapse_Rate_04_05.xlsx"))

# Load area size data and calculate log1p of area size
area_size <- read_excel(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/Alpine_Biome_Area_size.xlsx"))|>
  select(Mountain_range, area_size)|>
  mutate(log1p_area = log1p(area_size))

# Load checklist of dataframes
centroids <- RUtilpol::get_latest_file(
  "centroids_richness_sar",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Results/Data_results"),
  verbose = TRUE
)|>
  select(Mountain_range,longitude,latitude,lat_range)

#----------------------------#
#   check data
#----------------------------#
# get the generalists
generalists <- checklist$generalists

# get the distinct mountain ranges 

mountain_ranges <- generalists|>
  distinct(Mountain_range)|>
  drop_na()

# Find Mountain_ranges only in mountain_ranges
unique_to_mountain_ranges <- mountain_ranges %>%
  filter(!Mountain_range %in% treelines$Mountain_range)

# Find Mountain_ranges only in treelines
unique_to_treelines <- treelines %>%
  filter(!Mountain_range %in% mountain_ranges$Mountain_range)

# Combine results into a list
comparison_results <- list(
  Unique_to_mountain_ranges = unique_to_mountain_ranges,
  Unique_to_treelines = unique_to_treelines)

#----------------------------#
#    assign IDs
#----------------------------#

mountain_ranges <- mountain_ranges |>
  mutate(
    mountain_range_ID = dense_rank(Mountain_range)  # Rank alphabetically
  )  

#----------------------------#
#   reshape
#----------------------------#

mountain_ranges_join <- mountain_ranges %>%
  left_join(treelines %>% select(Mountain_range, Mean_elevation, Mean_temperature), by = "Mountain_range") %>%
  left_join(area_size, by = "Mountain_range") %>%
  left_join(centroids,by="Mountain_range")|>
  mutate(
    Mean_temperature = round(Mean_temperature, 2), # Round Mean_temperature to 2 decimals
    area_size_km2 = round(as.numeric(area_size), 2) # Convert area_size to numeric, then round to 2 decimals
  ) %>%
  select(-area_size, -log1p_area, -Mean_temperature)|> 
  relocate(mountain_range_ID, Mountain_range, Mean_elevation, area_size_km2,longitude,latitude,lat_range) 

#----------------------------#
#   make a nice table 
#----------------------------#
gt_table <- mountain_ranges_join|>
  arrange(mountain_range_ID)|>
  gt() |>
  fmt_number(
    columns = c(longitude, latitude,lat_range), # Specify longitude and latitude columns
    decimals = 1 # Retain 2 decimal places for these columns
  ) |>
  fmt_number(
    columns = c(mountain_range_ID, area_size_km2, Mean_elevation), # Other numeric columns
    decimals = 0 # Round to 0 decimals for these columns
  ) |>
  cols_label(
    mountain_range_ID = "Mountain range ID",
    Mountain_range = "Mountain range",
    area_size_km2 = "Alpine area (km2)",
    Mean_elevation = "Mean elevation UFL",
    longitude = "centroid longitude",
    latitude ="centroid latitude",
    lat_range = "latitudinal range"
  ) |>
  tab_style(
    style = cell_borders(sides = "left", color = "lightgrey", weight = px(0.1)), # Adds column borders
    locations = cells_body(columns = everything()) # Applies to all body columns
  )
# Print the table
gt_table

gtsave(gt_table,paste0(data_storage_path,"subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Mountains/mountain_info_table.html"))
writexl::write_xlsx(mountain_ranges_join,paste0(data_storage_path,"subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Mountains//mountain_info_table.xlsx"))
