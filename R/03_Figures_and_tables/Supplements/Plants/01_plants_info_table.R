
#--------------------------------------------------------
# Set up 
#--------------------------------------------------------

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


gift_data <- alpine_biome_richness|>
  select(geo_entity,alpine_area,treeline_GIFT,longitude,latitude)

#----------------------------------------------------------#
# Get GIFT references
#----------------------------------------------------------#

api_rst <-"https://giftuser:Hepaticanobilis*@gift.uni-goettingen.de/api/restricted/"

# get the references 
gift_references<-GIFT::GIFT_references(api=api_rst,GIFT_version = "beta")
gift_regions <- GIFT::GIFT_regions(api=api_rst,GIFT_version = "beta")


# rename and standardize naming
gift_regions <- gift_regions %>%
  mutate(geo_entity_standardized = str_replace_all(geo_entity, "_", " "))

gift_data <- gift_data %>%
  mutate(
    geo_entity_standardized = str_replace_all(geo_entity, "_", " "), # Replace underscores with spaces
    geo_entity_standardized = str_replace(geo_entity_standardized, "Shaanxi China", "Shaanxi"),
    geo_entity_standardized = str_replace(geo_entity_standardized, "Utah United States", "Utah"), # Rename Shaanxi China
    geo_entity_standardized = str_replace(geo_entity_standardized, "Shaanxi China", "Shaanxi") # Rename Shaanxi China
  )


# join dfs to get gift IDs
joined_GIFT <- gift_data|>
  left_join(gift_regions|>select(entity_ID,geo_entity_standardized),by="geo_entity_standardized") |>
  mutate(
    region_ID = dense_rank(geo_entity)  # Rank alphabetically
  )  


# get the references
gift_lists <- GIFT::GIFT_lists(api=api_rst,GIFT_version = "beta")

# join the IDs and references
joined_GIFT_ref <- joined_GIFT|>
  left_join(gift_lists|>select(ref_ID,entity_ID),by="entity_ID")|>
  left_join(gift_references|>select(ref_ID,ref_long,geo_entity_ref),by="ref_ID")|>
  relocate(region_ID,geo_entity,alpine_area,treeline_GIFT,longitude,latitude,entity_ID,ref_ID,geo_entity_ref,ref_long)|>
  select(-geo_entity_standardized)|>
  arrange(region_ID)


joined_GIFT_ref_filtered <- joined_GIFT_ref %>%
  filter(geo_entity_ref != "World" | is.na(ref_long))


#----------------------------------------------------------#
# make the table
#----------------------------------------------------------#

# 
gt_table <- joined_GIFT_ref_filtered|>
  arrange(region_ID)|>
  gt() |>
  fmt_number(
    columns = c(longitude, latitude), # Specify longitude and latitude columns
    decimals = 1 # Retain 2 decimal places for these columns
  ) |>
  fmt_number(
    columns = c(region_ID,geo_entity,alpine_area,treeline_GIFT,entity_ID,ref_ID,geo_entity_ref,ref_long), # Other numeric columns
    decimals = 0 # Round to 0 decimals for these columns
  ) |>
  cols_label(
    region_ID = "Checklist regional ID",
    geo_entity = "GIFT region",
    alpine_area = "Alpine area (km2)",
    treeline_GIFT = "Mean elevation UFL in GIFT region",
    longitude = "centroid longitude",
    latitude ="centroid latitude",
    entity_ID = "GIFT entity ID",
    ref_ID = "GIFT ref ID",
    geo_entity_ref = "GIFT entity ref",
    ref_long = "full GIFT reference"
  ) |>
  tab_style(
    style = cell_borders(sides = "left", color = "lightgrey", weight = px(0.1)), # Adds column borders
    locations = cells_body(columns = everything()) # Applies to all body columns
  )
# Print the table
gt_table

#----------------------------------------------------------#
# save tables
#----------------------------------------------------------#

# Define the base output directory
output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Plants")

# Save the infor table
gtsave(gt_table, file.path(output_file, "GIFT_info_table.html"))

# and as xlsx
writexl::write_xlsx(joined_GIFT, file.path(output_file, "GIFT_info_table.xlsx"))

# Save references
writexl::write_xlsx(joined_GIFT_ref_filtered, file.path(output_file, "GIFT_info_table_REF.xlsx"))

