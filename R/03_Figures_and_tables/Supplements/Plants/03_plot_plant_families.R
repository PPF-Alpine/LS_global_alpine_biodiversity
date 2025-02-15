
# libraries
library(here)
library(tidyverse)
library(purrr)
library(insight)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Load configuration
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# Load data
#----------------------------------------------------------#

# Load the latest files
proportional_richness_results <- RUtilpol::get_latest_file(
  "proportional_richness_results",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)

# Load species list
species_list <- RUtilpol::get_latest_file(
  "species_list",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)


# Load species list
gift_checklists <- RUtilpol::get_latest_file(
  "gift_checklists",
  dir = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants"),
  verbose = TRUE
)


# meta information about GIFT regions (for labelling)
gift_info <- readxl::read_xlsx(
  path = file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Plants/GIFT_info_table.xlsx")
)


#--------------------------------------------------------
# Filter Data
#--------------------------------------------------------

# 
geo_to_remove <- proportional_richness_results|>
  filter(condition == "alp_generalist" & richness < 7)|> 
  select(geo_entity)|> 
  distinct()

# Filter out geo_entities with low richness from proportional_richness_results
proportional_richness_results <- proportional_richness_results|>
  filter(!(geo_entity %in% geo_to_remove$geo_entity))

# Filter out geo_entities with low richness from species_list
species_list_filtered <- species_list|>
  filter(!(geo_entity %in% geo_to_remove$geo_entity))


species_list_with_taxonomy <- species_list_filtered|>
  left_join(
    gift_checklists$checklists|>
      select(work_species, family, tax_group)|>
      distinct(work_species, .keep_all = TRUE),  # Ensure one row per work_species
    by = "work_species"
  )


#--------------------------------------------------------
# Analyze plant families for generalists and specialists
#--------------------------------------------------------

# Generalists: Unique species per family and taxonomic group
unique_species_per_family_generalists <- species_list_with_taxonomy|>
  filter(max_elev > treeline_GIFT)|> 
  select(work_species, geo_entity, family, tax_group)|> 
  distinct()

unique_species_per_family_count_generalists <- unique_species_per_family_generalists|>
  group_by(family)|>                       # Group by family
  summarise(
    unique_species_count = n_distinct(work_species)  # Count unique species
  )|>
  arrange(desc(unique_species_count))|>
  drop_na()


unique_species_per_family_count_generalists <- unique_species_per_family_count_generalists|>
  mutate(proportion = unique_species_count / sum(unique_species_count),
         proportion_percent = proportion * 100)


unique_species_per_tax_count_generalists <- unique_species_per_family_generalists|>
  group_by(tax_group)|>                    # Group by taxonomic group
  summarise(
    unique_species_count = n_distinct(work_species)  # Count unique species
  )|>
  arrange(desc(unique_species_count))

# Specialists: Unique species per family and taxonomic group
unique_species_per_family_specialists <- species_list_with_taxonomy|>
  filter(max_elev > treeline_GIFT & min_elev > treeline_GIFT)|> 
  select(work_species, geo_entity, family, tax_group)|> 
  distinct()

unique_species_per_family_count_specialists <- unique_species_per_family_specialists|>
  group_by(family)|>                       # Group by family
  summarise(
    unique_species_count = n_distinct(work_species)  # Count unique species
  )|>
  arrange(desc(unique_species_count))

unique_species_per_tax_count_specialists <- unique_species_per_family_specialists|>
  group_by(tax_group)|>                    # Group by taxonomic group
  summarise(
    unique_species_count = n_distinct(work_species)  # Count unique species
  )|>
  arrange(desc(unique_species_count))

#--------------------------------------------------------
# plot the 100 most common families
#--------------------------------------------------------

# Select the top 101 families based on species count
top_101_families <- unique_species_per_family_count_generalists|>
  arrange(desc(proportion_percent))|>
  slice(1:101)


# Create the bar plot
fam <- ggplot(top_101_families, aes(x = reorder(family, +proportion_percent), 
                                    y = proportion_percent)) +
  geom_bar(stat = "identity", fill = "#8b8b05ff") +
  labs(
    y = "Proportion (%) of plant families with the most species"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 12),  # Axis labels size
    axis.text.y = element_text(size = 10),                                      # Plant family names size
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size=14),
    panel.background = element_blank(),      # Remove panel background
    plot.background = element_blank(),       # Remove plot background
    panel.grid.major = element_blank(),      # Remove major gridlines
    panel.grid.minor = element_blank()       # Remove minor gridlines
  ) +
  coord_flip()  #


output_file <- file.path(data_storage_path, 
                         "subm_global_alpine_biodiversity/Results/Figures_and_tables/Suppl/Plants/")

ggsave(filename = paste0(desktop_path, "plant_families.png"), plot = fam, width = 8, height = 11.5, dpi = 900) 

