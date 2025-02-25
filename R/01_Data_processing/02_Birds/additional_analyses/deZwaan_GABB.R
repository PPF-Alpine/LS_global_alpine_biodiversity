#------------------------------------------------------------------------------#
#  Checkout scope and data availability Global Alpine Breeding Birds Dataset
#-----------------------------------------------------------------------------#

# GABB is a checklist for alpine (generalist, specialist, alpine tundra) birds breeding above the treeline
# Treeline elevation is defined seperately for each mountain range from literature (sources can also be found in the dataset)

#----------------------------------------------------------#
# 1. Set up and load the data  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(readxl)
library(stringdist)
library(fuzzyjoin)

# Load configuration file
source(here::here("R/00_Config_file.R"))

# Path to load the GABB dataset
file_path <- paste0(data_storage_path,"Birds/GABB/GABB_Dataset.xlsx")

# metadata 
metadata <- readxl::read_excel(file_path, sheet = "Metadata")|>
  janitor::clean_names()

# global dataset
# one row one species and only one trait expression for a species across mountain regions
glob_gabb <- readxl::read_excel(file_path, sheet = "Global alpine breeding birds") |>
  janitor::clean_names()|>
  pivot_longer(
    cols = north_america_northwestern_ranges:islands_indo_malayan, 
    names_to = "Mountain_range",
    values_to = "presence",
    values_drop_na = TRUE 
  )

# regional dataset
# alpine breeding categories and species traits are specific to each mountain region for a particular species
reg_gabb <- readxl::read_excel(file_path, sheet = "Regional alpine breeding birds")|>
  janitor::clean_names()|>
  pivot_longer(
    cols = north_america_northwestern_ranges:islands_indo_malayan, 
    names_to = "Mountain_range",
    values_to = "presence",
    values_drop_na = TRUE 
  )


# Data sources (includings sources for treeline elevations)
data_sources <- readxl::read_excel(file_path, sheet = "Data sources")|>janitor::clean_names()

#-----------------------------------------------------#
# 2. Which are the same mountain ranges used?   -----
#------------------------------------------------------#

mountains_gabb <- reg_gabb |> distinct(Mountain_range) |>
  mutate(Mountain_range = tolower(gsub("_", " ", Mountain_range)))

# Compare unique mountain ranges to reptiles/mammals
mountains_rep <- readxl::read_excel(paste0(data_storage_path, "Reptiles/Output/Checklist/Reptiles_Checklist_Elevations_DEM.xlsx")) |>
  distinct(Mountain_range)|>
  mutate(Mountain_range = tolower(Mountain_range))

# Try to match mountain ranges from GABB and Reptile/Mammal Checklist

threshold <- 0 

matches <- fuzzyjoin::fuzzy_join(mountains_gabb, mountains_rep, by = "Mountain_range", max_dist = threshold, method = "jw")

# Same mountains in both:
# hindu kush
# himalayas
# ethiopian highlands
# altai sayan ranges
# Tibetean plateau
# Southern Andes
# Central Andes 
# Northern Andes 
# Europe Caucasus

#-----------------------------------------------------#
# 3. Species Richness GABB-----
#------------------------------------------------------#

richness_gabb <- reg_gabb |>
  group_by(Mountain_range) |>
  summarise(species_richness = n_distinct(scientific_name))|>
  mutate(Mountain_range = reorder(Mountain_range, species_richness))


# Create a bar plot
x11()
ggplot(richness_gabb, aes(x = Mountain_range, y = species_richness, fill = species_richness)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mountain Range", y = "Species Richness", fill = "Richness",
       title = "Species Richness per Mountain Range",
       subtitle = "Global Alpine Breeding Birds, de Zwaan et al., 2022") +
  scale_fill_gradient(low = "blue", high = "red")


