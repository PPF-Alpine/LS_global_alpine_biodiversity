#------------------------------------------------------------------------------#
#  Checkout scope and data availability elevational range dataset by Quintero and Jetz
#-----------------------------------------------------------------------------#

# regional elevational range data for birds

#----------------------------------------------------------#
# 1. Set up and load the data  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(readxl)
library(visdat)

# Load configuration file
source(here::here("R/00_Config_file.R"))

# mountain dataset has mountain IDs (need to be linked to elevations)
qu_j_mountain_data <- readxl::read_excel(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/processed/additional_data/Quintero_Jetz/Quintero_Jetz_Mountains.xlsx"))|>
  janitor::clean_names()

# this dataset has the elevations and mountain IDs
qu_j_elevations <- readxl::read_excel(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/processed/additional_data/Quintero_Jetz_elev_ranges_birds.xlsx"))|>
  janitor::clean_names()|> 
  left_join(qu_j_mountain_data |> # join the mountain names 
              select(mountain_range,mountain_id),by="mountain_id")


#----------------------------------------------------------#
# 2. Unit of analysis ? Which are the mountain ranges ?  -----
#----------------------------------------------------------#

# They use GMBA but version 1.2 
# Körner, C. et al. A global inventory of mountains for bio-geographical applications. Alp. Bot. 127, 1–15 (2016).

mountains_qu_j <- qu_j_elevations |>  
  distinct(mountain_range)

richness_qu_j <- qu_j_elevations |>
  group_by(mountain_range) |>
  summarise(species_richness = n_distinct(species))|>
  mutate(mountain_range = reorder(mountain_range, species_richness))


# Create a bar plot
x11()
ggplot(richness_qu_j, aes(x = mountain_range, y = species_richness, fill = species_richness)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mountain Range", y = "Species Richness", fill = "Richness",
       title = "Species Richness per Mountain Range",
       subtitle = "Global elevational diversity and diversification of birds, Quintero and Jetz") +
  scale_fill_gradient(low = "darkgreen", high = "yellow")

#-----------------------------------------------------------------------------------------------#
# 3.Jooin elev info from Quintero and Jetz with global alpine breeding birds dataset  -----
#--------------------------------------------------------------------------------------------#

# read dataset for global alpine breeeding birds 
reg_gabb <- readxl::read_excel(paste0(data_storage_path,"Birds/GABB/GABB_Dataset.xlsx"), 
                               sheet = "Regional alpine breeding birds")|>
  janitor::clean_names()|>
  pivot_longer(
    cols = north_america_northwestern_ranges:islands_indo_malayan, 
    names_to = "mountain_range",
    values_to = "presence",
    values_drop_na = TRUE 
  )|> 
  rename(mountain_range_gabb = mountain_range)


qu_j_elevations <- qu_j_elevations |> 
  rename(mountain_range_qu_j = mountain_range)


reg_gabb_elevations <- reg_gabb |>
  left_join(
    qu_j_elevations |> select(species,minimum_elevation, maximum_elevation, mountain_range_qu_j,country),
    by = c("scientific_name" = "species"))


reg_gabb_elevations |>
  filter(!is.na(country)) |>
  filter(!country=="NA") |>
  distinct(mountain_range_qu_j)

# For Mountain range Andes they separate between different countries

#--------------------------------------------------------#
# 4.check and visualize missing data for elevational info  -----
#-----------------------------------------------------------#

x11()
visdat::vis_miss(reg_gabb_elevations[c("minimum_elevation", "maximum_elevation")])
length(unique(reg_gabb_elevations$scientific_name))

# 96.5% of the birds in the global alpine breeding bird dataset have elevational info from the Quintero and Jetz dataset

# I suspect many 0 as min elevation --> visualize the distribution of minimum and maximum elevation 
x11()
ggplot(reg_gabb_elevations) +
  geom_density(aes(x = minimum_elevation), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = maximum_elevation), fill = "red", alpha = 0.5) +
  labs(title = "Density of Minimum and Maximum Elevation - Birds",
       x = "Elevation (m)",
       y = "Density",
       fill = "Elevation Type") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

# on log scale 
x11()
ggplot(reg_gabb_elevations) +
  geom_density(aes(x = log1p(minimum_elevation)), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = log1p(maximum_elevation)), fill = "red", alpha = 0.5) +
  labs(title = "Density of Minimum and Maximum Elevation",
       x = "Elevation (m)",
       y = "Density",
       fill = "Elevation Type") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

x11()
ggplot() +
  geom_histogram(data = reg_gabb_elevations, aes(x = minimum_elevation), 
                 fill = "blue", alpha = 0.5, bins = 100) +
  geom_histogram(data = reg_gabb_elevations, aes(x = maximum_elevation), 
                 fill = "red", alpha = 0.5, bins = 100) +
  labs(title = "Distribution Birds Minimum and Maximum Elevation",
       x = "Elevation",
       y = "Count",
       fill = "Elevation Type") +
  scale_fill_manual(values = c("minimum_elevation" = "blue", "maximum_elevation" = "red")) +
  theme_minimal()

