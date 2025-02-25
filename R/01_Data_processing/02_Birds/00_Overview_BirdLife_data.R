#------------------------------------------------------------------------------#
#  Source and check out data from BirdLife International
#-----------------------------------------------------------------------------#

# Data has been received from BirdLife International after formal application on the 09.05.2023 
# BirdLife International and Handbook of the Birds of the World (2022) Bird species distribution maps of the world. Version 2022.2. 
# Sourced from http://datazone.birdlife.org/species/requestdis.

#----------------------------------------------------------#
# 1. Set up and load the data  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(readxl)
library(sf)

# Load configuration file
source(here::here("R/00_Config_file.R"))

birds_data <- sf::st_read(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/BOTW.gdb"))

# set geometry false to investigate data a bit
birds_no_geom <- sf::st_set_geometry(birds_data, NULL)

#----------------------------------------------------------#
# Check out data  -----
#----------------------------------------------------------#

# get all the unique species names
unique_birds <- birds_no_geom |> 
  distinct(sci_name)

# filter birds with breeding range polygon
birds_breeding <- birds_no_geom |> 
  filter(seasonal==2)|> 
  distinct(sci_name)

birds_breeding_native <- birds_no_geom |> 
  filter(seasonal==2)|> 
  filter(origin==1)|> 
  filter(presence==1)|>
  distinct(sci_name)

# how many birds in BirdLife have a breeding range polygon
nrow(birds_breeding)/nrow(unique_birds)*100
# --> only 14 % 


# filter birds with breeding range polygon
birds_resident <- birds_no_geom |> 
  filter(seasonal==1)|> 
  distinct(sci_name)

# how many birds in BirdLife have a resident range polygon
nrow(birds_resident)/nrow(unique_birds)*100
# --> 94 % 

# I need to filter birds with breeding range, extant and native origin  
# filter birds with breeding range polygon
# some birds still have several ranges .. also from different sources 
# moved to Arcgis pro here to handle the dataset 
birds_resident_native <- birds_data |> 
  filter(seasonal==1)|> 
  filter(origin==1)|> 
  filter(presence==1)


birds_resident_native <- birds_no_geom |> 
  filter(seasonal==1)|> 
  filter(origin==1)|> 
  filter(presence==1)|>
  distinct(sci_name)


birds_breeding_native <- birds_no_geom |> 
  filter(seasonal==2)|> 
  filter(origin==1)|> 
  filter(presence==1)|>distinct(sci_name)



## Test 
aburria <- birds_data |> 
  filter(sci_name =="Aburria aburri")|> 
  filter(seasonal==1)|> 
  filter(origin==1)|> 
  filter(presence==1)

birds_shapes <- sf::st_read(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/birdlife.shp"))

birds_shapes <- make_shapes_valid(birds_shapes)
length(unique(birds_no_geom$sci_nam))


#--------------------------------------------------------------------#
# Writing multipolygon ranges -----
#--------------------------------------------------------------------#

unprocessed_species <- character()
# keep species names which cannot be processed (with errors)
# 16 of 10383 unique species cannot be processed

# Function to merge ranges if species has more than one range
process_species <- function(species_name, data) {
  tryCatch({
    species_data <- data|> 
      filter(sci_nam == species_name) |> 
      summarise(sci_nam = first(sci_nam), 
                geometry = sf::st_union(geometry), do_union = FALSE) |> # merge ranges into multipolygon
      st_cast("MULTIPOLYGON")
    return(species_data)
  }, error = function(e) {
    # On error, print the message and return NULL
    message("Error processing species: ", species_name, "\nError message: ", e$message)
    unprocessed_species <<- c(unprocessed_species, species_name)
    return(NULL)
  })
}

# Unique species names
species_names <- unique(birds_shapes$sci_nam)

# Process each species and combine results
results <- do.call(rbind, lapply(species_names, process_species, data = birds_shapes))
results_sf <- st_as_sf(results)

# data frame for unprocessed species 
unprocessed_species_df <- data.frame(sci_nam = unprocessed_species)

# set geom 0 to handle data more easily
results_no_geom<- sf::st_set_geometry(results_sf, NULL)

# keep metadata for species 
birds_metadata <- birds_no_geom|>
  inner_join(results_no_geom, by = "sci_nam") |>
  add_count(sci_nam, name = "occurrences") 


occurrence_distribution <- birds_metadata |>
  group_by(occurrences) |>
  summarise(species_count = n_distinct(sci_nam), .groups = 'drop')

# plot the number of polygons per species
x11()
ggplot(occurrence_distribution, aes(x = occurrences, y = species_count)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  geom_text(aes(label = species_count),
            position = position_dodge(width = 0.9), vjust = -0.25, check_overlap = TRUE) +
  labs(x = "count of range polygons per species",
       y = "count of Species (log scale)",
       title = "Counts of range polygons per species") +
  theme_minimal()

sp2 <- birds_metadata |> filter(occurrences==2)

sitpyg <- results_sf |> filter(sci_nam =="Geranoaetus albicaudatus")
x11()
plot(sitpyg$geometry)

#---------------------#
# Write Results ----
#----------------------#

# the merged shapefile
sf::st_write(results_sf,paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/birdlife_merge.shp"))

