#----------------------------------------------------------#
#         Source Mammal distribution Data from MDD
#----------------------------------------------------------#

# This script unpacks and opens zip folders containing gpkg for all mammals 
# zips can be downloaded via the Mammal Diversity Database: https://www.mammaldiversity.org/assets/data/MDD.zip

# There are some large files --> processing each order seperately

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)

# Load configuration file
source(here::here("R/00_Config_file.R"))

order_name <-"Afrosoricida"
#----------------------------------------------------------#
# 3. Enzip folder containing range shapefiles  -----
#----------------------------------------------------------#

# Define the zip folder
zip_folder <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/MDD_zips/MDD_", order_name, ".zip")

# List files in the zip folder and extract the name of the geopackage file
zip_contents <- unzip(zip_folder, list = TRUE)
gpkg_file <- zip_contents$Name[grepl("\\.gpkg$", zip_contents$Name)][1]

# Unzip and read the geopackage file as an sf object
unzip(zip_folder, files = gpkg_file, exdir = tempdir())
mammals <- sf::st_read(file.path(tempdir(), gpkg_file), quiet = TRUE)


# These are the names of all orders: 
all_order_names <- c(
  "Afrosoricida",
  "Artiodactyla",
  "Carnivora",
  "Chiroptera",
  "Cingulata",
  "Dasyuromorphia",
  "Dermoptera",
  "Didelphimorphia",
  "Diprotodontia",
  "Eulipotyphla",
  "Hyracoidea",
  "Lagomorpha",
  "Macroscelidea",
  "Microbiotheria",
  "Monotremata",
  #"Notoryctemorphia", no overlapping species
  "Paucituberculata",
  "Peramelemorphia",
  "Perissodactyla",
  "Pholidota",
  "Pilosa",
  "Primates",
  "Proboscidea",
  "Rodentia",
  "Scandentia",
  "Sirenia",
  "Tubulidentata")







