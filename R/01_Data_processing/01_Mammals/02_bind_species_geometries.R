
#----------------------------------------------------------#
#         Bind Species Geometries
#----------------------------------------------------------#

# After 02_Overlap_Mountain ranges all species occuring in mountain ranges (overlap > 1% with GMBA) are saved as rds 
# This script creates a spatial df to combine the geometries for unique species which is needed for the next step: extracting elevational ranges

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(data.table)
library(sf)

# these are all the order names
order_names <- c(
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
  "Tubulidentata"
)


#----------------------------------------------------------#
# 2. Loop through the orders to get the geometry -----
#----------------------------------------------------------#

mammal_geometries_list <- list()

for (order_name in order_names) {
  
  # Load the latest mammal file
  mammals <- RUtilpol::get_latest_file(
    file_name = order_name,
    dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/geom"))
  
  # Convert your dataframe to a data.table
  as.data.table(mammals)
  
  # Select the 'geom' and 'sciname' columns for unique 'sciname'
  mammals_geom <- mammals[, .(geom = first(geom)), by = sciname]
  
  # Store result in the list
  mammal_geometries_list[[order_name]] <- mammals_geom
  
  cat("Processing for order", order_name, "completed!\n")
}


mammal_geometries <- rbindlist(mammal_geometries_list, idcol = "order")

#----------------------------------------------------------#
# 3. Save the data -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save =mammal_geometries,
  dir = paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/geom"),
  prefered_format = "rds",
  use_sha = TRUE) 




# ------------- For individual orders or individual species -------------# 

# Define the order name 
order_name <- "Lagomorpha" # Replace with the order you want to read

# Load the latest mammal file
mammals<-RUtilpol::get_latest_file(
  file_name = paste0(order_name),
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/geom"))

# Convert your dataframe to a data.table
mammals <- as.data.table(mammals)

# Select the 'geom' and 'sciname' columns for unique 'sciname'
order_geom <- mammals[, .(geom = first(geom)), by = sciname]
#marmota_geom <- mammals_dt[sciname == "Marmota marmota", .(geom = first(geom)), by = sciname]
#sf::st_write(marmota_geom,paste0(data_storage_path,"/Mammals/marmota.shp"))
