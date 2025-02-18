
#------------------------------------#
#   Check out Animal Diversity Web 
#-----------------------------------#

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
install.packages("rvest")
library(rvest)
library(janitor)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#-------------------------------------------------#
# 2. Query and Download Mammal Data from ADW -----
#-------------------------------------------------#

#https://animaldiversity.ummz.umich.edu/quaardvark/

# Queried for alpine mammals and for habitat column mountains
# Results in two seperate tables 

#-------------------------------------------------#
# 3.Read tables and join them to one -----
#-------------------------------------------------#

alpine_mammals <- readxl::read_excel(paste0(data_storage_path,"Mammals/ADW_Alpine_0410.xls")) |> janitor::clean_names()

mountain_mammals <- readxl::read_excel(paste0(data_storage_path,"Mammals/ADW_Mountains_0410.xls")) |> janitor::clean_names()

# Add an 'alpine' column to alpine_mammals 
alpine_mammals <- alpine_mammals %>%
  mutate(alpine = "yes")

# Join with mountain_mammals
joined <- mountain_mammals %>%
  left_join(select(alpine_mammals, species, alpine), by = "species")

