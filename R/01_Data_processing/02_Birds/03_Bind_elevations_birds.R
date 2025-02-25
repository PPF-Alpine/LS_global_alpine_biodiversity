#----------------------------------------------------------#
#         Bind Elevations to Species 
#----------------------------------------------------------#

# This script binds elevation data to species names sourced from Global database of birds (quintero and Jetz, 2018)

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(visdat)
library(tidyverse)
library(readxl)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2. Load data -----
#----------------------------------------------------------#

file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Checklist.xlsx")

# this binds the different sheets into one dataframe
Birds_Checklist <- readxl::excel_sheets(file_path) |>
  map_df(~read_excel_sheets(.x))

length(unique(Birds_Checklist$sciname))

#----------------------------------------------------------#
# 3. Load elevation data -----
#----------------------------------------------------------#
# Dataset by Quintero and Jetz
# mountain dataset has mountain IDs (need to be linked to elevations)
qu_j_mountain_data <- readxl::read_excel(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/processed/additional_data/Quintero_Jetz_Mountains.xlsx"))|>
  janitor::clean_names()

# this dataset has the elevations and mountain IDs
qu_j_elevations <- readxl::read_excel(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/processed/additional_data/Quintero_Jetz_elev_ranges_birds.xlsx"))|>
  janitor::clean_names()|> 
  left_join(qu_j_mountain_data |> # join the mountain names 
              select(mountain_range,mountain_id),by="mountain_id")|>
  select(-x7)|>
  rename(Mountain_range_Qu_J = mountain_range)|>
  rename(min_elevation = minimum_elevation)|>
  rename(max_elevation = maximum_elevation)|>
  rename(sciname = species)

#----------------------------------------------------------#
#  save data -----
#----------------------------------------------------------#

writexl::write_xlsx(qu_j_elevations,data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Elevations_Qu_J.xlsx")


#----------------------------------------------------------#
# check out data -----
#----------------------------------------------------------#

Elevation_data_Birds <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Elevations_Qu_J.xlsx"))|> 
  group_by(sciname) |>
  summarize(
    min_elevation = round(mean(min_elevation, na.rm = TRUE), 0),
    max_elevation = round(mean(max_elevation, na.rm = TRUE), 0))

#----------------------------------------------------------#
# investigate data availability --
#----------------------------------------------------------#

Birds_Elevations <- Birds_Checklist|> 
  left_join(Elevation_data_Birds,by = "sciname")|> 
  arrange(sciname)

GMBA_names_level_03 <- readRDS(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_names_level_03_04.rds"))|>
filter(Hier_Lvl =="3")|>
  group_by(Mountain_range) |>
  summarise(gmba_ID = first(gmba_ID), 
            Mountain_system = first(Mountain_system))


Birds_Elevations <- Birds_Elevations |>
  left_join(GMBA_names_level_03_unique, by = "Mountain_range")|>
  select(sciname, gmba_ID, Mountain_system, 
         Mountain_range, species_area, overlap_percentage_mountain, 
         overlap_percentage_alpine, min_elevation, max_elevation)



writexl::write_xlsx(Birds_Elevations,data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Checklist_Elevations_Qu_J.xlsx")

#----------------------------------------------------------#
# visualize missing data --
#----------------------------------------------------------#
vismis <- Birds_Elevations |>
  select(sciname,min_elevation)|>
  group_by(sciname)|>
  summarise(min_elevation = mean(min_elevation, na.rm = FALSE)) |>
  rename("Missing Elevation Data" = min_elevation)|>
  select("Missing Elevation Data")


# Using vis_miss to visualize missing data
x11()
vis_miss(vismis) +
  ggtitle("Birlife Data with elevations from Quintero Jetz") +
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none",
                                                      axis.ticks = element_blank())

