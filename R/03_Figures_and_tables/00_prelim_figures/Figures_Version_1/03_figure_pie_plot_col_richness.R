

# TO DO's: 
# 1. clean function and write to R/FUNCTIONS
# 2. check order of groups in pies for different alpine categories (!)



#--------------------------------------------------------
# Set up and load data
#-----------------------------------------------------------

# no need to run config file for this script

# Load configuration
# source(here::here("R/00_Config_file.R"))

library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gridExtra)
library(svglite)


# load the data either by running 
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Results_and_Figures/01_calculate_richness_and_SAR.R")
)

# or load file
# species_richness_sar_df <- readxl::read_xlsx(paste0(data_storage_path,"/Biodiversity_combined/Final_lists/alpine_biodiversity_database.xlsx"))

# run the pie plot function --> TO DO:  clean and add to R/FUNCTIONS
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Results_and_Figures/prep_files/00_richness_plot_functions_pies.R")
)



# species_richness_sar_df is the dataframe to work with

#--------------------------------------------------------
# highlight validated mountain ranges
#-----------------------------------------------------------

# add a column with how many groups are expert validated per mountain range
species_richness_sar_df <- species_richness_sar_df|>
  left_join(expert_validated, by = c("Mountain_range", "group"))


validated_counts <- species_richness_sar_df|>
  select(Mountain_range,group,expert_validated) |>
  distinct()|>
  group_by(Mountain_range)|>
  mutate(validated_count = sum(expert_validated == "yes"))

# 
species_richness_sar_df <- species_richness_sar_df |>
  left_join(validated_counts, by = c("Mountain_range", "group","expert_validated"))


#--------------------------------------------------------
# Plot individual pie plots for certain mountain range coloured by richness
#-----------------------------------------------------------

test <- species_richness_sar_df |> 
  filter(Mountain_range=="Northern Andes")|>
  filter(filter_condition=="generalists")

# Define the colors for each group
colors <- c("#1A2A5A", 
            "#256C85", 
            "#37AF91", 
            "#44BF2F", 
            "#A3970E", 
            "#F2C12E",
            "#F2994A", 
            "#D8461B", 
            "#AD2E24", 
            "#4E1703")

# Create the pie chart
pie_chart <- ggplot(test, aes(x = "", y = richness_group, fill = richness_group_log)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  geom_text(aes(label = group, y = richness_group ), position = position_stack(vjust = 0.6), 
            color = "white", size = 4, angle = 0, hjust = 0.8) +
  theme_void()+
  scale_fill_gradientn(colors = colors, limits = c(0, 6)) +
  labs(title = paste("Species Richness", unique(test$Mountain_range)),
       x = NULL, y = NULL,
       fill = NULL) 

x11()
plot(pie_chart)



#--------------------------------------------------------
# Function to loop through all mountains and alpine categories
#-----------------------------------------------------------

# all pie plots are saved in 

colors <- c("#1A2A5A", 
            "#256C85", 
            "#37AF91", 
            "#44BF2F", 
            "#A3970E", 
            "#F2C12E", 
            "#F2994A", 
            "#D8461B", 
            "#AD2E24", 
            "#4E1703")




# These are the different categories to plot
# "generalists", "specialists", "degree_2", "degree_4", "degree_6"


# apply function to create pie charts
generate_pie_charts(species_richness_sar_df, "degree_4")









#-------------------------------------DONT RUN BELOW HERE --------------------------------------------------------------------------





#--------------------------------------------------------
# Plot individual mountain range coloured by richness
#-----------------------------------------------------------

test <- species_richness_sar_df |> 
  filter(Mountain_range=="Northern Andes")|>
  filter(filter_condition=="generalists")

# Define the colors for each group
colors <- c("#1A2A5A", 
            "#256C85", 
            "#37AF91", 
            "#44BF2F", 
            "#A3970E", 
            "#F2C12E",
            "#F2994A", 
            "#D8461B", 
            "#AD2E24", 
            "#4E1703")

# Create the pie chart
pie_chart <- ggplot(test, aes(x = "", y = richness_group, fill = richness_group_log)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  geom_text(aes(label = group, y = richness_group ), position = position_stack(vjust = 0.6), 
            color = "white", size = 4, angle = 0, hjust = 0.8) +
  theme_void()+
  scale_fill_gradientn(colors = colors, limits = c(0, 6)) +
  labs(title = paste("Species Richness", unique(test$Mountain_range)),
       x = NULL, y = NULL,
       fill = NULL) 

x11()
plot(pie_chart)



#--------------------------------------------------------
# Loop through all mountains and save as single svgs
#-----------------------------------------------------------

unique(species_richness_sar_df$filter_condition)

for (mountain_range in unique(species_richness_sar_df$Mountain_range)) {
  # Filter data for the current mountain range
  test <- species_richness_sar_df %>%
    filter(Mountain_range == mountain_range)
  
  # Create the pie chart with specified colors and other settings
  pie_chart <- ggplot(test, aes(x = "", y = richness_group, fill = richness_group_log)) +
    geom_bar(stat = "identity", width = 1) + 
    coord_polar(theta = "y") +
    geom_text(aes(label = group, y = richness_group ), position = position_stack(vjust = 0.6), 
              color = "white", size = 4, angle = 0, hjust = 0.8) +
    theme_void()+
    scale_fill_gradientn(colors = my_colors_ric, limits = c(0, 6)) +
    labs(title = paste("Species Richness", unique(test$Mountain_range)),
         x = NULL, y = NULL,
         fill = NULL)+
    labs(title = paste(unique(test$Mountain_range)),
         x = NULL, y = NULL,
         fill = NULL) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.1, size = 6))
  
  # Setup filename using sanitized mountain range name
  safe_filename <- gsub("[/ ]", "_", mountain_range)  # Replace slashes and spaces with underscores
  
  # Define the filename with path
  filename <- paste0("~/Desktop/Datasets/Biodiversity_combined/Visuals/Visuals_Manuscript/Pie_Charts/col_richness/species_richness_", safe_filename, ".svg")
  
  # Open the SVG device with specified settings
  svg(filename, width = 0.8, height = 0.8, bg = "transparent")
  
  # Print the plot to the SVG device
  print(pie_chart)
  
  # Close the SVG device to finalize the file
  dev.off()
}

