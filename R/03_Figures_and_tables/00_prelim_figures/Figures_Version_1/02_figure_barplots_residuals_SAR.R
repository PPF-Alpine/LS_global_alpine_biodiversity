

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


# run the barplot functions 
# TO DO:  clean and add to R/FUNCTIONS
source(
  here::here("R/02_Main_analyses/Biodiversity_combined/Post_expert_validation/Results_and_Figures/prep_files/00_residuals_plot_functions_barplots.R")
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
# Individual barplots coloured by residuals for specified mountain ranges and alpine categories
#-----------------------------------------------------------

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

#
test <- species_richness_sar_df |> 
  filter(Mountain_range=="Northern Andes")|> 
  filter(filter_condition=="generalists")

# Create the plot
bar_plot <- ggplot(test, aes(x = group, y = residuals_log, fill = residuals_log)) +
  geom_bar(stat = "identity", width = 0.8) +  # Increased width for less spacing
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +  # Adds a thin dashed line at y=0
  scale_y_continuous(limits = c(-1.55, 1.33)) +  # Set y-axis limits
  scale_fill_gradientn(colors = colors, limits = c(-1.55, 1.33)) +  # Adjusted color scale limits
  labs(title = paste(unique(test$Mountain_range)),
       x = NULL, y = NULL,
       fill = NULL) +
  theme(
    axis.text.x = element_text(size = 4),  # Smaller x-axis labels
    axis.text.y = element_text(size = 6),  # Smaller y-axis labels
    axis.title.x = element_blank(),        # Remove x-axis title
    axis.title.y = element_blank(),        # Remove y-axis title
    axis.ticks = element_line(size = 0.25),  # Thinner axis ticks
    legend.position = "none",
    plot.title = element_text(hjust = 0.1, size = 6)) 

plot(bar_plot)


#--------------------------------------------------------
# Function to loop through all mountains and alpine categories
#-----------------------------------------------------------

# all the plots are saved as svgs to onedrive folder (see function)

#
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

# plot bar plots for the different categories of alpine species
generate_bar_plots(species_richness_sar_df, "generalists")


