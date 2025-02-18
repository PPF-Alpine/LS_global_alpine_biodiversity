
# in this script I calculate elevation change for each mountain range using the lapse rate (see script Join lapse rates GMBA v1- v2 and lapse rates all mountains)
# Elevation change = Delta Temp / Lapse rate x 1000

library(tidyverse)
library(plotly)
library(ggplot2)
# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2. Load and join data ---
#----------------------------------------------------------#

lapse_rates <- readxl::read_excel(paste0(data_storage_path, "Mountains/Lapse_Rates/Lapse_Rates_GMBA_v1_v2.xlsx")) |>
  select(Mountain_range_V2, lapse_rate_V2) |>
  distinct()

  
treeline <-  readxl::read_excel("~/Desktop/Datasets/Mountains/Tree_Line/Treeline_Suzette.xls")|> # mutate treeline - elevation change for 2 degree 
  mutate(Mean_elevation = round(Mean_elevation),0)

tl_lapserate <- treeline|> 
  left_join(lapse_rates, by = c("Mountain_range" = "Mountain_range_V2"))|>
  rename(lapse_rate = lapse_rate_V2)


#-------------------------------------------------------------------------------#
# Calculate elevation change for different temperature thresholds (Delta T) ---
#-------------------------------------------------------------------------------#

# How does a temp difference of x Degree translate in Elevation Meters in each Mountain range 

# Elevation Change = Delta Temp/Lapse rate x 1000

elev_change <- tl_lapserate|>
  mutate(elev_change_2_degree = round((-2/lapse_rate)*1000),1)|> # mutate elevation change with 2 degree
  mutate(Mean_elevation_2_degree = Mean_elevation - elev_change_2_degree)|> # mutate treeline - elevation change for 2 degree 
  mutate(elev_change_1_degree = round((-1/lapse_rate)*1000),1)|>
  mutate(Mean_elevation_1_degree = Mean_elevation - elev_change_1_degree)|>
  mutate(elev_change_4_degree = round((-4/lapse_rate)*1000),1)|>
  mutate(Mean_elevation_4_degree = Mean_elevation - elev_change_4_degree)|>
  mutate(elev_change_6_degree = round((-6/lapse_rate)*1000),1)|>
  mutate(Mean_elevation_6_degree = Mean_elevation - elev_change_6_degree)|>
  mutate(elev_change_8_degree = round((-8/lapse_rate)*1000),1)|>
  mutate(Mean_elevation_8_degree = Mean_elevation - elev_change_8_degree)|>
  drop_na()|>
  select(-"1")|>
  select(-"0")|>
  arrange(Mean_elevation)

check_and_write_xlsx(elev_change, data_storage_path, "Mountains/Tree_Line/Treeline_Lapse_Rate_Suzette.xlsx")


p<-plot_ly(elev_change, 
        x = ~Mountain_range, 
        y = ~Mean_elevation, 
        type = 'scatter', 
        mode = 'markers', 
        marker = list(color = 'black', size = 10), # Adjusted color and size
        hoverinfo = 'text',
        text = ~paste('Mountain Range: ', Mountain_range,'<br>Mean elevation TL: ', Mean_elevation,'Lapse Rate: ', lapse_rate),
        name = 'Mean Elevation Treeline (Suzette)') %>%
  add_trace(y = ~Mean_elevation_2_degree, 
            mode = 'markers', 
            marker = list(color = 'lightgreen', size = 10), # Adjusted color and size
            hoverinfo = 'text',
            text = ~paste('Mountain Range: ', Mountain_range, '<br>Elev Change 2 Degree: ', elev_change_2_degree),
            name = 'Elevation TL 2 Degree') %>%
  add_trace(y = ~Mean_elevation_1_degree, 
            mode = 'markers', 
            marker = list(color = 'darkgreen', size = 10), # Adjusted color and size
            hoverinfo = 'text',
            text = ~paste('Mountain Range: ', Mountain_range, '<br>Elev Change 1 Degree: ', elev_change_1_degree),
            name = 'Elevation TL 1 Degree') %>%
  add_trace(y = ~Mean_elevation_6_degree, 
            mode = 'markers', 
            marker = list(color = 'gold', size = 10), # Adjusted color and size
            hoverinfo = 'text',
            text = ~paste('Mountain Range: ', Mountain_range, '<br>Elev Change 6 Degree: ', elev_change_6_degree),
            name = 'Elevation TL 6 Degree') %>%
  add_trace(y = ~Mean_elevation_8_degree, 
            mode = 'markers', 
            marker = list(color = 'darkred', size = 10), # Adjusted color and size
            hoverinfo = 'text',
            text = ~paste('Mountain Range: ', Mountain_range, '<br>Elev Change 8 Degree: ', elev_change_8_degree),
            name = 'Elevation TL 8 Degree') %>%
  layout(title = 'Elevational change Treeline = Delta Temp/Lapse rate*1000', 
         xaxis = list(title = 'Mountain Range'),
         yaxis = list(title = 'Elevation Treeline (m)'))

p

htmlwidgets::saveWidget(p, "~/Desktop/Datasets/Mountains/Elev_Change_with_Temp.html", selfcontained = TRUE)


# create a line plot out of this: 
# Order the data frame by Mean_elevation
elev_change <- elev_change %>%
  arrange(Mean_elevation)

# Convert Mountain_range to an ordered factor based on Mean_elevation
elev_change$Mountain_range <- factor(elev_change$Mountain_range, levels = unique(elev_change$Mountain_range))

elev_change_long <- elev_change |>
  select(Mountain_range, 
         Mean_elevation, 
         Mean_elevation_1_degree, 
         Mean_elevation_2_degree, 
         Mean_elevation_6_degree, Mean_elevation_8_degree,lapse_rate,elev_change_1_degree)|>
  gather(key = "Condition", 
         value = "Elevation", 
         -Mountain_range, -lapse_rate,-elev_change_1_degree)

# Create the interactive plot 
p <- plot_ly(data = elev_change_long, 
             x = ~Mountain_range, 
             y = ~Elevation, 
             type = 'scatter', 
             mode = 'lines+markers',
             hoverinfo = 'text', 
             text = ~paste('Mountain Range: ', 
                           Mountain_range, 
                           '<br>Elevation: ', Elevation,
                           'm<br>Elev change 1: ', elev_change_1_degree,
                           'm<br>Lapse Rate: ', lapse_rate),
             color = ~Condition) %>%
  layout(title = 'Elevation Change with temperature Across Mountain Ranges',
         xaxis = list(title = 'Mountain Range'),
         yaxis = list(title = 'Elevation (m)'))
p

htmlwidgets::saveWidget(p, "~/Desktop/Datasets/Mountains/Lapse_Rates/Elev_Change_with_Temp_line_plot.html", selfcontained = TRUE)



