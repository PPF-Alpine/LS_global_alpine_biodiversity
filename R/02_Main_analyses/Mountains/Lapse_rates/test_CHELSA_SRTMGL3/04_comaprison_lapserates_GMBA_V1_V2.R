
# In this script I join and compare lapse rates calculate bz Elsen et al using WorldClim temp and GMBA Version 1 and lapse rates calculated by me (see script calculate lapse rates all mountains) using CHELSA and GMBA Version 2

library(tidyverse)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2. Load data ---
#----------------------------------------------------------#

lapse_rate_V2 <- readxl::read_excel(paste0(data_storage_path, "Mountains/Lapse_Rates/Lapse_Rates_GMBA_V2.xlsx")) 
lapse_rate_V1 <- readxl::read_excel(paste0(data_storage_path, "Mountains/Lapse_Rates/Lapse_Rates_GMBA_V1_Elsenetal.xlsx"))
GMBA_names <- readxl::read_excel(paste0(data_storage_path, "Mountains/Lapse_Rates/GMBA_v2_to_v1.xls"))

lapse_rate_V1<- lapse_rate_V1|>
  dplyr::select(Mountain_range_V1,lapse_rate)|> 
  rename(lapse_rate_V1 = lapse_rate)

lapse_rate_V2 <- lapse_rate_V2 |> 
  rename(Mountain_range_V2 = Mountain_range)|>
  rename(lapse_rate_V2=Lapse_rate)

lapse_rate_V1$lapse_rate_V1 <- as.numeric(lapse_rate_V1$lapse_rate_V1)
lapse_rate_V2$lapse_rate_V2 <- as.numeric(lapse_rate_V2$lapse_rate_V2)

#----------------------------------------------------------#
# Join data together ---
#----------------------------------------------------------#
joined_df <- GMBA_names |>
  left_join(lapse_rate_V1, by = "Mountain_range_V1") |>
  # Now join the result with lapse_rate_V2
  left_join(lapse_rate_V2, by = "Mountain_range_V2")|>
  select(Mountain_system,Mountain_range_V1,Mountain_range_V2,lapse_rate_V1,lapse_rate_V2)|>mutate(
    lapse_rate_V1 = round(lapse_rate_V1, 1),
    lapse_rate_V2 = round(lapse_rate_V2, 1)
  )|>
  mutate(lapse_rate_difference = lapse_rate_V1 - lapse_rate_V2)


check_and_write_xlsx(joined_df,data_storage_path, "Mountains/Lapse_Rates/Lapse_Rates_GMBA_v1_v2.xlsx")

#----------------------------------------------------------#
# Plot the differnces ---
#----------------------------------------------------------#

plot_df <- joined_df|> filter(Mountain_system=="Andes"|
                                Mountain_system=="Europe"|
                                Mountain_system=="East African Highlands"|
                                Mountain_system=="Central Asia")


plot_ly(plot_df, 
        x = ~Mountain_range_V2, 
        y = ~lapse_rate_difference, 
        type = 'scatter', 
        mode = 'markers', 
        hoverinfo = 'text',
        text = ~paste('Mountain Range V1: ', Mountain_range_V1, 
                      '<br>Lapse Rate V1: ', lapse_rate_V1, 
                      '<br>Mountain Range V2: ', Mountain_range_V2, 
                      '<br>Lapse Rate V2: ', lapse_rate_V2)) %>%
  layout(title = 'Lapse Rate Differens GMBA Mountains Version 1 vs Version 2', 
         xaxis = list(title = 'Mountain Range V2'), 
         yaxis = list(title = 'Lapse Rate Difference'))

