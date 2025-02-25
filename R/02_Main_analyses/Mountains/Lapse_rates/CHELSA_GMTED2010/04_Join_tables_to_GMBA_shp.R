


# Load configuration
source(
  here::here("R/00_Config_file.R")
)

# load gmba mountain shapes version 2
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)

  
# load lapse rates level 05 
lapse_rates_05 <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/Lapse_Rates/Lapse_Rates_GMBA_V2_Level_05.xlsx")) |>
  dplyr::select(Mountain_range_03, mean_level_03,median_level_03) |>
  distinct(Mountain_range_03, .keep_all = TRUE)|>
  rename(Mountain_range = Mountain_range_03)|>
  rename(lapse_rate_mean_05 = mean_level_03)|>
  rename(lapse_rate_median_05 = median_level_03)

# level 04
lapse_rates_04 <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/Lapse_Rates/Lapse_Rates_GMBA_V2_Level_04.xlsx"))|>
  group_by(Mountain_range_03)|>
  mutate(lapse_rate_median_04 = round(median(Lapse_rate),3))|>
  ungroup()|>
  dplyr::select(Mountain_range_03, Mountain_system, mean_level_03,lapse_rate_median_04) |>
  distinct(Mountain_range_03, .keep_all = TRUE)|>
  rename(Mountain_range = Mountain_range_03)|>
  rename(lapse_rate_mean_04 = mean_level_03)

# level 03 
lapse_rates_03 <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/Lapse_Rates/Lapse_Rates_GMBA_V2_Level_03.xlsx"))|>
  rename(lapse_rate_03 = Lapse_rate)

# combine the lapse rates 
comb_lapserate <- lapse_rates_03|> 
  left_join(lapse_rates_05,by="Mountain_range")|>
  left_join(lapse_rates_04,by="Mountain_range")

# join it to mountain dataframe 
mountain_shapes_join <- mountain_shapes|>
  left_join(comb_lapserate,by="Mountain_range")|>
  dplyr::select(GMBA_V2_ID,Level_01,Level_02,Mountain_range,Hier_Lvl,Area,
                lapse_rate_03,
                lapse_rate_mean_04,lapse_rate_median_04,
                lapse_rate_mean_05,lapse_rate_median_05,geometry)|>
  rename(Level_03=Mountain_range,lr_3=lapse_rate_03,lr_mn_4 =lapse_rate_mean_04,
         lr_md_4 =lapse_rate_median_04,
         lr_mn_5 =lapse_rate_mean_05,
         lr_md_5 =lapse_rate_median_05,geom=geometry)



mountain_df <- mountain_shapes_join |> 
  sf::st_set_geometry(NULL)

sf::st_write(mountain_shapes_join,paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/Lapse_rates/GMBA_lapse_rates.shp"))
