

# Central Andes Reptiles (Stephan Halloy added lots of additional GBIF info, seperated it in supplementary file and info I need in the Reptile folder)


# Reptiles Northern Andes (jose Nicolas Urbina Cardona) did not edit in the file I sent --> need to be joined together

# read in my initial reptile file
Reptiles_Northern_Andes <- read_excel("~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists/Reptiles/all_lists/Reptiles_Northern_Andes.xlsx")

# read in nicolas' validated file
Reptiles_Northern_Andes_validated <- read_excel("~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists_validated/Reptiles/Suppl_Files/Suppl_Reptiles_Northern_Andes_JoseNicolas_UrbinaCardona.xlsx")|>janitor::clean_names()


df_cleaned <- left_join(Reptiles_Northern_Andes, Reptiles_Northern_Andes_validated, by = "sciname") |>
  mutate(mountain_range_corrected = ifelse(is.na(presence_in_colombian_andes_experts), "no", "yes")) |>
  rowwise() |>
  mutate(reviewer_comments = paste(
    na.omit(c(
      ifelse(presence_in_colombian_montane_forest_iucn != "", presence_in_colombian_montane_forest_iucn, NA),
      ifelse(endemic_to_colombia_to_colombia != "", endemic_to_colombia_to_colombia, NA),
      ifelse(gbif != "", gbif, NA)
    )),
    collapse = "; "
  )) |>
  ungroup() |>
  mutate(reviewer_comments = ifelse(reviewer_comments != "",
                                    paste(reviewer_comments, "; taxon ID:", taxon_id),
                                    paste("taxon ID:", taxon_id))) |>
  select(-presence_in_colombian_montane_forest_iucn, -endemic_to_colombia_to_colombia, -gbif,-taxon_id,-presence_in_colombian_andes_experts)


# 

writexl::write_xlsx(df_cleaned, "~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists_validated/Reptiles/Reptiles_Northern_Andes_JoseNicolas_UrbinaCardona.xlsx")
