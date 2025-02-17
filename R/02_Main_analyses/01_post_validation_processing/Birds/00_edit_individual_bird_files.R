

# 1. Brindusa Covaci added an additional document with birds to add 


# read in file they sent back
birds_file_val<- read_excel("~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists_validated/Birds/Suppl_Files/Birds_East_European_Highlands_BrindusaandMihai_Covaci.xlsx")


# read in additional file 
additional_species_val <- read_excel("~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists_validated/Birds/Suppl_Files/Additional_Birds_East_European_Highlands.xlsx")

# Identify scinames not in df
new_entries <- additional_species_val %>%
  filter(!sciname %in% birds_file_val$sciname)

# bind to validated birds file
df_cleaned <- bind_rows(birds_file_val, new_entries)



writexl::write_xlsx(df_cleaned, "~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists_validated/Birds/Birds_East_European_Highlands_BrindusaandMihai_Covaci.xlsx")


#2. Matteo Anderle edited in min max elevation column

# initial file I sent
old_file <- read_excel("~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists/Birds/Birds_Central_European_Highlands.xlsx")

# file he edited
birds_file_val <- read_excel("~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists_validated/Birds/Birds_Central_European_Highlands_Matteo_Anderle.xlsx")|>
  select(-min_corrected,-max_corrected)|>
  rename(min_corrected = min_elevation, max_corrected = max_elevation)|>
  left_join(old_file|> 
              select(sciname, min_elevation, max_elevation), by = "sciname")|>
  mutate(min_corrected = ifelse(min_corrected == min_elevation, NA, min_corrected),
         max_corrected = ifelse(max_corrected == max_elevation, NA, max_corrected))

writexl::write_xlsx(birds_file_val, "~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists_validated/Birds/Birds_Central_European_Highlands_Matteo_Anderle.xlsx")


#2. Daniel Cadena northern andes

old_file <- read_excel("~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists_validated/Birds/Birds_Northern_Andes_Daniel_Cadena.xlsx")


cleaned_file <- old_file %>%
  group_by(sciname) %>%                # Group by scientific name
  slice_max(order_by = min_elevation, with_ties = FALSE) %>%  # First, select row with highest min_elevation
  slice_max(order_by = max_elevation, with_ties = FALSE) %>%  # If there's a tie, select the one with the highest max_elevation
  ungroup()  

duplicate_count <- old_file %>%
  group_by(sciname) %>%
  filter(n() > 1) %>%
  summarise(count = n())

# Print the number of unique scinames that had duplicates
nrow(duplicate_count)

sum(duplicate_count$count)

writexl::write_xlsx(cleaned_file, "~/Desktop/Datasets/Biodiversity_combined/Expert_validation/Checklists_validated/Birds/Birds_Northern_Andes_Daniel_Cadena.xlsx")


