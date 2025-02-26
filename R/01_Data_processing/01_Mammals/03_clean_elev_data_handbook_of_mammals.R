
#--------------------------------------------------------------#
#        Clean the data from Handbook of Mammals 
#--------------------------------------------------------------#

# Physical copies of Handbook of the Mammals of the World available at
# https://github.com/jhpoelen/hmw

# This script cleans textual information from the handbook of mammals to min and max elevational ranges for mammals. 

# ❗ ATTENTION !! the functions below do not clean HMW completely. there are still elevationa data that can not be grasped by the functions
# after running this script the cleaning of the output file has been finalized manually


#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(stringr)
library(tidyverse)
library(purrr)
library(readxl)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)


#------------------------#
# 2. Download the data
#-------------------------#

# Loop to read each CSV file (for single files) 
#for (i in 1:9) {
#assign(paste0("hmw_v", i), 
#      read.csv(paste0("https://raw.githubusercontent.com/jhpoelen/hmw/main/hmw-volume-", i, ".csv")))
#}

# This is the single files combined
url <- "https://raw.githubusercontent.com/jhpoelen/hmw/main/hmw.csv"
hmw_data <- read.csv(url)

# Load The checklist
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/Mammals_prelim_checklist.xlsx")


# bind sheets into one dataframe
MDD_checklist <- excel_sheets(file_path) |>
  map_df(~process_sheet(.x))


#------------------------------------------------------------------#
# 3. Filter for species in the checklist which are also in the HMW
#-------------------------------------------------------------------#

matched_species <- inner_join(MDD_checklist, hmw_data, by = c("sciname" = "name"))

# reduce the data 
reduced_hmw <- matched_species |> select(sciname, habitat)|> distinct(sciname,.keep_all = TRUE)



#-------------------------------------------------------#
# 4. first clean out the common typos
#-------------------------------------------------------#

# common patterns numbers
pattern <- "(\\w+\\s+){0,5}(\\d+\\s*\\-?\\s*\\d*\\s*m)(\\s+\\w+){0,5}"

# Clean common typos in the habitat column ¢
reduced_hmw <- reduced_hmw |>
  # Remove '¢.' or 'c.'
  mutate(habitat = str_replace_all(habitat, regex("¢\\.|c\\.", ignore_case = TRUE), "")) |>
  mutate(habitat = str_replace_all(habitat, regex("\\b(c|¢)\\b", ignore_case = TRUE), "")) |>
  # Remove 'c .' or 'C .', with case insensitivity and regardless of spaces between characters
  mutate(habitat = str_replace_all(habitat, regex("c\\s*\\.\\s*", ignore_case = TRUE), "")) |>
  # Remove '.' followed by spaces and digits
  mutate(habitat = str_replace_all(habitat, regex("\\.\\s*(\\d+)"), "\\1")) |>
  # Transform "number—number" with possible multiple dashes and spaces to "number m - number m"
  mutate(habitat = str_replace_all(habitat, regex("(\\d+)\\s*—[-]*\\s*(\\d+)"), "\\1 m - \\2 m")) |>
  # Transform "of1100 m" to "of number m"
  mutate(habitat = str_replace_all(habitat, regex("of(\\d+)\\s*m"), "of number m"))
# cleaned info from 'habitat'

#--------------------------------------------------------------------#
# 5. Cleaning the elevations out of the habitat column 
#---------------------------------------------------------------------#

# Step 1
# With this function I clean if there is a clear pattern of number 1 - number 2 m --> min and max elevation

extract_elevation <- function(elevation_info) {
  # Extract the pattern of two numbers separated by a hyphen
  pattern <- str_extract(elevation_info, "\\b\\d+-\\d+\\b")
  
  # Count the number of hyphens and numbers in the pattern
  hyphen_count <- str_count(elevation_info, "-")
  number_count <- str_count(elevation_info, "\\d+")
  
  # If there are exactly two numbers and one hyphen, extract the min and max elevations
  if (!is.na(pattern) && str_detect(elevation_info, pattern) && hyphen_count == 1 && number_count == 2) {
    elevations <- str_split(pattern, "-")[[1]]
    min_elevation <- as.numeric(elevations[1])
    max_elevation <- as.numeric(elevations[2])
  } else {
    min_elevation <- NA_real_ # using NA_real_ to ensure numeric NA
    max_elevation <- NA_real_ # using NA_real_ to ensure numeric NA
  }
  
  # Return a tibble (small data frame)
  return(tibble(min_elevation = min_elevation, max_elevation = max_elevation))
}

# Apply the function to the 'elevation_info' column and unnest the results
reduced_hmw_step1 <- reduced_hmw |>
  mutate(elevation_data = map(elevation_info, extract_elevation)) |>
  tidyr::unnest(cols = c(elevation_data)) # spread the nested data frame into separate columns


# Step 2
# Function to extract elevation based on keywords
extract_keyword_elevation <- function(text, keywords_max, keywords_min) {
  max_val <- NA_real_
  min_val <- NA_real_
  
  # Regular expression to match any characters between keywords and numbers, non-greedy
  regex_between <- ".*?"
  
  # Check if text contains any max keywords and extract the number that follows
  for (keyword in keywords_max) {
    if (str_detect(text, paste0("\\b", keyword, "\\b"))) {
      # Extract the first number that follows the keyword, possibly after other content
      number_after_keyword <- str_extract(text, paste0("(?<=\\b", keyword, "\\b)", regex_between, "\\d+"))
      if (!is.na(number_after_keyword)) {
        # Extract the numeric part from the result
        max_val <- as.numeric(str_extract(number_after_keyword, "\\d+"))
        break
      }
    }
  }
  
  # Check if text contains any min keywords and extract the number that follows
  for (keyword in keywords_min) {
    if (str_detect(text, paste0("\\b", keyword, "\\b"))) {
      # Extract the first number that follows the keyword, possibly after other content
      number_after_keyword <- str_extract(text, paste0("(?<=\\b", keyword, "\\b)", regex_between, "\\d+"))
      if (!is.na(number_after_keyword)) {
        # Extract the numeric part from the result
        min_val <- as.numeric(str_extract(number_after_keyword, "\\d+"))
        break
      }
    }
  }
  
  # Return a list with min and max values
  return(list(min_val = min_val, max_val = max_val))
}


# Define the keywords for max and min elevation
keywords_max <- c("over", "up to","below","as high as","to elevations of", "to at least","less than","not exceeding","upper elevations of","elevations that reach") 
keywords_min <- c("down to","above","greater than","higher than") 

# Apply the function to extract additional elevations
reduced_hmw_step_2 <- reduced_hmw_step1 |>
  mutate(
    keyword_elevation = map(elevation_info, extract_keyword_elevation, keywords_max = keywords_max, keywords_min = keywords_min),
    keyword_min = map_dbl(keyword_elevation, ~ .x$min_val),
    keyword_max = map_dbl(keyword_elevation, ~ .x$max_val)
  ) |>
  mutate(
    min_elevation = if_else(is.na(min_elevation) & !is.na(keyword_min), keyword_min, min_elevation),
    max_elevation = if_else(is.na(max_elevation) & !is.na(keyword_max), keyword_max, max_elevation)
  ) |>
  select(-keyword_elevation, -keyword_min, -keyword_max) # remove the intermediate columns


# Step 3 there are key words from Sea level to .. elevation 
extract_general_elevation <- function(text) {
  max_val <- NA_real_
  min_val <- NA_real_
  
  # Patterns for variations of 'sea level' followed by various phrases, then a number
  sea_level_patterns <- c(
    "sea\\s*level\\s*to\\s*(\\d+)",                   # pattern a
    "sea\\s*level\\s*to\\s*elevation(?:s)?\\s*of\\s*(\\d+)", # pattern b
    "sea\\s*level\\s*up\\s*to\\s*(\\d+)",              # pattern c
    "sea\\s*level\\s*to\\s*about\\s*(\\d+)",           # pattern d
    "sealevel\\s*to\\s*(\\d+)"                        # pattern for 'sealevel' variation
    # add more patterns here as needed
  )
  
  for (pattern in sea_level_patterns) {
    if (str_detect(text, regex(pattern, ignore_case = TRUE))) {
      matches <- str_match(text, regex(pattern, ignore_case = TRUE))
      min_val <- 0  # 'sea level' implies a starting elevation of 0
      if (!is.na(matches[1,2])) { # if there's a number following 'sea level'
        max_val <- as.numeric(matches[1,2]) # this should be the captured number, indicating the max elevation
      }
      return(list(min_val = min_val, max_val = max_val)) # return immediately if 'sea level' pattern was found
    }
  }
  
  # General pattern for number-to-number, applies if 'sea level' wasn't matched
  general_pattern <- "(\\d+)\\s+(?:.*?\\s+)?to(?:\\s+.*?\\s+)?(\\d+)"  # adjusted to potentially capture more varied text
  if (str_detect(text, regex(general_pattern, ignore_case = TRUE))) {
    matches <- str_match(text, regex(general_pattern, ignore_case = TRUE))
    min_val <- as.numeric(matches[1,2]) # this should be the first captured number
    max_val <- as.numeric(matches[1,3]) # this should be the second captured number
  }
  
  return(list(min_val = min_val, max_val = max_val))
}


reduced_hmw_step3 <- reduced_hmw_step_2 |>
  mutate(general_elevation = map(elevation_info, extract_general_elevation), # apply the new function
         general_min = map_dbl(general_elevation, ~ .x$min_val), # extract min_val from the list
         general_max = map_dbl(general_elevation, ~ .x$max_val)) |> # extract max_val from the list
  mutate(
    min_elevation = if_else(is.na(min_elevation) & !is.na(general_min), general_min, min_elevation), # update min_elevation if needed
    max_elevation = if_else(is.na(max_elevation) & !is.na(general_max), general_max, max_elevation) # update max_elevation if needed
  ) |>
  select(-general_elevation, -general_min, -general_max) # remove the temporary columns



### Clean out more common patterns
extract_at_elevation <- function(text) {
  min_val <- NA_real_
  max_val <- NA_real_
  
  # Define the patterns
  pattern_an_elevation <- "at\\s+an\\s+elevation\\s+of\\s+(\\d+)" # single number pattern with "an elevation of"
  pattern_up_to_sea_level <- "up\\s+to\\s+(\\d+)\\s+above\\s+sea\\s+level" # single number pattern with "up to" above sea level
  pattern_at_elevations_around <- "at\\s+elevations?\\s+around\\s+(\\d+)" # single number pattern with "around"
  pattern_between <- "between\\s+(\\d+)\\s*(?:m)?\\s+and\\s+(\\d+)\\s*(?:m)?" # range pattern: number and number, optionally followed by 'm'
  pattern_from_to <- "from\\s+(\\d+)\\s*m?\\s+to\\s+(\\d+)\\s*m?\\s*(elevation)?" # range pattern: number to number, optionally followed by 'm' and 'elevation'
  pattern_to_elevation <- "to\\s+an\\s+elevation\\s+of\\s+(\\d+)" # single number pattern with "to an elevation of"
  pattern_range <- "at\\s+elevations?\\s+of\\s+(\\d+)\\s*-\\s*(\\d+)" # range pattern: number - number
  pattern_single <- "at\\s+elevations?\\s+of\\s+(?:around\\s+)?(\\d+)" # single pattern: number or around number
  
  # Check for each pattern, starting with the new ones
  if (str_detect(text, regex(pattern_to_elevation, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_to_elevation)
    max_val <- as.numeric(matches[1,2])
  } else if (str_detect(text, regex(pattern_an_elevation, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_an_elevation)
    min_val <- as.numeric(matches[1,2])
  } else if (str_detect(text, regex(pattern_up_to_sea_level, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_up_to_sea_level)
    max_val <- as.numeric(matches[1,2])
  } else if (str_detect(text, regex(pattern_at_elevations_around, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_at_elevations_around)
    min_val <- as.numeric(matches[1,2])
  } else if (str_detect(text, regex(pattern_between, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_between)
    min_val <- as.numeric(matches[1,2])
    max_val <- as.numeric(matches[1,3])
  } else if (str_detect(text, regex(pattern_from_to, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_from_to)
    min_val <- as.numeric(matches[1,2])
    max_val <- as.numeric(matches[1,3])
  } else if (str_detect(text, regex(pattern_range, ignore_case = TRUE))) { # existing patterns
    matches <- str_match(text, pattern_range)
    min_val <- as.numeric(matches[1,2])
    max_val <- as.numeric(matches[1,3])
  } else if (str_detect(text, regex(pattern_single, ignore_case = TRUE))) { # existing patterns
    matches <- str_match(text, pattern_single)
    min_val <- as.numeric(matches[1,2])
  }
  
  return(list(min_val = min_val, max_val = max_val))
}

# 
reduced_hmw_step_4 <- reduced_hmw_step3 |>
  mutate(at_elevation = map(elevation_info, extract_at_elevation), 
         at_min = map_dbl(at_elevation, ~ .x$min_val), # extract min_val from the list
         at_max = map_dbl(at_elevation, ~ .x$max_val)) |> # extract max_val from the list
  mutate(
    min_elevation = if_else(is.na(min_elevation) & !is.na(at_min), at_min, min_elevation), # update min_elevation if needed
    max_elevation = if_else(is.na(max_elevation) & !is.na(at_max), at_max, max_elevation) # update max_elevation if needed
  ) |>
  select(-at_elevation, -at_min, -at_max) # remove the temporary columns



cleaned_hnw <- reduced_hmw_step_4|>mutate(alpine = ifelse(grepl("alpine", habitat, ignore.case = TRUE), "yes", "no"))|>
  mutate(min_elevation_regional = "",
         max_elevation_regional = "",
         regional_elevation_info = "")


#-------------------------------------#
# 6. Check which are still NA
#------------------------------------#

NA_elevation <- cleaned_hnw|>
  filter(is.na(min_elevation) & is.na(max_elevation))

#--------------------------------------------#
# 6. save file
#---------------------------------------------#


file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/HMW_cleaned.xlsx")

writexl::write_xlsx(cleaned_hnw, file_path)



# ------------ To filter for key words alpine, mountain, mountainous, elevation,...

#filter_words <- function(df, words) {
#pattern <- paste(words, collapse = "|")

#df |>
# rowwise() |>
#filter(if_any(everything(), ~str_detect(.x, regex(pattern, ignore_case = TRUE)))) |>
#ungroup()
#}

# here instead of filtering for alpine, mountain specific words use the checklist to filter for species once it is finalized
# Filter for words 
#words_to_filter <- c("alpine", "elevation","mountain","mountainous")


#filtered_hmw <- filter_words(hmw_data, words_to_filter)

# remove columns
#filtered_hmw <- filtered_hmw |>
# select(docId, name, habitat, activityPatterns, movementsHomeRangeAndSocialOrganization, statusAndConservation, verbatimText) |>
#filter(!is.na(name) & name != "") |>
#mutate(alpine = ifelse(grepl("alpine", habitat, ignore.case = TRUE), "yes", "no"))# add a column if "alpine" occurs

# reduce the data 
#reduced_hmw <- filtered_hmw |>
# select(name, habitat, alpine)
