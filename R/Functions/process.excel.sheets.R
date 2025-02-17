# Function to read the sheet and convert certain columns to numeric
process_sheet <- function(sheet) {
  df <- read_excel(file_path, sheet = sheet)
  
  df <- df |>
    mutate(
      overlap_percentage_alpine = as.numeric(overlap_percentage_alpine),
      overlap_percentage_mountain = as.numeric(overlap_percentage_mountain)
    )
  
  return(df)
}
