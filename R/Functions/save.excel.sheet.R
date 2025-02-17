# Save Excel on seperate sheets

save_excel_sheet <- function(file_path, order_name, mammals_checklist) {
  # Load the openxlsx library
  library(openxlsx)
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("The target Excel file does not exist.")
  }
  
  # Load the existing workbook
  wb <- loadWorkbook(file_path)
  
  # Check if the sheet already exists
  if (!(order_name %in% names(wb))) {
    # If not, add a new sheet and write the data there
    addWorksheet(wb, order_name)
    writeData(wb, sheet = order_name, x = mammals_checklist)
  } else {
    # If the sheet exists, append the data (assuming the new data has the same column structure)
    writeData(wb, sheet = order_name, x = mammals_checklist, startRow = getRowsHeight(wb, order_name) + 1)
  }
  
  # Save the workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
}
