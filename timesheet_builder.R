# To create timesheet based on worker preprocess data
source('worker_time_table.R')
library(stringr)
library(logger)


build_timesheet <- function(workersTimeClock) {
  log_info("Building time sheet")
  library(lubridate)
  library(openxlsx)
   
  wb <- createWorkbook()
  IDS <- workersTimeClock %>% select(ID) %>% unique
   
  log_info(paste0("Workbook created, iterating over ", nrow(IDS), " ids."))
  for(i in 1:nrow(IDS)) {
  # Obtain parameter  ---------------------------------
  currentID <- IDS[i,]$ID
  fullname <- workersTimeClock %>% filter(ID == currentID) %>% pull(Name) %>% unique
  log_info(paste0("Fullname: ", fullname))
  
  max_date <- max(lubridate::dmy(workersTimeClock$Fecha))
  min_date <- min(lubridate::dmy(workersTimeClock$Fecha))
  
  addWorksheet(wb, sheetName = str_trunc(fullname, 31, "right"))
  
  log_info(paste0("Builing header for sheet: ", i))
  
  # Result table  ---------------------------------
  resultTable <- workersTimeClock %>% timeTable(currentID, get_lunch_time(currentID))
  
  total_approved_hours <- sum(as.numeric(resultTable$total_he_approved))
  builtHeader(wb, sheet_number = i, min_date, max_date, currentID, total_approved_hours)
  
  resultTable <- resultTable %>%  select(-total_he_approved)
  writeDataTable(wb, sheet = i, resultTable, startCol = 2, startRow = 12, tableStyle = "TableStyleLight2")
  
  # Footer  ---------------------------------
  TITLE_STYLE <- createStyle(fontSize = 16, fontColour = "#000000", halign = "center", valign = "center", textDecoration = c("bold", "underline"))
  SUBTITLE_STYLE <- createStyle(fontSize = 16, fontColour = "#000000", textDecoration = c("bold"))
  
  writeData(wb, i, x = "Elaborado por:", startCol = 2, startRow = 35)
  addStyle(wb, i, SUBTITLE_STYLE, rows = 35, cols = 2)
  
  writeData(wb, i, x = "Recibido Trabajador:", startCol = 7, startRow = 35)
  addStyle(wb, i, SUBTITLE_STYLE, rows = 35, cols = 7)
  
  writeData(wb, i, x = "Ing. Alba Rosa Espinoza Guevara", startCol = 2, startRow = 36)
  addStyle(wb, i, SUBTITLE_STYLE, rows = 36, cols = 2)
  
  writeData(wb, i, x = "Supervisora de producción", startCol = 2, startRow = 37)
  addStyle(wb, i, SUBTITLE_STYLE, rows = 37, cols = 2)
  
  writeData(wb, i, x = fullname, startCol = 7, startRow = 37)
  addStyle(wb, i, SUBTITLE_STYLE, rows = 37, cols = 7)
  
  filename <- paste0("generated/",get_report_filename(max_date),".xlsx")
  }
  log_info(paste0("Generating file: ", filename))
  saveWorkbook(wb, filename, overwrite = T)
  
  return (filename)
}