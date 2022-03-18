# To create timesheet based on worker preprocess data
source('worker_time_table.R')

build_timesheet <- function(workersTimeClock) {
  library(lubridate)
  library(r2excel)
  
  wb <- createWorkbook(type="xlsx")
  IDS <- workersTimeClock %>% select(ID) %>% unique
  for(i in 1:nrow(IDS)) {
    # Obtain parameter  ---------------------------------
    currentID <- IDS[i,]$ID
    fullname <- workersTimeClock %>% filter(ID == currentID) %>% pull(Name) %>% unique
    
    max_date <- max(lubridate::dmy(workersTimeClock$Fecha))
    min_date <- min(lubridate::dmy(workersTimeClock$Fecha))
    
    sheet <- createSheet(wb, sheetName = fullname)
    builtHeader(wb, sheet, min_date, max_date, currentID)
    
    # Result table  ---------------------------------
    resultTable <- workersTimeClock %>% timeTable(currentID, get_lunch_time(currentID))
    xlsx.addTable(wb, sheet, resultTable, startCol = 1)
    
    xlsx.addLineBreak(sheet, 2)
    
    # Footer  ---------------------------------
    TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, isBold=TRUE, underline=1)
    SUBTITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=16, isBold=TRUE)
    
    rows <-createRow(sheet,rowIndex=35)
    sheetTitle <-createCell(rows, colIndex=2)
    setCellValue(sheetTitle[[1,1]], "Elaborado por:")
    setCellStyle(sheetTitle[[1,1]], SUBTITLE_STYLE)
    
    sheetTitle <-createCell(rows, colIndex=6)
    setCellValue(sheetTitle[[1,1]], "Recibido Trabajador:")
    setCellStyle(sheetTitle[[1,1]], SUBTITLE_STYLE)
    
    rows <-createRow(sheet,rowIndex=36)
    sheetTitle <-createCell(rows, colIndex=2)
    setCellValue(sheetTitle[[1,1]], "Ing. Alba Rosa Espinoza Guevara")
    setCellStyle(sheetTitle[[1,1]], SUBTITLE_STYLE)
    
    rows <-createRow(sheet,rowIndex=37)
    sheetTitle <-createCell(rows, colIndex=2)
    setCellValue(sheetTitle[[1,1]], "Supervisora de producción")
    setCellStyle(sheetTitle[[1,1]], SUBTITLE_STYLE)
    
    sheetTitle <-createCell(rows, colIndex=6)
    setCellValue(sheetTitle[[1,1]], fullname)
    setCellStyle(sheetTitle[[1,1]], SUBTITLE_STYLE)
    
    filename <- paste0("generated/",get_report_filename(max_date),".xls")
  }
  saveWorkbook(wb, filename)
  
}