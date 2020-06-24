# To create timesheet based on worker preprocess data

build_timesheet <- function(workersTimeClock) {
  
  source('./worker_time_table.R')
  
  IDS <- workersTimeClock %>% select(ID) %>% unique
  for(i in 1:nrow(IDS)) {
    currentID <- IDS[i,]$ID
    fullname <- workersTimeClock %>% filter(ID == currentID) %>% pull(Name) %>% unique
    
    max_date <- max(lubridate::dmy(workersTimeClock$Fecha))
    min_date <- min(lubridate::dmy(workersTimeClock$Fecha))
    
    filename <- paste0("generated/",fullname,".xls")
    wb <- createWorkbook(type="xlsx")
    sheet <- createSheet(wb, sheetName = "hoja1")
    
    builtHeader(wb, sheet, min_date, max_date)
    
    resultTable <- workersTimeClock %>% timeTable(currentID)
    
    xlsx.addTable(wb, sheet, resultTable[1:4,], startCol=2)
    xlsx.addTable(wb, sheet, resultTable[5,] %>% mutate_at(vars(matches("\\d{2}\\/\\d{2}\\/\\d{4}")), as.numeric), 
                  startCol=2, col.names = FALSE, row.names = T)
    xlsx.addTable(wb, sheet, resultTable[6,], startCol=2, col.names = FALSE, row.names = T)
    
    xlsx.addTable(wb, sheet, resultTable[7:9,] %>% mutate_at(vars(matches("\\d{2}\\/\\d{2}\\/\\d{4}")), as.numeric), 
                  startCol=2, col.names = FALSE)
    
    xlsx.addLineBreak(sheet, 4)
    
    TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, isBold=TRUE, underline=1)
    SUBTITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=14, isBold=FALSE, underline=0)
    
    rows <-createRow(sheet,rowIndex=35)
    sheetTitle <-createCell(rows, colIndex=4)
    setCellValue(sheetTitle[[1,1]], "Elaborado por:")
    setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
    
    sheetTitle <-createCell(rows, colIndex=8)
    setCellValue(sheetTitle[[1,1]], "Recibido Trabajador:")
    setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
    
    rows <-createRow(sheet,rowIndex=36)
    sheetTitle <-createCell(rows, colIndex=4)
    setCellValue(sheetTitle[[1,1]], "Ing. Alba Rosa Espinoza Guevara")
    setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
    
    rows <-createRow(sheet,rowIndex=37)
    sheetTitle <-createCell(rows, colIndex=4)
    setCellValue(sheetTitle[[1,1]], "Supervisora de producción")
    setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
    
    sheetTitle <-createCell(rows, colIndex=8)
    setCellValue(sheetTitle[[1,1]], fullname)
    setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
    
    saveWorkbook(wb, filename)
    
  }
  
}