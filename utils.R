library(lubridate)
# Utility functions, this will serve as a kind of sink for functions 
IGNORE_WORKERS_IDS = c(17, 116)

SUNDAY_WDAY = 1
SATURDAY_WDAY = 7
WEEKENDS = c(SATURDAY_WDAY, SUNDAY_WDAY)
MONTHS = c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")

# TODO: this function should be simplify somehow
get_worker_daily_hours <- function(worker_id, work_date) { 
  work_date <- lubridate::wday(dmy(work_date))
  if (is_weekend(work_date)) {
    return(0.0)
  }
  if (worker_id %in% c(88)) {
    return (9.0)
  }
  return(9.6)
}

is_weekend <- function(wday) {
  wday %in% WEEKENDS
}


get_header_text <- function(fechas) {
  max_date <- max(lubridate::dmy(fechas))
  get_header_from_date(max_date)
}

get_header_from_date <- function(date) {
  paste("PERIODO", case_when(lubridate::day(date) < 16 ~ "1º", TRUE ~ "2º"), 
        MONTHS[lubridate::month(date)], 
        lubridate::year(date))
}

builtHeader <- function(wb, sheet, min_date, max_date) {
  library(xlsx)
  
  TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=22, isBold=TRUE, underline = 2, boldweight = 900) + Alignment(h="ALIGN_CENTER")
  SUBTITLE_STYLE1 <- CellStyle(wb) + Font(wb,  heightInPoints=18, isBold=TRUE) + Alignment(h="ALIGN_CENTER")
  SUBTITLE_STYLE2 <- CellStyle(wb) + Font(wb,  heightInPoints=16, isBold=TRUE) + Alignment(h="ALIGN_CENTER")
  SUBTITLE_STYLE3 <- CellStyle(wb) + Font(wb,  heightInPoints=14, isBold=TRUE) + Alignment(h="ALIGN_CENTER")
  SUBTITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isBold=FALSE)
  
  title <- paste('REPORTE DE HORAS EXTRAS PRODUCCIÓN', get_header_from_date(max_date))
  subtitle <- paste('PERIODO:', lubridate::day(min_date), MONTHS[lubridate::month(min_date)],
                      "AL", lubridate::day(max_date), MONTHS[lubridate::month(max_date)], lubridate::year(max_date))
  subtitle2 <- '1. HORARIOS OPERADORES Y AUXILIARES PRODUCCIÓN'
  subtitle3 <- '(JORNADA CONTINUA) 48 HORAS SEMANALES - TURNO DIURNO'
  
  addHeader(wb, sheet, value=title, startCol = 1, textStyle = TITLE_STYLE)
  addMergedRegion(sheet, 1, 1, 1, 10)
  
  addHeader(wb, sheet, value=subtitle, startCol = 1, textStyle = SUBTITLE_STYLE1)
  addMergedRegion(sheet, 2, 2, 1, 10)
  
  addHeader(wb, sheet, value=subtitle2, startCol = 1, textStyle = SUBTITLE_STYLE2)
  addMergedRegion(sheet, 3, 3, 1, 10)
  
  addHeader(wb, sheet, value=subtitle3, startCol = 1, textStyle = SUBTITLE_STYLE3)
  addMergedRegion(sheet, 4, 4, 1, 10)
  
  xlsx.addLineBreak(sheet, 3)
  
  rows <- xlsx.addTitle(sheet, NULL, 10, 3, "ENTRADA", TITLE_STYLE)
  xlsx.addTitle(sheet, rows, 10, 4, "SALIDA", TITLE_STYLE)
  xlsx.addTitle(sheet, rows, 10, 5, "ALMUERZO(1/2 HORA)", TITLE_STYLE)
  
  rows <- xlsx.addTitle(sheet, NULL, 11, 2, "LUNES A VIERNES", TITLE_STYLE)
  xlsx.addTitle(sheet, rows, 11, 3, "7:00 AM", SUBTITLE_STYLE)
  xlsx.addTitle(sheet, rows, 11, 4, "4:36 PM", SUBTITLE_STYLE)
  xlsx.addTitle(sheet, rows, 11, 5, "12:00 PM - 12:30 PM", SUBTITLE_STYLE)
  xlsx.addTitle(sheet, rows, 11, 6, "HORAS DIARIAS LUNES A VIERNES", TITLE_STYLE)
  xlsx.addTitle(sheet, rows, 11, 7, "9,6", SUBTITLE_STYLE)
  
  rows <- xlsx.addTitle(sheet, NULL, 12, 2, "TIEMPO ALMUERZO", TITLE_STYLE)
  xlsx.addTitle(sheet, rows, 12, 3, "0,5", SUBTITLE_STYLE)
  xlsx.addTitle(sheet, rows, 12, 6, "DIA SEMANA", TITLE_STYLE)
  xlsx.addTitle(sheet, rows, 12, 7, "5", SUBTITLE_STYLE)
  
  rows <- xlsx.addTitle(sheet, NULL, 13, 6, "HORAS SEMANALES", TITLE_STYLE)
  xlsx.addTitle(sheet, rows, 13, 7, "48", SUBTITLE_STYLE)
  
  
  xlsx.addLineBreak(sheet, 4)
}



xlsx.addTitle <- function(sheet, rows = NULL, rowIndex, colIndex, title, titleStyle){
  if (is.null(rows))
    rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=colIndex)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
  
  return (rows)
}


addHeader <- function (wb, sheet, value = "Header", startRow = NULL, startCol = 2, textStyle = NULL) 
{
  if (is.null(startRow)) {
    rows <- getRows(sheet)
    startRow = length(rows) + 1
  }
  rows <- createRow(sheet, rowIndex = startRow)
  sheetTitle <- createCell(rows, colIndex = startCol)
  setCellValue(sheetTitle[[1, 1]], value)
  xlsx::setCellStyle(sheetTitle[[1, 1]], textStyle)
}

