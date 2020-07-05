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
  get_hour_by_worker_id(worker_id)
}

get_hour_by_worker_id <- function(worker_id) {
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

get_report_filename <- function(date) {
  paste("REPORTE-", case_when(lubridate::day(date) < 16 ~ "PRIMERO-", TRUE ~ "SEGUNDO-"), 
        MONTHS[lubridate::month(date)], 
        lubridate::year(date))
}

get_lunch_time <- function(worker_id) {
  # Exception for Lechus
  ifelse(worker_id == 108, 1.0, 0.5)
}

builtHeader <- function(wb, sheet, min_date, max_date, worker_id) {
  library(xlsx)
  
  TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=22, isBold=TRUE, underline = 2, boldweight = 900) + Alignment(h="ALIGN_CENTER")
  SUBTITLE_STYLE1 <- CellStyle(wb) + Font(wb,  heightInPoints=18, isBold=TRUE) + Alignment(h="ALIGN_CENTER")
  SUBTITLE_STYLE2 <- CellStyle(wb) + Font(wb,  heightInPoints=16, isBold=TRUE) + Alignment(h="ALIGN_CENTER")
  SUBTITLE_STYLE3 <- CellStyle(wb) + Font(wb,  heightInPoints=14, isBold=TRUE) + Alignment(h="ALIGN_CENTER")
  SUBTITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isBold=FALSE)
  
  SUBTITLE_STYLE4 <- CellStyle(wb) + Font(wb,  heightInPoints=14, isBold=TRUE, underline = T)
  SUBTITLE_STYLE5 <- CellStyle(wb) + Font(wb,  heightInPoints=14)
  
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
  
  rows <- xlsx.addTitle(sheet, NULL, 7, 3, "ENTRADA", SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 7, 4, "SALIDA", SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 7, 6, "HORAS DIARIAS", SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 7, 7, get_hour_by_worker_id(worker_id), SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 7, 9, "TIEMPO HE", SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 7, 11, "HRS", SUBTITLE_STYLE5)
  
  rows <- xlsx.addTitle(sheet, NULL, 8, 2, "LUNES A VIERNES", SUBTITLE_STYLE4)
  
  xlsx.addTitle(sheet, rows, 8, 3, "7:00 AM", SUBTITLE_STYLE5)
  xlsx.addTitle(sheet, rows, 8, 4, "4:36 PM", SUBTITLE_STYLE5)
  xlsx.addTitle(sheet, rows, 8, 6, "DIA SEMANAS", SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 8, 7, 5, SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 8, 9, "DEDUCCION", SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 8, 11, "HRS", SUBTITLE_STYLE5)
  
  rows <- xlsx.addTitle(sheet, NULL, 9, 2, "TIEMPO ALMUERZO", SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 9, 3, get_lunch_time(worker_id), SUBTITLE_STYLE5)
  xlsx.addTitle(sheet, rows, 9, 4, "hr", SUBTITLE_STYLE5)
  xlsx.addTitle(sheet, rows, 9, 6, "HORAS SEMANALES", SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 9, 7, get_hour_by_worker_id(worker_id) * 5, SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 9, 9, "TOTAL HE", SUBTITLE_STYLE4)
  xlsx.addTitle(sheet, rows, 9, 11, "HRS", SUBTITLE_STYLE5)
  xlsx.addLineBreak(sheet, 2)
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

