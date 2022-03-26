library(lubridate)
# Utility functions, this will serve as a kind of sink for functions 
IGNORE_WORKERS_IDS = c(17)

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

builtHeader <- function(wb, sheet_number, min_date, max_date, worker_id, total_approved_hours) {
  library(openxlsx)
  
  TITLE_STYLE <- createStyle(fontSize = 22, fontColour = "#000000", halign = "center", valign = "center", textDecoration = c("bold", "underline"))
  SUBTITLE_STYLE1 <- createStyle(fontSize = 18, fontColour = "#000000", halign = "center", valign = "center")
  SUBTITLE_STYLE2 <- createStyle(fontSize = 16, fontColour = "#000000", halign = "center", valign = "center")
  SUBTITLE_STYLE3 <- createStyle(fontSize = 14, fontColour = "#000000", halign = "center", valign = "center")
  SUBTITLE_STYLE <- createStyle(fontSize = 14, fontColour = "#000000", halign = "center", valign = "center")
   
  SUBTITLE_STYLE4 <- createStyle(fontSize = 14, fontColour = "#000000", textDecoration = c("underline"))
  SUBTITLE_STYLE5 <- createStyle(fontSize = 14, fontColour = "#000000")
  # 
  title <- paste('REPORTE DE HORAS EXTRAS PRODUCCIÓN', get_header_from_date(max_date))
  subtitle <- paste('PERIODO:', lubridate::day(min_date), MONTHS[lubridate::month(min_date)],
                    "AL", lubridate::day(max_date), MONTHS[lubridate::month(max_date)], lubridate::year(max_date))
  subtitle2 <- '1. HORARIOS OPERADORES Y AUXILIARES PRODUCCIÓN'
  subtitle3 <- '(JORNADA CONTINUA) 48 HORAS SEMANALES - TURNO DIURNO'
   
  addHeader(wb, sheet_number, value=title, startRow = 1, startCol = 1, textStyle = TITLE_STYLE)
  mergeCells(wb, sheet=sheet_number, cols=1:15, rows=1)
   
  addHeader(wb, sheet_number, value=subtitle, startCol = 1, startRow = 2, textStyle = SUBTITLE_STYLE1)
  mergeCells(wb, sheet=sheet_number, cols=1:15, rows=2)
  
  addHeader(wb, sheet_number, value=subtitle2, startCol = 1, startRow = 3, textStyle = SUBTITLE_STYLE2)
  mergeCells(wb, sheet=sheet_number, cols=1:15, rows=3)
   
  addHeader(wb, sheet_number, value=subtitle3, startCol = 1, startRow = 4, textStyle = SUBTITLE_STYLE3)
  mergeCells(wb, sheet=sheet_number, cols=1:15, rows=4)
  # 
  # xlsx.addLineBreak(sheet, 3)
   
  addTitle(wb, sheet_number, 7, 3, "ENTRADA", SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 7, 4, "SALIDA", SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 7, 6, "HORAS DIARIAS", SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 7, 7, get_hour_by_worker_id(worker_id), SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 7, 9, "TIEMPO HE", SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 7, 10, total_approved_hours, SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 7, 11, "HRS", SUBTITLE_STYLE5)
   
  addTitle(wb, sheet_number, 8, 2, "LUNES A VIERNES", SUBTITLE_STYLE4)
   
  addTitle(wb, sheet_number, 8, 3, "7:00 AM", SUBTITLE_STYLE5)
  addTitle(wb, sheet_number, 8, 4, "4:36 PM", SUBTITLE_STYLE5)
  addTitle(wb, sheet_number, 8, 6, "DIA SEMANAS", SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 8, 7, 5, SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 8, 9, "DEDUCCION", SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 8, 11, "HRS", SUBTITLE_STYLE5)
  
  addTitle(wb, sheet_number, 9, 2, "TIEMPO ALMUERZO", SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 9, 3, get_lunch_time(worker_id), SUBTITLE_STYLE5)
  addTitle(wb, sheet_number, 9, 4, "hr", SUBTITLE_STYLE5)
  addTitle(wb, sheet_number, 9, 6, "HORAS SEMANALES", SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 9, 7, get_hour_by_worker_id(worker_id) * 5, SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 9, 9, "TOTAL HE", SUBTITLE_STYLE4)
  addTitle(wb, sheet_number, 9, 11, "HRS", SUBTITLE_STYLE5)
  
}



addTitle <- function(workbook, sheet_number, startRow, startCol, title, titleStyle){
  writeData(workbook, sheet = sheet_number, x = title, startCol = startCol, startRow = startRow)
  addStyle(workbook, sheet_number, titleStyle, rows = startRow, cols = startCol)
}


addHeader <- function (wb, sheet_number, value = "Header", startRow = 1, startCol = 1, textStyle = NULL) 
{
  writeData(wb, sheet = sheet_number, x = value, startCol = startCol, startRow)
  addStyle(wb, sheet_number, textStyle, rows = startRow, cols = startCol)
}

