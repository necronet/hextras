# Utility functions, this will serve as a kind of sink for functions 
IGNORE_WORKERS_IDS = c(17, 116, 190)


get_worker_daily_hours <- function(worker_id) { 
  if (worker_id == 88) {
    return (9.0)
  }
  return(9.6)
}



builtHeader <- function(wb, sheet) {
  library(xlsx)
  
  TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, isBold=TRUE, underline=1)
  SUBTITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=14, isBold=FALSE, underline=0)
  
  title<-'REPORTE DE HORAS EXTRAS PRODUCCIÓN 2º MAYO 2020'
  subtitle <- 'PERIODO: 		11 MAYO AL 25 MAYO 2020	'
  subtitle2 <- '1. HORARIOS OPERADORES Y AUXILIARES PRODUCCIÓN'
  subtitle3 <- '(JORNADA CONTINUA) 48 HORAS SEMANALES - TURNO DIURNO'
  
  xlsx.addHeader(wb, sheet, value=title,level=1, color="black", underline=1)
  xlsx.addLineBreak(sheet, 1)
  
  xlsx.addHeader(wb, sheet, value=subtitle,level=2, color="black", underline=1)
  xlsx.addLineBreak(sheet, 1)
  
  xlsx.addHeader(wb, sheet, value=subtitle2,level=2, color="black", underline=1)
  xlsx.addLineBreak(sheet, 1)
  
  xlsx.addHeader(wb, sheet, value=subtitle3,level=2, color="black", underline=1)
  xlsx.addLineBreak(sheet, 1)
  
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


