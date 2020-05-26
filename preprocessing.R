library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(r2excel)

ignore_workers = c(17, 88, 116, 190)

workers <-  read_excel("data/trabajadores_horas_extras.xlsx")

worker_id %>% filter(ID == 7)

worker_id <- workers %>% filter(!str_detect(Fecha, "\\d{2}\\/\\d{2}\\/\\d{4}")) %>% 
        mutate(worker = Fecha) %>% select(worker) %>% 
        separate(worker, c('ID','Name'), sep = "\\-") %>% mutate(ID = as.integer(ID), Name = str_trim(Name)) %>%
        filter(!(ID %in% ignore_workers))


worker_workday <- workers %>% filter(!(ID %in% ignore_workers)) %>% mutate( Registros = ifelse(toupper(Registros) == "VACACIONES", NA, Registros), Obs = `...13` ) %>% 
            filter(str_detect(Fecha, "\\d{2}\\/\\d{2}\\/\\d{4}")) %>% filter(Registros != '--') %>%
            filter(!is.na(Registros)) %>% select(ID, Fecha, Registros, Obs)  %>% 
            separate(Registros, c('T1','T2','T3','T4'), sep = "\\|") %>% mutate_all(str_trim) %>%
            filter(!is.na(T4) & ID %in% c(7,11) )
            

lubridate::dmy_hms('11/05/2020 07:01:32')
round(as.POSIXct(starttime, format="%H:%M %S", tz="UTC"), units="hours")

xlsx.addTitle <- function(sheet, rows = NULL, rowIndex, colIndex, title, titleStyle){
  if (is.null(rows))
    rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=colIndex)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
  
  return (rows)
}

dumpTable <- function(currentID = 7) {
  worker_workday %>% filter(ID == currentID) %>% select(-Obs) %>% mutate_at(vars(contains("T")), ~lubridate::dmy_hms(paste0(Fecha, .))) %>% 
    mutate_at(vars(contains("T")), ~floor_date(., unit = "minute")) %>%
    mutate(lunch = T3 - T2, lunch_delta = pmax(0, (as.numeric(lunch) - 30)/60 )) %>%
    mutate( total_workday = T4 - T1 ) %>%
    mutate(workday_total_hours = as.numeric(total_workday) - 9.6 ) %>% 
    mutate(total_workday_with_lunch = workday_total_hours -lunch_delta ) %>%
    mutate(ENTRADA = format(T1, "%H:%M %p"), `SALIDA A ALMUERZO` = format(T2, "%H:%M %p"),
           `ENTRADA ALMUERZO` = format(T3, "%H:%M %p"), `HORA ALMUERZO`=paste0(as.numeric(lunch), " min"),
           `SOBRE EXCESO T/ ALMUERZO` = sprintf("%.2f", as.numeric(lunch_delta)),
           `0.5 HORA ALMUERZO SE COMPUTA TIEMPO EFECTIVO`="12:30 AM",
           `SALIDA` = format(T4, "%H:%M %p"),
           `TIEMPO CONVERTIDO A HORAS`=sprintf("%.2f", total_workday),
           `TIEMPO EXTRAORDINARIO = TIEMPO EFECTIVO - 9.60 HORAS LUNES A VIERNES` = sprintf("%.2f",workday_total_hours),
           `TIEMPO ALMUERZO MAYOR DE 30 MINUTOS` = sprintf("%.2f",total_workday_with_lunch)) %>%
    select(Fecha, ENTRADA, `SALIDA A ALMUERZO`, `ENTRADA ALMUERZO`, `HORA ALMUERZO`, `SOBRE EXCESO T/ ALMUERZO`,
           `0.5 HORA ALMUERZO SE COMPUTA TIEMPO EFECTIVO`, `SALIDA`, `TIEMPO CONVERTIDO A HORAS`,
           `TIEMPO EXTRAORDINARIO = TIEMPO EFECTIVO - 9.60 HORAS LUNES A VIERNES`,
           `TIEMPO ALMUERZO MAYOR DE 30 MINUTOS`) %>%
    pivot_longer(-Fecha, names_to = "PERIODO 2º MAYO 2020", values_to="Hora") %>%
    pivot_wider(names_from = "Fecha", values_from = "Hora")
}

title<-'REPORTE DE HORAS EXTRAS PRODUCCIÓN 2º MAYO 2020'
subtitle <- 'PERIODO: 		11 MAYO AL 25 MAYO 2020	'
subtitle2 <- '1. HORARIOS OPERADORES Y AUXILIARES PRODUCCIÓN'
subtitle3 <- '(JORNADA CONTINUA) 48 HORAS SEMANALES - TURNO DIURNO'

IDS <- worker_workday %>% select(ID) %>% unique

for(i in 1:nrow(IDS) ) {
    
  currentID <- IDS[i,]$ID
  fullname <- worker_id %>% filter(ID == currentID) %>% select(Name)
  
  filename <- paste0("generated/",fullname,".xls")
  wb <- createWorkbook(type="xlsx")
  TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, isBold=TRUE, underline=1)
  SUBTITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=14, isBold=FALSE, underline=0)
  sheet <- createSheet(wb, sheetName = "hoja1")
    
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
    
  xlsx.addTable(wb, sheet, dumpTable(currentID), startCol=2)
    
  xlsx.addLineBreak(sheet, 4)

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

  


        
            


