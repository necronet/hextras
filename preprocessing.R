library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(r2excel)
source('utils.R')

workers <-  read_excel("data/trabajadores_horas_extras.xlsx")

worker_id <- workers %>% filter(!str_detect(Fecha, "\\d{2}\\/\\d{2}\\/\\d{4}")) %>% 
        mutate(worker = Fecha) %>% select(worker) %>% 
        separate(worker, c('ID','Name'), sep = "\\-") %>% mutate(ID = as.integer(ID), Name = str_trim(Name)) %>%
        filter(!(ID %in% IGNORE_WORKERS_IDS))

worker_id %>% filter(ID == 7)

worker_workday <- workers %>% filter(!(ID %in% IGNORE_WORKERS_IDS)) %>% mutate( Registros = ifelse(toupper(Registros) == "VACACIONES", NA, Registros), Obs = `...13` ) %>% 
            filter(str_detect(Fecha, "\\d{2}\\/\\d{2}\\/\\d{4}")) %>% filter(Registros != '--') %>%
            filter(!is.na(Registros)) %>% select(ID, Fecha, Registros, Obs)  %>% 
            separate(Registros, c('T1','T2','T3','T4'), sep = "\\|") %>% mutate_all(str_trim) %>%
            filter(!is.na(T4) & ID %in% c(88) )
            

#lubridate::dmy_hms('11/05/2020 07:01:32')
#round(as.POSIXct(starttime, format="%H:%M %S", tz="UTC"), units="hours")


dumpTable <- function(currentID = 7) {
  worker_workday %>% filter(ID == currentID) %>% select(-Obs) %>% mutate_at(vars(contains("T")), ~lubridate::dmy_hms(paste0(Fecha, .))) %>% 
    mutate_at(vars(contains("T")), ~floor_date(., unit = "minute")) %>%
    mutate(lunch = T3 - T2, lunch_delta = pmax(0, (as.numeric(lunch) - 30)/60 )) %>%
    mutate( total_workday = T4 - T1 ) %>%
    mutate(workday_total_hours = as.numeric(total_workday) - get_worker_daily_hours(currentID) ) %>% 
    mutate(total_workday_with_lunch = workday_total_hours -lunch_delta ) %>%
    mutate(ENTRADA = format(T1, "%H:%M %p"), `SALIDA A ALMUERZO` = format(T2, "%H:%M %p"),
           `ENTRADA ALMUERZO` = format(T3, "%H:%M %p"), `HORA ALMUERZO`=paste0(as.numeric(lunch), " min"),
           `SOBRE EXCESO T/ ALMUERZO` = sprintf("%.2f", as.numeric(lunch_delta)),
           `0.5 HORA ALMUERZO SE COMPUTA TIEMPO EFECTIVO`="12:30 AM",
           `SALIDA` = format(T4, "%H:%M %p"),
           `TIEMPO CONVERTIDO A HORAS`=sprintf("%.2f", total_workday),
           `TIEMPO EXTRAORDINARIO = TIEMPO EFECTIVO - 9 HORAS LUNES A VIERNES` = sprintf("%.2f",workday_total_hours),
           `TIEMPO ALMUERZO MAYOR DE 30 MINUTOS` = sprintf("%.2f",total_workday_with_lunch)) %>%
    select(Fecha, ENTRADA, `SALIDA A ALMUERZO`, `ENTRADA ALMUERZO`, `HORA ALMUERZO`, `SOBRE EXCESO T/ ALMUERZO`,
           `0.5 HORA ALMUERZO SE COMPUTA TIEMPO EFECTIVO`, `SALIDA`, `TIEMPO CONVERTIDO A HORAS`,
           `TIEMPO EXTRAORDINARIO = TIEMPO EFECTIVO - 9 HORAS LUNES A VIERNES`,
           `TIEMPO ALMUERZO MAYOR DE 30 MINUTOS`) %>%
    pivot_longer(-Fecha, names_to = "PERIODO 2º MAYO 2020", values_to="Hora") %>%
    pivot_wider(names_from = "Fecha", values_from = "Hora")
}


IDS <- worker_workday %>% select(ID) %>% unique

for(i in 1:nrow(IDS) ) {
    
  currentID <- IDS[i,]$ID
  fullname <- worker_id %>% filter(ID == currentID) %>% select(Name)
  
  filename <- paste0("generated/",fullname,".xls")
  wb <- createWorkbook(type="xlsx")
  sheet <- createSheet(wb, sheetName = "hoja1")
  
  TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, isBold=TRUE, underline=1)
  SUBTITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=14, isBold=FALSE, underline=0)
  
  builtHeader(wb, sheet)
    
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

  


        
            


