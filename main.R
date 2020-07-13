# main script to generate hextras file
source('preprocessing.R')
source('persistent.R')
source('timesheet_builder.R')

generateFiles <- function(sourceFile) {
  workerTimeClock <- processTimeClock(sourceFile) %>% 
                     storeInDatabase() %>% 
                     build_timesheet()
}

#generateFiles("data/trabajadores_horas_extras2.xlsx")