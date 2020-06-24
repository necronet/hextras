# main script to generate hextras file
source('preprocessing.R')
source('timesheet_builder.R')

generateFiles <- function(sourceFile) {
  workerTimeClock <- processTimeClock(sourceFile)
  build_timesheet(workerTimeClock)  
}


generateFiles("data/trabajadores_horas_extras.xlsx")