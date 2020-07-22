# main script to generate hextras file
source('preprocessing.R')
source('persistent.R')
source('timesheet_builder.R')

generateFiles <- function(sourceFile, strickColumns = T) {
  workerTimeClock <- processTimeClock(sourceFile, strickColumns=) %>% 
                     storeInDatabase() %>% 
                     build_timesheet()
}