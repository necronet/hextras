# main script to generate hextras file
source('preprocessing.R')
source('persistent.R')
source('timesheet_builder.R')

generateFiles <- function(sourceFile, strickColumns = T, workers_id = NULL) {
  workerTimeClock <- processTimeClock(sourceFile, strickColumns=strickColumns,  workers_id) %>% 
                     storeInDatabase() %>% 
                     build_timesheet()
}

generateFilesFromDb <- function(from = from, to = to) {
  workerTimeClock <- fetchFromDatabase(from = from, to = to) %>% build_timesheet()
}

#processTimeClock(sourceFile, strickColumns=strickColumns,  workers_id)
#generateFiles("data/ene222.xlsx")
#generateFilesFromDb(from = "2021/02/24" , to = "2021/03/09")