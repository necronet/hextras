# main script to generate hextras file
source('preprocessing.R')
source('persistent.R')
source('timesheet_builder.R')
library(aws.s3)

generateFiles <- function(sourceFile, bucket = NULL, isAWS = F, strickColumns = T, workers_id = NULL) {
  if (isAWS) {
    # Aws runtime
    workers <- s3read_using(FUN = read_excel, bucket = bucket, object = sourceFile) 
  } else {
    # Non aws runtime
    workers <- read_excel(sourceFile)
  }
  
  fileName <- processTimeClock(workers, strickColumns=strickColumns,  workers_id) %>% build_timesheet()
  
  if (isAWS) {
    ## Upload to s3
    put_object(
      file = fileName, 
      object = fileName, 
      bucket = bucket
    )
  }
  return (fileName)
  #workerTimeClock <- processTimeClock(workers, strickColumns=strickColumns,  workers_id) %>% 
  #                   storeInDatabase() %>% 
  #                   build_timesheet()
}

generateFilesFromDb <- function(from = from, to = to) {
  workerTimeClock <- fetchFromDatabase(from = from, to = to) %>% build_timesheet()
}

#processTimeClock(sourceFile, strickColumns=strickColumns,  workers_id)
#generateFiles("data/ene222.xlsx")
#generateFilesFromDb(from = "2021/02/24" , to = "2021/03/09")