# main script to generate hextras file
source('preprocessing.R')
source('persistent.R')
source('timesheet_builder.R')
library(aws.s3)
library(logger)

data <- NULL

generateFiles <- function(sourceFile, bucket = NULL, isAWS = F, strickColumns = T, workers_id = NULL) {
  if (isAWS) {
    # Aws runtime
    log_info("Reading from S3 buckets")
    workers <- s3read_using(FUN = read_excel, bucket = bucket, object = sourceFile) 
  } else {
    # Non aws runtime
    workers <- read_excel(sourceFile)
  }
  
  log_info("Starting processing time clock")
  fileName <- processTimeClock(workers, strickColumns=strickColumns,  workers_id) %>% build_timesheet()
 
  if (isAWS) {
      ## Upload to s3
      put_object(file = fileName, object = fileName, bucket = bucket)
  }
  
  return (fileName)
  #workerTimeClock <- processTimeClock(workers, strickColumns=strickColumns,  workers_id) %>% 
  #                   storeInDatabase() %>% 
  #                   build_timesheet()
}
