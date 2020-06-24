# main script to generate hextras file
source('preprocessing.R')
source('timesheet_builder.R')

TEST_WORKER_IDS <- c(11)
sourceFile <- "data/trabajadores_horas_extras.xlsx"

workerTimeClock <- processTimeClock(sourceFile, TEST_WORKER_IDS)

source('./worker_time_table.R')
#timeTable(workerTimeClock, 11)
build_timesheet(workerTimeClock)