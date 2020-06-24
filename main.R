# main script to generate hextras file
source('preprocessing.R')
source('timesheet_builder.R')

TEST_WORKER_IDS <- c(11)
sourceFile <- "data/trabajadores_horas_extras.xlsx"

workerTimeClock <- process(sourceFile, TEST_WORKER_IDS)

build_timesheet(workerTimeClock)