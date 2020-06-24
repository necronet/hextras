library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
source('utils.R')

## From raw data it will get the worker list of ids
getWorkersId <-function(workerData, workersId) {
  workerData %>% filter(!str_detect(Fecha, "\\d{2}\\/\\d{2}\\/\\d{4}")) %>% 
    mutate(worker = Fecha) %>% select(worker) %>% 
    separate(worker, c('ID','Name'), sep = "\\-") %>% mutate(ID = as.integer(ID), Name = str_trim(Name)) %>%
    filter(!(ID %in% IGNORE_WORKERS_IDS)) %>%
    { ifelse(length(workersId) > 0 , filter(., ID %in% workersId), . ) }
  
}

# From raw data it will provide the timeclock of the workers
getWorkerTimeClock <- function(workerData, workersId = NULL){
  workerData %>% 
    mutate(ID=case_when(
      grepl(x=Fecha, pattern='^([0-9]+) .+') ~ gsub(x=Fecha, pattern='^([0-9]+) .+', replacement='\\1'), 
      TRUE ~ NA_character_)) %>%
    fill(ID) %>% filter(!is.na(Entrada)) %>% 
    purrr::when(length(workersId) > 0 ~ filter(., ID %in% workersId), T ~ . ) %>%
    filter(!(ID %in% IGNORE_WORKERS_IDS)) %>% 
    mutate(Registros = ifelse(toupper(Registros) == "VACACIONES", NA, Registros)) %>% 
    filter(str_detect(Fecha, "\\d{2}\\/\\d{2}\\/\\d{4}")) %>% filter(Registros != '--') %>%
    filter(!is.na(Registros)) %>% select(ID, Fecha, Registros) %>%
    separate(Registros, c('T1','T2','T3','T4'), sep = "\\|") %>% mutate_all(str_trim) %>%
    filter(!is.na(T2)) %>% mutate(ID = as.integer(ID)) %>%
    mutate( incomplete = case_when(is.na(T4) ~ T, T ~ F), T4 = case_when(incomplete ~ T2, T ~ T4) )
}

# Process the source file and transform it into a timeClock tidy data
processTimeClock <- function(sourceFile, workersId = NULL) {
  workers <-  read_excel(sourceFile)
  
  worker_id <- workers %>% filter(!str_detect(Fecha, "\\d{2}\\/\\d{2}\\/\\d{4}")) %>% 
    mutate(worker = Fecha) %>% select(worker) %>% 
    separate(worker, c('ID','Name'), sep = "\\-") %>% 
    mutate(ID = as.integer(ID), Name = str_trim(Name)) %>%
    filter(!(ID %in% IGNORE_WORKERS_IDS))
  
  getWorkerTimeClock(workers, workersId)  %>% inner_join(worker_id)
}