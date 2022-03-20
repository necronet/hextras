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
    fill(ID) %>% filter(across(any_of("Entrada"), ~!is.na(.x))) %>% 
    purrr::when(length(workersId) > 0 ~ filter(., ID %in% workersId), T ~ . ) %>%
    filter(!(ID %in% IGNORE_WORKERS_IDS)) %>% 
    mutate(Registros = ifelse(toupper(Registros) == "VACACIONES", NA, Registros), 
              viatico_alimentacion = `VIATICO ALIMENTACION`, 
              viatico_transporte = `VIATICO TRANSPORTE`,
              Observaciones = OBSERVACIONES) %>% 
    filter(str_detect(Fecha, "\\d{2}\\/\\d{2}\\/\\d{4}")) %>% filter(Registros != '--') %>%
    filter(!is.na(Registros)) %>% select(ID, Fecha, Registros, Observaciones, viatico_alimentacion, viatico_transporte) %>%
    separate(Registros, c('T1','T2','T3','T4'), sep = "\\|") %>% mutate_all(str_trim) %>% 
    filter(!is.na(T2)) %>% mutate(ID = as.integer(ID)) %>%
    mutate( incomplete = case_when(is.na(T4) ~ T, T ~ F), 
            T4 = case_when(incomplete ~ T2, T ~ T4),
            T2 = case_when(incomplete ~ NA_character_, T ~ T2)) %>% 
    # when T2 and T3 are not available replace the values with 12:00 and 13:00 range
    mutate(T2 = case_when( is.na(T2) & is.na(T3) ~ "12:00:00", T ~ T2),
           T3 = case_when( is.na(T3) ~ "13:00:00", T ~ T3),
           incomplete = case_when(is.na(T4) ~ T, T ~ F))
}

# Process the source file and transform it into a timeClock tidy data
processTimeClock <- function(workers, workersId = NULL, strickColumns = F) {
  
  #TODO: set this required columns into a config file
  requiredColumns <- c("Fecha", "Registros", "VIATICO ALIMENTACION", "VIATICO TRANSPORTE", "OBSERVACIONES")
  missingColumns <- requiredColumns %in% colnames(workers)
  if ( length(requiredColumns[!missingColumns]) > 0 & strickColumns ) {
    stop(paste("Columnas insuficientes para general el reporte", paste(requiredColumns[!missingColumns], collapse = ',')))
  }
  
  
  worker_id <- workers %>% filter(!str_detect(Fecha, "\\d{2}\\/\\d{2}\\/\\d{4}")) %>% 
    mutate(worker = Fecha) %>% select(worker) %>% 
    separate(worker, c('ID','Name'), sep = "\\-") %>% 
    mutate(ID = as.integer(ID), Name = str_trim(Name)) %>%
    filter(!(ID %in% IGNORE_WORKERS_IDS))
  
  getWorkerTimeClock(workers, workersId)  %>% inner_join(worker_id)
}