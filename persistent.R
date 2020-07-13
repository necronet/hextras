library(DBI)
DATABASE_NAME <- "work_schedule.sqlite"

getConnection <- function() {
  dbConnect(RSQLite::SQLite(), dbname = DATABASE_NAME)
}

storeInDatabase <- function(workerTimeClock) {
  con <- getConnection()
  worker_time_clock_tbl <- "worker_time_clock"
  persistentData <- NULL
  if (dbExistsTable(con, worker_time_clock_tbl)) {
    print(paste("Table:", worker_time_clock_tbl, "found. Loading persisted data"))
    
    persistentData <- dbReadTable(con, worker_time_clock_tbl)  
    joinColumns <- c("ID","Fecha", "Name")
    selectColumns <- c("ID","Fecha", "Name","T1","T2","T3","T4","incomplete", "Observaciones", "viatico_alimentacion", "viatico_transporte")
    
    workerTimeClock <- workerTimeClock %>% left_join(persistentData, by = joinColumns, suffix = c("_x", "_y")) %>% 
      mutate(Observaciones = case_when(!is.na(Observaciones_x) ~ `Observaciones_x`, T ~ `Observaciones_y`),
             viatico_transporte = case_when(!is.na(viatico_transporte_x) ~ `viatico_transporte_x`, T ~ `viatico_transporte_y`),
             viatico_alimentacion = case_when(!is.na(viatico_alimentacion_x) ~ `viatico_alimentacion_x`, T ~ `viatico_alimentacion_y`),
             T1 = T1_x,
             T2 = T2_x,
             T3 = T3_x,
             T4 = T4_x,
             incomplete = incomplete_x) %>%
      select(selectColumns)
    
    # From https://github.com/r-dbi/DBI/blob/master/R/table-insert.R#L47
    sqlValues <- sqlData(con, workerTimeClock)
    fields <- dbQuoteIdentifier(con, names(sqlValues))
    rows <- do.call(paste, c(sqlValues, sep = ", "))
    sqlInsertOrReplace <- SQL(paste0("insert OR REPLACE into ", 
                                 worker_time_clock_tbl,"\n", "(",
                                 paste(fields, collapse = ", "),")\n",
                                 " values\n",paste("(",rows,")", collapse = ",\n")))
    
    upsert_count <- dbExecute(con,sqlInsertOrReplace)
    print(paste0("Upserted:", upsert_count, " records, into [", worker_time_clock_tbl, "] table"))
    
  } else {
    print(paste("Table:", worker_time_clock_tbl, "NOT found. Creating the table"))
    
    dbExecute(con, paste0("CREATE TABLE ", worker_time_clock_tbl,
    "(
        ID INTEGER , 
        Fecha TEXT,
        Name TEXT,
        T1 TEXT,
        T2 TEXT,
        T3 TEXT,
        T4 TEXT,
        incomplete INTEGER,
        Observaciones TEXT,
        viatico_transporte TEXT,
        viatico_alimentacion TEXT,
        CONSTRAINT PK_worker_time_clock PRIMARY KEY (ID, Fecha)
      )"))
    inserted_count <- dbAppendTable(con, worker_time_clock_tbl, workerTimeClock, append = TRUE)
    print(paste0("Inserted:", inserted_count, " records, into [", worker_time_clock_tbl, "] table"))
  }
  dbDisconnect(con)
  return(workerTimeClock)
}
