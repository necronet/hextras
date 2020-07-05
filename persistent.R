library(DBI)
DATABASE_NAME <- "work_schedule_db"

getConnection <- function() {
  dbConnect(RSQLite::SQLite(), ":memory:")  
}

storeInDatabase <- function(workerTimeClock) {
  con <- getConnection()
  worker_time_clock_tbl <- "worker_time_clock"
  persistentData <- NULL
  if (dbExistsTable(con, worker_time_clock_tbl)) {
    print(paste("Table:", worker_time_clock_tbl, "found. Loading persisted data"))
    
    persistentData <- dbReadTable(con, worker_time_clock_tbl)  
    joinColumns <- c("ID","Fecha", "Name","T1","T2","T3","T4")
    
    workerTimeClock <- workerTimeClock %>% inner_join(persistentData, by = joinColumns, suffix = c("_x", "_y")) %>% 
      mutate(Observacion = case_when(!is.na(Observacion_x) ~ `Observacion_x`, T ~ `Observacion_y`),
             incomplete = incomplete_x) %>%
      select(joinColumns, incomplete, Observacion)
    
    workerTimeClock %>% count(ID, Fecha, Name)
    
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
        Observacion TEXT,
        CONSTRAINT PK_worker_time_clock PRIMARY KEY (ID, Fecha)
      )"))
    inserted_count <- dbAppendTable(con, worker_time_clock_tbl, workerTimeClock, append = TRUE)
    print(paste0("Inserted:", inserted_count, " records, into [", worker_time_clock_tbl, "] table"))
  }
  dbDisconnect(con)
  
  return(workerTimeClock)
}
