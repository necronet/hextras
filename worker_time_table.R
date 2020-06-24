
timeTable <- function(worker_workday, currentID = 190) {
  library(lubridate)
  
  worker_workday %>% filter(ID == currentID) %>% mutate_at(vars(contains("T")), ~lubridate::dmy_hms(paste0(Fecha, .))) %>% 
    mutate_at(vars(contains("T")), ~floor_date(., unit = "minute")) %>%
    mutate(lunch = T3 - T2, lunch_delta = pmax(0, (as.numeric(lunch) - 30)/60 )) %>%
    mutate( total_workday = T4 - T1 ) %>%
    mutate(
      workday_total_hours = as.numeric(total_workday),
      workday_total_hours = workday_total_hours - map_dbl(Fecha, ~get_worker_daily_hours(currentID, . ))) %>% 
    mutate(total_workday_with_lunch = workday_total_hours -lunch_delta ) %>%
    mutate(ENTRADA = format(T1, "%H:%M %p"), `SALIDA A ALMUERZO` = format(T2, "%H:%M %p"),
           `ENTRADA ALMUERZO` = format(T3, "%H:%M %p"), `HORA ALMUERZO`=paste0(as.numeric(lunch), " min"),
           `SOBRE EXCESO T/ ALMUERZO` = sprintf("%.2f", as.numeric(lunch_delta)),
           `SALIDA` = format(T4, "%H:%M %p"),
           `TIEMPO CONVERTIDO A HORAS`=sprintf("%.2f", total_workday),
           `TIEMPO EXTRA = TE - 9.6` = sprintf("%.2f",workday_total_hours),
           `TIEMPO EXTRA/ FALTANTE` = sprintf("%.2f",total_workday_with_lunch),
           `VIATICO ALIMENTACION` = "", `VIATICO TRANSPORTE` = "") %>%
    select(Fecha, ENTRADA, `SALIDA A ALMUERZO`, `ENTRADA ALMUERZO`, `HORA ALMUERZO`, `SOBRE EXCESO T/ ALMUERZO`,
           `SALIDA`, `TIEMPO CONVERTIDO A HORAS`,
           `TIEMPO EXTRA = TE - 9.6`,
           `TIEMPO EXTRA/ FALTANTE`)
    #pivot_longer(-Fecha, names_to = get_header_text(.$Fecha), values_to="Hora") %>%
    #pivot_wider(names_from = "Fecha", values_from = "Hora")
}