library(tidyverse)

#cargar datos
item <- c('biomasa','clima','fenologia','gcc','lai','suelo',
          'vi_ps_suavizado','vi_s2_suavizado','vi_s2_bkp_suavizado')

data_list <- lapply(item, \(x) {read_rds(paste0('rds/',x,'.rds'))})
names(data_list) <- item

muestras <- lapply(data_list, \(x) {
  if('muestra' %in% names(x)) {
    x |> 
      pull(muestra) |> 
      unique() |> 
      sort()
  } else {return(NA)}
}) |> 
  unlist() |> 
  unique() |> 
  sort()

sos <- data_list$fenologia |> 
  filter(fenologia == 'SOWING') |> 
  select(sitio,temporada,fecha)

fechas_break <- data_list$biomasa |>
  bind_rows(sos) |> 
  arrange(sitio,temporada,fecha) |> 
  distinct(sitio,temporada,fecha) |> 
  mutate(fecha_break = fecha)

fechas_break |> 
  group_by(sitio,temporada) |> 
  complete(fecha = seq(min(fecha), max(fecha), by = "1 day")) |> 
  left_join()

lapply(data_list,\(x) {
  x |> 
    group_by(sitio,temporada) |> 
    reframe(min = min(fecha),
            max = max(fecha))
})

lapply(data_list,\(x) {head(x)})
