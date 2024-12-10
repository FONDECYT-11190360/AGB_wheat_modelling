library(tidyverse)

#cargar datos
item <- c('biomasa','clima','fenologia','gcc','lai','suelo',
          'vi_ps_suavizado','vi_s2_suavizado','vi_s2_bkp_suavizado')

data_list <- lapply(item, \(x) {read_rds(paste0('rds/',x,'.rds'))})
names(data_list) <- item

sos <- data_list$fenologia |> 
  filter(fenologia == 'SOWING') |> 
  select(sitio,temporada,fecha)

fechas_break <- data_list$biomasa |>
  bind_rows(sos) |> 
  arrange(sitio,temporada,fecha) |> 
  distinct(sitio,temporada,fecha) |> 
  group_by(sitio, temporada) |> 
  complete(fecha = seq(min(fecha), max(fecha), by = "1 day"))

lapply(data_list,\(x) {
  x |> 
    group_by(sitio,temporada) |> 
    reframe(min = min(fecha),
            max = max(fecha))
})

lapply(data_list,\(x) {head(x)})

x <- round(runif(20,0,10),1)

df <- tibble(g = rep(1:2,each=10),i = rep(c(1,rep(NA,4),2,rep(NA,3),3),2)) |> 
  mutate(x=1)

df |> 
  group_by(g) |> 
  mutate(i_1 = ifelse(i<=1,1,NA),
         i_2 = ifelse(i<=2,2,NA),
         i_3 = ifelse(i<=3,3,NA)) |>
  pivot_longer(cols=contains('i_'), names_to = NULL, 
               values_to = 'i_acum') |> 
  ungroup() |> 
  na.omit() |> 
  group_by(g,i_acum) |> 
  reframe(x_acum = sum(x))

df |>
  distinct(g,i) |> 
  expand_grid(df)





data_list$clima |> 
  left_join(fechas_break, by = c("sitio", "temporada")) |>  # Combina con fechas_break
  arrange(sitio, temporada, fecha) |>  # Ordena los datos
  group_by(sitio, temporada) %>% # Agrupa por sitio y temporada
  mutate(
    break_id = findInterval(fecha, sort(unique(fechas))), # Identifica los intervalos
    valor_acumulado = cumsum(valor) # Acumula valores dentro de cada grupo
  ) %>%
  group_by(sitio, temporada, break_id) %>%
  summarise(
    fecha_inicio = min(fecha),
    fecha_fin = max(fecha),
    valor_acumulado = max(valor_acumulado, na.rm = TRUE), # Toma el máximo acumulado en el intervalo
    .groups = "drop"
  )