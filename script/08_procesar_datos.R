library(tidyverse)

#biomasa

data_bio <- read_rds('data/processed_old/data_biomasa_estructuras.rds') |> 
  as_tibble() |>  
  mutate(sitio = ifelse(site == 'ariztia','la_cancha',site),
         temporada = gsub('_','-',season),
         fecha = as.Date(date_sample),
         muestra = case_when(
           site == 'hidango' ~ gsub('IS','',sample),
           TRUE ~ gsub('S1','',sample))) |> 
  select(sitio, temporada, fecha, muestra,
         biomasa = biomass) |>
  arrange(temporada,sitio,muestra,fecha)

data_bio |> 
  group_by(sitio,temporada) |> 
  reframe(fecha_inicio = min(fecha)) |> 
  arrange(temporada,sitio)

write_rds(data_bio,'data/processed/rds/biomasa.rds')

#fenologia y clima

fechas_feno <- read_rds('data/processed_old/data_fenologia.rds') |>
  as_tibble() |> 
  mutate(sitio = ifelse(site == 'ariztia','la_cancha',site),
         temporada = gsub('_','-',season)) |> 
  select(sitio, temporada,
         fecha = date_sentinel,
         gdd,fenologia = phenology,
         zadoks = code_zadoks) |> 
  group_by(sitio,temporada,fenologia) |> 
  reframe(fecha = min(fecha)) |> 
  arrange(temporada,sitio,fecha) |> 
  select(sitio,temporada,fecha,fenologia)

write_rds(fechas_feno,'data/processed/rds/fechas_fenologia.rds')

sos <- fechas_feno |> 
  filter(fenologia == 'SOWING')

data_clima <- read_rds('data/processed_old/data_clima_new.rds') |>
  left_join(fechas_feno |> filter(fenologia %in% c('SOWING','TRILLERING'))) |> 
  group_by(sitio,temporada) |> 
  mutate(fenologia = ifelse(is.na(fenologia) & lead(fenologia,1) == 'SOWING','OUT',fenologia)) |> 
  mutate(fenologia = ifelse(is.na(fenologia), zoo::na.locf(fenologia, fromLast = TRUE, na.rm = FALSE), fenologia)) |> 
  ungroup() |> 
  filter(fenologia != 'OUT' | is.na(fenologia)) |> 
  mutate(t_base = ifelse(!is.na(fenologia),2,6),
         gdd = t_mean_dia - t_base,
         gdd = ifelse(gdd <0,0,gdd)) |> 
  group_by(sitio,temporada) |> 
  mutate(gdd_cumsum = cumsum(gdd),
         pp_cumsum = cumsum(pp_dia)) |> 
  ungroup() |> 
  select(sitio,temporada,fecha,pp_cumsum,gdd_cumsum)

data_clima |> 
  group_by(sitio,temporada) |> 
  reframe(fecha_inicio = min(fecha)) |> 
  arrange(temporada,sitio)

write_rds(data_clima,'data/processed/rds/clima.rds')

#humedad de suelo

data_sm <- read_rds('data/processed_old/data_soil.rds') |> 
  ungroup() |> 
  mutate(sitio = ifelse(site == 'ariztia','la_cancha',site),
         temporada = gsub('_','-',season),
         fecha = date,
         .before = season) |>
  mutate(temporada = ifelse(sitio == 'la_cancha','2022-2023',temporada),
         mm = case_when(
    sitio == 'hidango' & depth == 15 ~ sm*300,
    sitio == 'hidango' & depth == 50 ~ sm*250,
    sitio == 'hidango' & depth == 75 ~ NA,
    sitio != 'hidango' & depth == 15 ~ sm*200,
    sitio != 'hidango' & depth == 30 ~ sm*150,
    sitio != 'hidango' & depth == 45 ~ sm*200)) |> 
  group_by(sitio,temporada,fecha = as.Date(fecha)) |> 
  reframe(sm_mm = mean(mm,na.rm=T)) |> 
  arrange(temporada,sitio,fecha) |>
  group_by(sitio,temporada) |> 
  mutate(sm_cumsum = cumsum(sm_mm),
         sm_cummean = cummean(sm_mm)) |> 
  ungroup()

data_sm |> 
  group_by(sitio,temporada) |> 
  reframe(fecha_inicio = min(fecha)) |> 
  arrange(temporada,sitio)

write_rds(data_sm,'data/processed/rds/sm.rds')

#indices satelitales

fechas_feno <- read_rds('data/processed/rds/fechas_fenologia.rds')

data_s2 <- read_rds('data/processed/rds/vi_sentinel_2a_filled.rds') |> 
  left_join(fechas_feno |> filter(fenologia %in% c('SOWING','TRILLERING'))) |> 
  group_by(sitio,temporada,muestra) |> 
  mutate(fenologia = ifelse(is.na(fenologia) & lead(fenologia,1) == 'SOWING','OUT',fenologia)) |> 
  mutate(fenologia = ifelse(is.na(fenologia), zoo::na.locf(fenologia, fromLast = TRUE, na.rm = FALSE), fenologia)) |> 
  filter(fenologia != 'OUT' | is.na(fenologia)) |> 
  select(-fenologia) |> 
  mutate(across(EVI:SAVI, cumsum, .names = "{.col}_cumsum")) |> 
  ungroup() |> 
  arrange(temporada,sitio,muestra,fecha)

data_s2 |> 
  group_by(sitio,temporada) |> 
  reframe(fecha_inicio = min(fecha)) |> 
  arrange(temporada,sitio)

write_rds(data_s2,'data/processed/rds/vi_filled.rds')
  


