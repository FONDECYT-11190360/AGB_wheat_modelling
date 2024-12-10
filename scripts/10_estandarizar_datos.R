library(tidyverse)

fecha_a_temporada <- function(fecha) {
  
  año <- year(fecha)
  mes <- month(fecha)
  
  if (mes < 5) {
    primer_año <- año -1
    ultimo_año <- año
  } else {
    primer_año <- año
    ultimo_año <- año + 1
  }
  
  temporada <- paste0(primer_año,'-',ultimo_año)
  return(temporada)
  
}

#datos
data_bio <- read_rds('data/data_processed/data_biomasa_estructuras.rds')
data_clima <- read_rds('data/data_processed/data_clima.rds')
data_feno <- read_rds('data/data_processed/data_fenologia.rds')
data_gcc <- read_rds('data/data_processed/data_gcc.rds')
data_lai <- read_rds('data/data_processed/data_lai.rds')
data_soil <- read_rds('data/data_processed/data_soil.rds')
data_visps <- read_rds('data/data_processed/data_vis_planetscope.rds')
data_viss2 <- read_rds('data/data_processed/data_vis_sentinel2.rds')
data_viss22 <- read_rds('data/data_processed/data_vis_sentinel2_bkp.rds')

#unificar

data_bio |> 
  mutate(site = ifelse(site == 'ariztia','la_cancha',site),
         season = gsub('_','-',season),
         sample = gsub('M','',sample)) |> 
  select(sitio = site,
         temporada = season,
         fecha = date_sample,
         fecha_s2 = date_sentinel,
         muestra = sample,
         biomasa = biomass) |> 
  write_rds('rds/biomasa.rds')

data_clima |> 
  mutate(site = ifelse(site == 'ariztia','la_cancha',site),
         season = gsub('_','-',season)) |> 
  select(sitio = site,
         temporada = season,
         fecha = date,
         precipitacion_mm,temp_c) |> 
  write_rds('rds/clima.rds')

data_feno |>
  mutate(site = ifelse(site == 'ariztia','la_cancha',site),
         season = gsub('_','-',season)) |> 
  select(sitio = site,
         temporada = season,
         fecha = date_sentinel,
         gdd,fenologia = phenology,cod_zadoks = code_zadoks) |> 
  write_rds('rds/fenologia.rds')

data_gcc |> 
  mutate(site = ifelse(site == 'ariztia','la_cancha',site),
         season = gsub('_','-',season)) |> 
  select(sitio = site,
         temporada = season,
         fecha = date,
         gcc) |> 
  write_rds('rds/gcc.rds')

data_lai |>
  mutate(site = ifelse(site == 'ariztia','la_cancha',site),
         season = gsub('_','-',season),
         sample = gsub('M','',sample)) |> 
  select(sitio = site,
         temporada = season,
         fecha = date_sample,
         fecha_s2 = date_sentinel,
         muestra = sample,
         lai_cept,lai_manual) |> 
  write_rds('rds/lai.rds')

data_soil |>
  filter(hour(date) %in% 15:16) |> 
  mutate(site = ifelse(site == 'ariztia','la_cancha',site),
         season = gsub('_','-',season)) |> 
  group_by(site,season,fecha = as.Date(date),depth) |> 
  reframe(sm = mean(sm,na.rm=T),
          temp = mean(temp,na.rm=T)) |>
  select(sitio = site,
         temporada = season,
         fecha,
         profundidad = depth,
         sm,temp) |>  
  write_rds('rds/suelo.rds')

data_visps |>
  mutate(site = ifelse(site == 'ariztia','la_cancha',site),
         dates = as.Date(dates),
         sample = as.character(sample),
         season = gsub('_','-',season)) |> 
  select(sitio = site,
         temporada = season,
         fecha = dates,
         muestra = sample,
         vi,valor = value) |> 
  write_rds('rds/vi_ps.rds')

data_viss2 |>
  mutate(sitio = ifelse(sitio == 'ariztia','la_cancha',sitio),
         index = tolower(index),
         season = gsub('_','-',season),
         value = ifelse(index != 'kndvi',value/10000,value)) |> 
  select(sitio,
         temporada = season,
         fecha = date,
         muestra = sample,
         vi = index,
         valor = value) |> 
  write_rds('rds/vi_s2.rds')

data_viss22 |>
  rowwise() |> 
  mutate(sitio = ifelse(sitio == 'ariztia','la_cancha',sitio),
         index = tolower(index),
         season = fecha_a_temporada(date),
         value = ifelse(index != 'kndvi',value/10000,value)) |> 
  select(sitio,
         temporada = season,
         fecha = date,
         muestra = sample,
         vi = index,
         valor = value) |> 
  write_rds('rds/vi_s2_bkp.rds')
