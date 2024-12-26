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

muestra <- function(sitio,temporada,sample) {
  
  case_when(sitio == 'hidango')
  
}

#datos
data_bio <- read_rds('data/processed_old/data_biomasa_estructuras.rds')
data_clima <- read_rds('data/processed_old/data_clima.rds')
data_feno <- read_rds('data/processed_old/data_fenologia.rds')
data_gcc <- read_rds('data/processed_old/data_gcc.rds')
data_soil <- read_rds('data/processed_old/data_soil.rds')

#unificar

data_bio |> as_tibble() |>  
  mutate(sitio = ifelse(site == 'ariztia','la_cancha',site),
         temporada = gsub('_','-',season),
         fecha = as.Date(date_sample),
         muestra = case_when(
           site == 'hidango' ~ gsub('IS','',sample),
           TRUE ~ gsub('S1','',sample))) |> 
  select(sitio, temporada, fecha, muestra,
         biomasa = biomass) |> 
  write_rds('rds/biomasa.rds')

data_clima |> 
  mutate(sitio = ifelse(site == 'ariztia','la_cancha',site),
         temporada = gsub('_','-',season)) |> 
  select(sitio, temporada,
         fecha = date,
         pp_mm = precipitacion_mm) |> 
  write_rds('rds/pp.rds')

data_feno |>
  mutate(sitio = ifelse(site == 'ariztia','la_cancha',site),
         temporada = gsub('_','-',season)) |> 
  select(sitio, temporada,
         fecha = date_sentinel,
         gdd,fenologia = phenology) |> 
  write_rds('rds/gdd.rds')

data_gcc |> 
  mutate(sitio = ifelse(site == 'ariztia','la_cancha',site),
         temporada = gsub('_','-',season)) |> 
  select(sitio, temporada,
         fecha = date,
         gcc) |> 
  write_rds('rds/gcc.rds')

data_soil |> 
  ungroup() |> 
  mutate(sitio = ifelse(site == 'ariztia','la_cancha',site),
         temporada = gsub('_','-',season),
         fecha = date,
         mes = month(fecha),
         .before = season) |>
  mutate(mm = case_when(
    sitio == 'hidango' & depth == 15 ~ sm*300,
    sitio == 'hidango' & depth == 50 ~ sm*250,
    sitio == 'hidango' & depth == 75 ~ NA,
    sitio != 'hidango' & depth == 15 ~ sm*200,
    sitio != 'hidango' & depth == 30 ~ sm*150,
    sitio != 'hidango' & depth == 45 ~ sm*200)) |> 
  group_by(sitio,temporada,fecha = as.Date(fecha)) |> 
  reframe(sm_mm = mean(mm,na.rm=T)) |> 
  write_rds('rds/suelo.rds')
