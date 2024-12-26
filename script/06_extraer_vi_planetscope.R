library(terra)
library(tidyverse)
library(fs)
library(glue)

cod_id <- c('hidango_2021-2022','hidango_2022-2023',
            'la_cancha_2022-2023','villa_baviera_2020-2021')
pols <- lapply(cod_id,function(x) {
  vector <- vect('data/processed/sitios.gpkg',layer = glue('muestreo_{x}')) |> 
    project('EPSG:32719')
})
names(pols) <- cod_id

dir_in <- 'data/raw/indices/planetscope/'
vi_files <- lapply(cod_id, \(x) {list.files(glue('{dir_in}{x}/'), full.names=T)})
names(vi_files) <- cod_id

extraer_ps <- \(cod_id) {
  
  sitio <- str_extract(cod_id, ".*(?=_)")
  temporada <- str_extract(cod_id, "\\d{4}-\\d{4}")
  
  pol <- pols[[cod_id]]
  vis <- vi_files[[cod_id]]
  
  lapply(vis, \(x) {
    
    vi_name <- gsub('.tif','',basename(x))
    vi <- rast(x)
    
    terra::extract(vi,pol) |> 
      mutate(sitio = sitio,
             temporada = temporada,
             muestra = as.character(pol$muestra),
             .before = ID) |> 
      select(-ID) |> 
      pivot_longer(cols = -c(sitio,temporada,muestra), names_to = 'fecha', values_to = vi_name) |> 
      mutate(fecha = as.Date(fecha)) |> 
      select(sitio,temporada,fecha,muestra,everything())
    
  }) |> 
    reduce(full_join) |> 
    suppressMessages()
  
}

data_vips <- lapply(cod_id, extraer_ps) |> 
  bind_rows() |> 
  mutate(muestra = case_when(sitio == 'hidango' & temporada == '2021-2022' ~ glue('H1M{muestra}'),
                             sitio == 'hidango' & temporada == '2022-2023' ~ glue('H2M{muestra}'),
                             sitio == 'la_cancha' ~ glue('LCM{muestra}'),
                             sitio == 'villa_baviera' ~ glue('VBM{muestra}')) |> 
           as.character()) |> 
  arrange(sitio,temporada,fecha,muestra) |> 
  write_rds('data/processed/rds/vi_ps.rds')

# comparar series anteriores

data_new <- read_rds('data/processed/rds/vi_ps.rds') |> 
  select(-NDRE) |> 
  pivot_longer(cols = EVI:SAVI, names_to = 'vi', values_to  = 'valor') |> 
  mutate(vi = tolower(vi),
         data = 'new')

data_old <- read_rds('rds/vi_ps.rds') |> 
  mutate(data = 'old')

data <- bind_rows(data_new,data_old) |> 
  mutate(muestra_2 = str_sub(muestra, -1, -1),
         valor = ifelse(valor > 1 | valor < -1, NA, valor))

for (i in seq_along(cod_id)) {
  
  sitio_actual <- str_extract(cod_id[i], ".*(?=_)")
  temporada_actual <- str_extract(cod_id[i], "\\d{4}-\\d{4}")
  
  data |> 
    filter(sitio == sitio_actual,
           temporada == temporada_actual) |> 
    # filter(!fecha %in% as.Date(c('2021-08-19','2021-09-08'))) |> 
    ggplot(aes(fecha,valor,color = data, shape = data)) +
    geom_point(size = 2, alpha = .7) +
    # geom_line() +
    labs(title = glue('Planetscope {cod_id[i]}')) +
    facet_grid(muestra_2~vi) +
    theme_bw()
  
  ggsave(glue('output/figs/series_indices/planetscope/{cod_id[i]}.png'), height = 7, width = 13)
}
