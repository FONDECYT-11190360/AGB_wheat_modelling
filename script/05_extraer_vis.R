library(terra)
library(tidyverse)
library(fs)
library(glue)

dir_ps <- list.files('data/processed/raster/indicadores/planetscope_filled',full.names=T)
dir_ps_cumsum <- list.files('data/processed/raster/indicadores/planetscope_filled_cumsum',full.names=T)

dir_s2 <- list.files('data/processed/raster/indicadores/sentinel_2_filled',full.names=T)
dir_s2_cumsum <- list.files('data/processed/raster/indicadores/sentinel_2_filled_cumsum',full.names=T)

dir_s1 <- list.files('data/processed/raster/indicadores/sentinel_1_filled',full.names=T)

tif <- c(dir_ps,dir_ps_cumsum,
         dir_s2,dir_s2_cumsum,
         dir_s1)

all_tif <- lapply(tif,\(dir) list.files(dir,full.names=T)) |> unlist()

fechas <- str_extract(all_tif,"\\d{4}-\\d{2}-\\d{2}") |> unique() |> sort()

pol <- lapply(list.files('data/processed/shp/puntos_muestreo',full.names=T, pattern = 'shp'), vect)
names(pol) <- lapply(pol,sources) |> unlist() |> basename() |> gsub(pattern = '.shp',replacement = '')

data <- list()

for (fecha in fechas) {
  
  gc()
  
  print(fecha)
  tif_fecha <- grep(fecha,all_tif,value=T)
  
  product_id <- lapply(tif_fecha, \(x) {
    case_when(length(grep('planetscope',x))>0 ~ 'PS',
              length(grep('sentinel_2',x))>0 ~ 'S2',
              length(grep('sentinel_1',x))>0 ~ 'S1',
              .default = '')
  }) |> unlist()
  
  cod_id <- lapply(tif_fecha, \(x) {
    str_extract(x, '(hidango_\\d{4}-\\d{4}|la_cancha_\\d{4}-\\d{4}|villa_baviera_\\d{4}-\\d{4})')
  }) |> unlist()
  
  v_extract <- list()
  
  for (i in seq_along(tif_fecha)) {
    
    gc()
    
    r <- rast(tif_fecha[i]) |> 
      project('EPSG:32719')
    names(r) <- paste0(product_id[i],'_',names(r))
    
    pol_id <- pol[[cod_id[i]]]
    
    sitio <- str_extract(cod_id[i], ".*(?=_)")
    temporada <- str_extract(cod_id[i], "\\d{4}-\\d{4}")
    
    v_extract[[i]] <- terra::extract(r,pol_id) |> 
      mutate(sitio = sitio,
             temporada = temporada,
             fecha = as.Date(fecha),
             muestra = as.character(pol_id$muestra),
             muestra = case_when(
               sitio == 'hidango' & temporada == '2021-2022' ~ glue('H1M{muestra}'),
               sitio == 'hidango' & temporada == '2022-2023' ~ glue('H2M{muestra}'),
               sitio == 'la_cancha'  ~ glue('LCM{muestra}'),
               sitio == 'villa_baviera'  ~ glue('VBM{muestra}')
             ) |> as.character(),
             .before = ID) |> 
      select(-ID)
  }
  
    grouped <- split(v_extract, map_chr(v_extract, ~ paste(unique(.$sitio), unique(.$temporada), sep = "_")))
    data[[fecha]] <- map(grouped, \(tbls) {
      reduce(tbls, left_join, by = c("sitio", "temporada", "fecha", "muestra"))
    }) |> bind_rows()
  
}

data |> 
  bind_rows() |> 
  as_tibble() |> 
  arrange(temporada,sitio,fecha,muestra) |> 
  write_rds('data/processed/rds/vi_extract.rds')

#visualizar

data <- read_rds('data/processed/rds/vi_extract.rds')

id_s2 <- setdiff(grep('S2',names(data),value=T),grep('cumsum',names(data),value=T))
id_ps <- setdiff(grep('PS',names(data),value=T),grep('cumsum',names(data),value=T))
 
id_s2accu <- grep('cumsum',grep('S2',names(data),value=T),value=T)
id_psaccu <- grep('cumsum',grep('PS',names(data),value=T),value=T)

id_s1 <- grep('S1',names(data),value=T)

graficar_index <- \(df,id,id_name) {
  
  lapply(id, \(x) {
    df |> 
      select(sitio:muestra,all_of(x)) |> 
      ggplot(aes(as.Date(fecha),!!sym(x),color = as.factor(muestra))) +
      # geom_point() +
      geom_line() +
      facet_grid(~sitio+temporada, scales = 'free_x') +
      theme_bw() +
      theme(legend.position = 'none')
    
    ggsave(glue('output/figs/series/series_finales/{id_name}/{x}.png'), height = 5,width = 10)
  })
}

graficar_index(data, id = id_s2,id_name = 'S2')
graficar_index(data, id = id_ps,id_name = 'PS')
graficar_index(data, id = id_s2accu,id_name = 'S2_ACCU')
graficar_index(data, id = id_psaccu,id_name = 'PS_ACCU')
graficar_index(data, id = id_s1,id_name = 'S1')
