library(terra)
library(tidyverse)
library(fs)
library(glue)

dir <- 'data/processed/raster/indices/'
indices <- list.files(dir)

cod_id <- list.files('data/raw/raster/sentinel_2a')

lapply(indices, \(x) {
  
  cod <- list.files(glue('{dir}{x}'))
  
  lapply(cod, \(y) {
    
    sitio <- str_extract(y, ".*(?=_)")
    temporada <- str_extract(y, "\\d{4}-\\d{4}")
    pol <- vect('data/processed/sitios.gpkg',layer = glue('muestreo_{y}')) |> 
      project('EPSG:32719')
    
    vis <- list.files(glue('{dir}{x}/{y}'),full.name=T)
    
    lapply(vis, \(z) {
      
      vi_name <- gsub('.tif','',basename(z))
      vi <- rast(z)
      
      terra::extract(vi,pol) |> 
        mutate(sitio = sitio,
               temporada = temporada,
               muestra = as.character(pol$muestra),
               muestra = case_when(
                 sitio == 'hidango' & temporada == '2021-2022' ~ glue('H1M{muestra}'),
                 sitio == 'hidango' & temporada == '2022-2023' ~ glue('H2M{muestra}'),
                 sitio == 'la_cancha'  ~ glue('LCM{muestra}'),
                 sitio == 'villa_baviera'  ~ glue('VBM{muestra}')
                 ) |> as.character(),
               .before = ID) |> 
        select(-ID) |> 
        pivot_longer(cols = -c(sitio,temporada,muestra), names_to = 'fecha', values_to = vi_name) |> 
        mutate(fecha = as.Date(fecha)) |> 
        select(sitio,temporada,fecha,muestra,everything())
    }) |> 
      reduce(full_join) |> 
      suppressMessages()
  }) |> 
    bind_rows() |> 
    arrange(temporada,sitio,fecha,muestra) |> 
    write_rds(glue('data/processed/rds/vi_{x}.rds'))
  
  return(glue('VI {x} guardado en: data/processed/rds/vi_{x}.rds'))
})

# comparar series anteriores

graficar_s2 <- \(x,data,output) {
  
  sitio_id <- str_extract(x, ".*(?=_)")
  temporada_id <- str_extract(x, "\\d{4}-\\d{4}")
  
  data |> 
    filter(sitio == sitio_id,
           temporada == temporada_id) |> 
    ggplot(aes(fecha)) +
    geom_point(aes(y = filled, color = "filled")) +
    geom_line(aes(y = filled, color = "filled")) +
    geom_point(aes(y = raw, color = "raw")) +
    facet_wrap(~vi,nrow=4, scales = 'free_y') +
    scale_color_manual(
      name = "", 
      values = c("filled" = "#00BFC4", "raw" = "#F8766D")
    ) + 
    labs(title = glue('Sentinel 2A raw/filled   - {sitio_id} {temporada_id}'),
         x = NULL, y = NULL) +
    theme_bw()
  
  ggsave(glue('{output}/{x}.png'), width = 18, height = 10)
  
}

data_raw <- read_rds(glue('data/processed/rds/vi_sentinel_2a.rds')) |> 
  mutate(type = 'raw')
data_fill <- read_rds(glue('data/processed/rds/vi_sentinel_2a_filled.rds')) |> 
  mutate(type = 'filled')

data <- bind_rows(data_raw,data_fill) |> 
  pivot_longer(cols=c(B1:NDWI,-type),names_to = 'vi', values_to = 'value') |> 
  pivot_wider(names_from = type,values_from=value) |> 
  group_by(temporada,sitio,fecha,vi) |> 
  reframe(raw = mean(raw,na.rm=T),
          filled = mean(filled,na.rm=T))

graficar_s2('hidango_2021-2022',data,'output/figs/series/s2_raw_filled')
graficar_s2('hidango_2022-2023',data,'output/figs/series/s2_raw_filled')
graficar_s2('la_cancha_2022-2023',data,'output/figs/series/s2_raw_filled')
graficar_s2('villa_baviera_2020-2021',data,'output/figs/series/s2_raw_filled')
