library(terra)
library(tidyverse)
library(fs)
library(glue)

cod_id <- list.files('data/processed/raster/indices/planetscope_filled')

dir_ps <- list.files('data/processed/raster/indices/planetscope_filled',full.names=T)
dir_ps_cumsum <- list.files('data/processed/raster/indices/planetscope_filled_cumsum',full.names=T)

dir_s2 <- list.files('data/processed/raster/indices/sentinel_2_filled',full.names=T)
dir_s2_cumsum <- list.files('data/processed/raster/indices/sentinel_2_filled_cumsum',full.names=T)

dir_s1 <- list.files('data/processed/raster/indices/sentinel_1_filled',full.names=T)

tif <- c(dir_ps,dir_ps_cumsum,
         dir_s2,dir_s2_cumsum,
         dir_s1)

all_tif <- lapply(tif,\(dir) list.files(dir,full.names=T)) |> unlist()

fechas <- str_extract(all_tif,"\\d{4}-\\d{2}-\\d{2}") |> unique()

x = grep('sentinel_1',all_tif,value=T)[20]
x = grep('sentinel_2_filled/',all_tif,value=T)[20]
x = grep('planetscope_filled/',all_tif,value=T)[20]
x = grep('sentinel_2_filled_cumsum',all_tif,value=T)[20]
x = grep('planetscope_filled_cumsum',all_tif,value=T)[20]

lapply(fechas, \(fecha) {
  
  tif_fecha <- grep(fecha,all_tif,value=T)
  
  product_id <- lapply(tif_fecha, \(x) {
    case_when(length(grep('planetscope',x))>0 ~ 'PS',
              length(grep('sentinel_2',x))>0 ~ 'S2',
              length(grep('sentinel_1',x))>0 ~ 'S1')
  }) |> unlist()
  
  cod_id <- lapply()
  
  lapply(tif_fecha, \(x) {
    product_id <- case_when(length(grep('planetscope',x))>0 ~ 'PS',
                            length(grep('sentinel_2',x))>0 ~ 'S2',
                            length(grep('sentinel_1',x))>0 ~ 'S1')
    name_id <- case_when(length(grep('hidango_2021-2022',x))>0 ~ 'H1',
                         length(grep('higango_2022-2023',x))>0 ~ 'H2',
                         length(grep('la_cancha_2022-2023',x))>0 ~ 'LC',
                         length(grep('villa_baviera_2020-2021',x))>0 ~ 'VB')
    
    subset <- case_when(length(grep('cumsum',x))>0 ~ 'ACC',
                        .default = '')
    return(c(product_id,name_id,subset))
  })
  
  
  
  
  
})







lapply(all_tif, \(x) {
  
  fecha <- str_extract(x,"\\d{4}-\\d{2}-\\d{2}")
  
  product_id <- case_when(length(grep('planetscope',x))>0 ~ 'PS',
                          length(grep('sentinel_2',x))>0 ~ 'S2',
                          length(grep('sentinel_1',x))>0 ~ 'S1')
  
  name_id <- case_when(length(grep('hidango_2021-2022',x))>0 ~ 'H1',
                       length(grep('higango_2022-2023',x))>0 ~ 'H2',
                       length(grep('la_cancha_2022-2023',x))>0 ~ 'LC',
                       length(grep('villa_baviera_2020-2021',x))>0 ~ 'VB')
  
  subset <- case_when(length(grep('cumsum',x))>0 ~ 'ACC',
                      .default = '')
  
  glue('data/processed/raster/indices/dataset/{product_id}{name_id}{subset}_{fecha}.tif')
  
  r <- rast(x) |> 
    project('EPSG:32719')
  
  names(r) <- paste0(product_id,'_',names(r))
  
  writeRaster(r,glue('data/processed/raster/indices/dataset/{product_id}_{name_id}_{fecha}.tif'))
  
})
