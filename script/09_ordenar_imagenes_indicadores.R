library(terra)
library(tidyverse)
library(fs)
library(glue)

var_imp <- c('pp_cumsum','sm_mm','S2_B1','S2_B6','S2_MCARI','S2_TCARI','S2_MCARI_OSAVI2',
             'S2_SWIR11_MCARI','S2_SWIR11_TCARI','S2_SWIR12_MCARI','S2_CVI','S2_NDRE3',
             'S2_NDRE_NDVI','S2_WI1','S2_TCARI_OSAVI_8A','S2_CI_red_8A','S2_NDRE3_8A',
             'S2_SIPI_8A','S2_WI1_8A','S1_VV','S1_VH','S1_VH_VV','PS_B3','PS_B8','PS_SR',
             'PS_CVI','PS_EVI','PS_GNDVI','PS_SIPI','PS_CI_red_cumsum','PS_EVI_cumsum',
             'PS_GRVI_cumsum','S2_B1_cumsum','S2_SWIR12_MCARI_cumsum','S2_SWIR12_TCARI_cumsum',
             'S2_NDRE3_cumsum','S2_CI_red_8A_cumsum','S2_NDRE3_8A_cumsum','PS_B5','PS_TCARI_OSAVI',
             'PS_MCARI_OSAVI','PS_NDRE_NDVI')

cod_id <- list.files('data/processed/raster/indices/sentinel_2_filled')

lapply(cod_id,\(x) {
  
  dir_ps <- list.files(glue('data/processed/raster/indices/planetscope_filled/{x}'),full.names=T)
  dir_ps_cumsum <- list.files(glue('data/processed/raster/indices/planetscope_filled_cumsum/{x}'),full.names=T)
  
  dir_s2 <- list.files(glue('data/processed/raster/indices/sentinel_2_filled/{x}'),full.names=T)
  dir_s2_cumsum <- list.files(glue('data/processed/raster/indices/sentinel_2_filled_cumsum/{x}'),full.names=T)
  
  dir_s1 <- list.files(glue('data/processed/raster/indices/sentinel_1_filled/{x}'),full.names=T)
  
  tif <- c(dir_s2,dir_ps,dir_ps_cumsum,dir_s2_cumsum,dir_s1)
  
  fechas <- str_extract(tif,"\\d{4}-\\d{2}-\\d{2}") |> unique() |> sort()
  
  fecha_sos <- read_rds('data/processed/rds/fechas_fenologia.rds') |> 
    filter(fenologia == 'SOWING',
           paste0(sitio,'_',temporada) == x) |> 
    pull(fecha)
  
  fecha_eos <- read_rds('data/processed/rds/fechas_fenologia.rds') |> 
    filter(fenologia == 'RIPENING',
           paste0(sitio,'_',temporada) == x) |> 
    pull(fecha)
  
  fechas <- fechas[fechas >= fecha_sos]
  fechas <- fechas[fechas <= fecha_eos]
  
  for (fecha in fechas) {
    
    tif_fecha <- grep(fecha,tif,value=T)
    
    
    
  }
  
  
  
})





dir_ps <- list.files('data/processed/raster/indices/planetscope_filled',full.names=T)
dir_ps_cumsum <- list.files('data/processed/raster/indices/planetscope_filled_cumsum',full.names=T)

dir_s2 <- list.files('data/processed/raster/indices/sentinel_2_filled',full.names=T)
dir_s2_cumsum <- list.files('data/processed/raster/indices/sentinel_2_filled_cumsum',full.names=T)

dir_s1 <- list.files('data/processed/raster/indices/sentinel_1_filled',full.names=T)

tif <- c(dir_s2,dir_ps,
         dir_ps_cumsum,dir_s2_cumsum,
         dir_s1)

all_tif <- lapply(tif,\(dir) list.files(dir,full.names=T)) |> unlist()

fechas <- str_extract(all_tif,"\\d{4}-\\d{2}-\\d{2}") |> unique() |> sort()

lapply(fechas, \(fecha) {
  
  tif_fecha <- grep(fecha,all_tif,value=T)
  
  r_fecha <- lapply(tif_fecha, \(x) {
    product_id <- case_when(length(grep('planetscope',x))>0 ~ 'PS',
                            length(grep('sentinel_2',x))>0 ~ 'S2',
                            length(grep('sentinel_1',x))>0 ~ 'S1')
    name_id <- case_when(length(grep('hidango_2021-2022',x))>0 ~ 'H1',
                         length(grep('higango_2022-2023',x))>0 ~ 'H2',
                         length(grep('la_cancha_2022-2023',x))>0 ~ 'LC',
                         length(grep('villa_baviera_2020-2021',x))>0 ~ 'VB')
    subset <- case_when(length(grep('cumsum',x))>0 ~ '_cumsum',
                        .default = '')
    
    r <- rast(tif_fecha)
    names(r) <- paste0(product_id,'_',names(r),subset)
    
  })
  
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
