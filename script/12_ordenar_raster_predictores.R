library(terra)
library(tidyverse)
library(fs)
library(glue)

var_imp <- c('pp_cumsum','gdd_cumsum','sm_mm','S2_B1','S2_B6','S2_MCARI','S2_TCARI','S2_MCARI_OSAVI2',
             'S2_SWIR11_MCARI','S2_SWIR11_TCARI','S2_SWIR12_MCARI','S2_CVI','S2_NDRE3',
             'S2_NDRE_NDVI','S2_WI1','S2_TCARI_OSAVI_8A','S2_CI_red_8A','S2_NDRE3_8A',
             'S2_SIPI_8A','S2_WI1_8A','S1_VV','S1_VH','S1_VH_VV','PS_B3','PS_B8','PS_SR',
             'PS_CVI','PS_EVI','PS_GNDVI','PS_SIPI','PS_CI_red_cumsum','PS_EVI_cumsum',
             'PS_GRVI_cumsum','S2_B1_cumsum','S2_SWIR12_MCARI_cumsum','S2_SWIR12_TCARI_cumsum',
             'S2_NDRE3_cumsum','S2_CI_red_8A_cumsum','S2_NDRE3_8A_cumsum','PS_B5','PS_TCARI_OSAVI',
             'PS_MCARI_OSAVI','PS_NDRE_NDVI')

cod_id <- list.files('data/processed/raster/indicadores/sentinel_2_filled')

lapply(cod_id,\(x) {
  
  print(x)
  
  dir_meteo <- list.files(glue('data/processed/raster/indicadores/meteo/{x}'),full.names=T)
  dir_ps <- list.files(glue('data/processed/raster/indicadores/planetscope_filled/{x}'),full.names=T)
  dir_ps_cumsum <- list.files(glue('data/processed/raster/indicadores/planetscope_filled_cumsum/{x}'),full.names=T)
  dir_s2 <- list.files(glue('data/processed/raster/indicadores/sentinel_2_filled/{x}'),full.names=T)
  dir_s2_cumsum <- list.files(glue('data/processed/raster/indicadores/sentinel_2_filled_cumsum/{x}'),full.names=T)
  dir_s1 <- list.files(glue('data/processed/raster/indicadores/sentinel_1_filled/{x}'),full.names=T)
  
  tif <- c(dir_meteo,dir_s2,dir_ps,dir_ps_cumsum,dir_s2_cumsum,dir_s1)
  fechas <- str_extract(tif,"\\d{4}-\\d{2}-\\d{2}") |> unique() |> sort()
  
  fecha_sos <- read_rds('data/processed/rds/biomasa.rds') |> 
    filter(paste0(sitio,'_',temporada) == x) |> 
    pull(fecha) |> 
    first()
  fecha_eos <- read_rds('data/processed/rds/biomasa.rds') |> 
    filter(paste0(sitio,'_',temporada) == x) |> 
    pull(fecha) |> 
    last()
  
  fechas <- fechas[fechas >= fecha_sos]
  fechas <- fechas[fechas <= fecha_eos]
  
  r_base <- rast(glue('data/processed/raster/r_base/{x}.tif'))
  
  for (fecha in fechas) {
    
    gc()
    
    tif_fecha <- grep(fecha,tif,value=T)
    
    r_fecha <- lapply(tif_fecha, \(i) {
      product_id <- case_when(length(grep('planetscope',i))>0 ~ 'PS_',
                              length(grep('sentinel_2',i))>0 ~ 'S2_',
                              length(grep('sentinel_1',i))>0 ~ 'S1_',
                              .default = '')
      r <- rast(i) |> 
        project(r_base)
      names(r) <- paste0(product_id,names(r))
      r[[which(names(r) %in% var_imp)]]
    }) |> 
      rast()
    
    nlyr_start = length(names(r_fecha))
    
    var_faltantes <- setdiff(var_imp, names(r_fecha))
    
    if (length(var_faltantes) > 0) {
      for (var in var_faltantes) {
        nueva_layer <- r_fecha[[1]]
        values(nueva_layer) <- NA
        names(nueva_layer) <- var
        r_fecha <- c(r_fecha, nueva_layer)
      }
    }
    
    r_fecha <- r_fecha[[match(var_imp, names(r_fecha))]]
    
    dir_out <- glue('data/processed/raster/predictores/{x}/')
    if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
    
    writeRaster(r_fecha,glue('{dir_out}PREDICTORES_{fecha}.tif'),
                overwrite = T)
    
    print(paste0(fecha,' (nlyr: ',nlyr_start,') predictores faltantes: ',
                 paste(var_faltantes, collapse = "; "), ' (nlyr_final: ',nlyr(r_fecha),')'))
  }
})

#visualizar

dir <- 'data/processed/raster/predictores/'
cod_id <- list.files(dir)

lapply(cod_id,\(x) {
  
  sp <- vect(glue('data/processed/shp/puntos_muestreo/{x}.shp'))
  
  tif <- list.files(glue('{dir}{x}'),full.names=T)
  
  dataset_x <- lapply(tif,\(tif_fecha) {
    r <- rast(tif_fecha)
    fecha <- str_extract(sources(r), "\\d{4}-\\d{2}-\\d{2}")
    
    terra::extract(r,sp) |> 
      mutate(fecha = fecha,
             .before = ID) |> 
      select(-ID)
  }) |> 
    bind_rows() |> 
    group_by(fecha) |> 
    reframe(across(everything(),  \(x) mean(x, na.rm = T))) |> 
    pivot_longer(c(everything(),-fecha), values_to = 'value', names_to = 'predictor')
  # pivot_longer(c(everything(),-fecha), values_to = 'value', names_to = 'predictor') |> 
  # group_by(predictor) |> 
  # mutate(value = as.numeric(scale(value)))
  
  dataset_x |> 
    ggplot(aes(as.Date(fecha),value,color=as.factor(predictor))) +
    # geom_point() +
    geom_smooth(se = F,span = .1) +
    theme_bw() +
    theme(legend.position = 'none') +
    labs(title = x, x = NULL, y = 'predictor value')
  # ylim(-1,1)
  
})


