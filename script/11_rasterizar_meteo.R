library(terra)
library(tidyverse)
library(glue)

data_clima <- read_rds('data/processed/rds/clima.rds')
data_sm <- read_rds('data/processed/rds/sm.rds')

cod_id <- data |> 
  mutate(cod_id = paste0(sitio,'_',temporada)) |> 
  pull(cod_id) |> 
  unique()

lapply(cod_id,\(x) {
  
  r_base <- rast(glue('data/processed/raster/r_base/{x}.tif'))
  
  data_cod_id <- data_clima |> 
    left_join(data_sm) |> 
    filter(paste0(sitio,'_',temporada) == x)
  
  for (fecha in as.character(data_cod_id$fecha)) {
    pp <- data_cod_id$pp_cumsum[which(data_cod_id$fecha == fecha)]
    sm <- data_cod_id$sm_mm[which(data_cod_id$fecha == fecha)]
    gdd <- data_cod_id$gdd_cumsum[which(data_cod_id$fecha == fecha)]
    
    r_pp <- r_base
    values(r_pp) <- pp
    names(r_pp) <- 'pp_cumsum'
    r_sm <- r_base
    values(r_sm) <- sm
    names(r_sm) <- 'sm_mm'
    r_gdd <- r_base
    values(r_gdd) <- gdd
    names(r_gdd) <- 'gdd_cumsum'
    
    r_meteo <- c(r_pp,r_sm,r_gdd)
    varnames(r_meteo) <- ''
    
    writeRaster(r_meteo,glue('data/processed/raster/indicadores/meteo/{x}/meteo_{fecha}.tif'),
                overwrite=T)
  }
})
