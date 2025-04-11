library(tidyverse)
library(terra)
library(glue)

cod_id <- c('villa_baviera_2020-2021','hidango_2021-2022','hidango_2022-2023','la_cancha_2022-2023')

lapply(cod_id,\(x) {
  
  r_base <- list.files(glue('data/processed/raster/indicadores/sentinel_2/{x}/'),full.names=T)[1] |> 
    rast() |> 
    project('EPSG:32719')
  
  r_base <- r_base[[1]]
  values(r_base) <- NA
  names(r_base) <- x
  varnames(r_base) <- ''
  
  writeRaster(r_base,glue('data/processed/raster/r_base/{x}.tif'),
              overwrite=T)
})
