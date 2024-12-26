library(terra)
library(tidyverse)
library(fs)
library(glue)

cod_id <- list.files('data/raw/planetscope/')

dir_in <- 'data/raw/planetscope/'
dir_out <- 'data/raw/indices/planetscope/'

lapply(cod_id, \(x) {
  
  tif_b <- grep('Analytic',list.files(glue('{dir_in}{x}/'),full.names=T),value=T)
  tif_c <- grep('udm',list.files(glue('{dir_in}{x}/'),full.names=T),value=T)
  dates <- unique(as.Date(str_extract(basename(tif_b), "^\\d{8}"), format = "%Y%m%d"))
  
  if (x %in% cod_id[3:4]) {
    r_base <- rast(tif_b[1])
    r <- rast(lapply(tif_b,\(x) {project(rast(x),r_base)}))
    r_c <- rast(lapply(tif_c,\(x) {project(rast(x),r_base,method = 'near')}))
  } else {
    r <- rast(tif_b)
    r_c <- rast(tif_c)
  }
  
  mask <- r_c[[grep('clear',names(r_c))]] |> 
    classify(matrix(c(0,1,NA,1),nrow=2,ncol=2))
  
  band_2 <- r[[which(names(r) == 'blue')]]/10000*mask
  band_4 <- r[[which(names(r) == 'red')]]/10000*mask
  band_8 <- r[[which(names(r) == 'nir')]]/10000*mask
  
  NDVI <- (band_8-band_4)/(band_8+band_4)
  SAVI <- (band_8-band_4)/(band_8+band_4+.5)*(1+.5)
  EVI <- 2.5*(band_8-band_4)/((band_8+6*band_4-7.5*band_2)+1)
  KNR <- exp(-(band_8-band_4)^2/(2*.15^2))
  KNDVI <- (1-KNR)/(1+KNR)
  
  names(NDVI) <- dates
  names(SAVI) <- dates
  names(EVI) <- dates
  names(KNDVI) <- dates
  
  output_dir <- glue("{dir_out}{x}/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  writeRaster(NDVI,glue('{output_dir}NDVI.tif'), overwrite = T)
  writeRaster(SAVI,glue('{output_dir}SAVI.tif'), overwrite = T)
  writeRaster(EVI,glue('{output_dir}EVI.tif'), overwrite = T)
  writeRaster(KNDVI,glue('{output_dir}KNDVI.tif'), overwrite = T)
  
  if (!x %in% cod_id[c(1,4)]) {
    band_5 <- r[[which(names(r) == 'rededge')]]/10000*mask
    NDRE <- (band_8-band_5)/(band_8+band_5)
    names(NDRE) <- dates
    writeRaster(NDRE,glue('{output_dir}NDRE.tif'), overwrite = T)
  }
  
  NULL
  
})
