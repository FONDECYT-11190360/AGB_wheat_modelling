library(terra)
library(tidyverse)
library(fs)

names <- list.files('data/raw/sentinel_2a/')

dir_in <- 'data/raw/sentinel_2a/'
dir_out <- 'data/processed/raster/indices/sentinel_2a/'

lapply(names, \(x) {
  
  tif <- list.files(glue('{dir_in}{x}/'),full.names=T)
  dates <- str_extract(basename(tif), "\\d{4}-\\d{2}-\\d{2}")
  r <- rast(tif)
  
  scl <- r[[grep("SCL", names(r))]] |> 
    classify(rcl = matrix(c(1,3,8,9,10,NA,NA,NA,NA,NA),nrow=5,ncol=2), others = 1)
  
  band_2 <- r[[which(names(r) == 'B02')]]/10000*scl
  band_4 <- r[[which(names(r) == 'B04')]]/10000*scl
  band_5 <- r[[which(names(r) == 'B05')]]/10000*scl
  band_8 <- r[[which(names(r) == 'B08')]]/10000*scl
  band_11 <- r[[which(names(r) == 'B11')]]/10000*scl
  
  NDVI <- (band_8-band_4)/(band_8+band_4)
  SAVI <- (band_8-band_4)/(band_8+band_4+.5)*(1+.5)
  EVI <- 2.5*(band_8-band_4)/((band_8+6*band_4-7.5*band_2)+1)
  NDWI <- (band_8-band_11)/(band_8+band_11)
  NDRE <- (band_8-band_5)/(band_8+band_5)
  KNR <- exp(-(band_8-band_4)^2/(2*.15^2))
  KNDVI <- (1-KNR)/(1+KNR)
  
  names(NDVI) <- dates
  names(SAVI) <- dates
  names(EVI) <- dates
  names(NDWI) <- dates
  names(NDRE) <- dates
  names(KNDVI) <- dates
  
  output_dir <- glue("{dir_out}{x}/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  writeRaster(NDVI,glue('{output_dir}NDVI.tif'), overwrite = T)
  writeRaster(SAVI,glue('{output_dir}SAVI.tif'), overwrite = T)
  writeRaster(EVI,glue('{output_dir}EVI.tif'), overwrite = T)
  writeRaster(NDWI,glue('{output_dir}NDWI.tif'), overwrite = T)
  writeRaster(NDRE,glue('{output_dir}NDRE.tif'), overwrite = T)
  writeRaster(KNDVI,glue('{output_dir}KNDVI.tif'), overwrite = T)
  
  NULL
  
})



