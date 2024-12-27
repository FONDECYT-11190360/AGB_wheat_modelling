library(terra)
library(tidyverse)
library(fs)
library(glue)

calc_index_s2 <- function(r,index,scl_mask_values = c(0,1,2,3,8,9,10)) {
  
  scl <- r[[grep("SCL", names(r))]] |> 
    classify(rcl = matrix(c(scl_mask_values,rep(NA,length(scl_mask_values))),
                          nrow=length(scl_mask_values),ncol=2), others = 1)
  
  band_name <- grep('^B',unique(names(r)),value=T)
  
  B <- lapply(band_name,\(x) {
    r[[which(names(r) == x)]]/10000*scl
  })
  names(B) <- gsub('B','',gsub('0','',band_name))
  
  B1 = B[['1']]
  B2 = B[['2']]
  B3 = B[['3']]
  B4 = B[['4']]
  B5 = B[['5']]
  B6 = B[['6']]
  B7 = B[['7']]
  B8 = B[['8']]
  B8A = B[['8A']]
  B9 = B[['9']]
  B11 = B[['11']]
  B12 = B[['12']]
  
  list(
    B1,B2,B3,B4,B5,B6,B7,B8,B8A,B9,B11,B12,
    NDVI = (B8-B4)/(B8+B4),
    NDVI2 = (B7-B4)/(B7+B4),
    NDVI705 = (B6-B5)/(B6+B5),
    SR = B8/B4,
    MSR = (B8/B4-1)/(B8/B4+1)^0.5,
    MSR705 = (B6/B5-1)/(B6/B5+1)^0.5,
    MCARI = ((B5-B4)-0.2*(B5-B3))*(B5/B4),
    TCARI = 3*((B5-B4)-0.2*(B5-B3)*(B5/B4)),
    SAVI = 1.5*(B8-B4)/(B8+B4+0.5),
    SAVI2 = 1.5*(B7-B4)/(B7+B4+0.5),
    SAVI_green = 1.5*(B8-B3)/(B8+B3+0.5),
    OSAVI = 1.16*(B8-B4)/(B8+B4+0.16),
    OSAVI2 = 1.16*(B7-B4)/(B7+B4+0.16),
    OSAVI_green = 1.16*(B8-B3)/(B8+B3+0.16),
    OSAVI_rededge = 1.16*(B8-B5)/(B8+B5+0.16),
    TCARI_OSAVI = (3*((B5-B4)-0.2*(B5-B3)*(B5/B4)))/(1.16*(B8-B4)/(B8+B4+0.16)),
    MCARI_OSAVI = (((B5-B4)-0.2*(B5-B3))*(B5/B4))/(1.16*(B8-B4)/(B8+B4+0.16)),
    TCARI_OSAVI2 = (3*((B5-B4)-0.2*(B5-B3)*(B5/B4)))/(1.16*(B7-B4)/(B7+B4+0.16)),
    MCARI_OSAVI2 = (((B5-B4)-0.2*(B5-B3))*(B5/B4))/(1.16*(B7-B4)/(B7+B4+0.16)),
    SWIR11_MCARI = ((B5-B11)-0.2*(B5-B3))*(B5/B11),
    SWIR11_TCARI = 3*((B5-B11)-0.2*(B5-B3)*(B5/B11)),
    SWIR11_OSAVI = 1.16*(B8-B11)/(B8+B11 + 0.16),
    SWIR12_MCARI = ((B5-B12)-0.2*(B5-B3))*(B5/B12),
    SWIR12_TCARI = 3*((B5-B12)-0.2*(B5-B3)*(B5/B12)),
    SWIR12_OSAVI = 1.16*(B8-B12)/(B8+B12 + 0.16),
    CI_green = (B8/B3)-1,
    CI_red = (B8/B4)-1,
    CI_rededge = (B8/B5)-1,
    OSAVI_CI_rededge = (1.16*(B8-B4)/(B8+B4+0.16))*((B8/B5)-1),
    CVI = (B8/B3)*(B4/B3),
    DVI = B8-B4,
    DVI_green = B8-B3,
    DVI_rededge = B8-B5,
    EVI = 2.5*(B8-B4)/((B8+6*B4-7.5*B2)+1),
    EVI2 = 2.5*(B7-B4)/(1+B7+2.4*B4),
    GNDVI = (B8-B3)/(B8+B3),
    REVI1 = B8-B5,
    REVI2 = B8-B6,
    WDVI = B8-0.5*B4,
    GRVI = (B3-B4)/(B3+B4),
    GRVI2 = B8/B3,
    GARI = (B8-(B3-1.7*(B2-B4)))/(B8+(B3-1.7*(B2-B4))),
    LCI = (B8-B5)/(B8-B4),
    MTCI = (B6-B5)/(B5-B4),
    NDRE1 = (B8-B5)/(B8+B5),
    NDRE2 = (B8-B6)/(B8+B6),
    NDRE3 = (B8-B7)/(B8+B7),
    NDREI = (B7-B3)/(B7+B3),
    NDRE_NDVI = ((B8-B5)/(B8+B5))/((B8-B4)/(B8+B4)),
    SIPI = (B8-B2)/(B8-B4),
    WI1 = B8/B9,
    NDMI = (B8-B11)/(B8+B11),
    
    NDVI_8A = (B8A-B4)/(B8A+B4),
    SR_8A = B8A/B4,
    MSR_8A = (B8A/B4-1)/(B8A/B4+1)^0.5,
    SAVI_8A = 1.5*(B8A-B4)/(B8A+B4 + 0.5),
    SAVI_green_8A = 1.5*(B8A-B3)/(B8A+B3 + 0.5),
    OSAVI_8A = 1.16*(B8A-B4)/(B8A+B4+0.16),
    OSAVI_green_8A = 1.16*(B8A-B3)/(B8A+B3+0.16),
    OSAVI_rededge_8A = 1.16*(B8A-B5)/(B8A+B5+0.16),
    TCARI_OSAVI_8A = (3*((B5-B4)-0.2*(B5-B3)*(B5/B4)))/(1.16*(B8A-B4)/(B8A+B4+0.16)),
    MCARI_OSAVI_8A = (((B5-B4)-0.2*(B5-B3))*(B5/B4))/(1.16*(B8A-B4)/(B8A+B4+0.16)),
    SWIR11_OSAVI_8A = 1.16*(B8A-B11)/(B8A+B11 + 0.16),
    SWIR12_OSAVI_8A = 1.16*(B8A-B12)/(B8A+B12 + 0.16),
    CI_green_8A = (B8A/B3)-1,
    CI_red_8A = (B8A/B4)-1,
    CI_rededge_8A = (B8A/B5)-1,
    OSAVI_CI_rededge_8A = (1.16*(B8A-B4)/(B8A+B4+0.16))*((B8A/B5)-1),
    CVI_8A = (B8A/B3)*(B4/B3),
    DVI_8A = B8A-B4,
    DVI_green_8A = B8A-B3,
    DVI_rededge_8A = B8A-B5,
    EVI_8A = 2.5*(B8A-B4)/((B8A+6*B4-7.5*B2)+1),
    GNDVI_8A = (B8A-B3)/(B8A+B3),
    REVI1_8A = B8A-B5,
    REVI2_8A = B8A-B6,
    WDVI_8A = B8A-0.5*B4,
    GRVI2_8A = B8A/B3,
    GARI_8A = (B8A-(B3-1.7*(B2-B4)))/(B8A+(B3-1.7*(B2-B4))),
    LCI_8A = (B8A-B5)/(B8A-B4),
    NDRE1_8A = (B8A-B5)/(B8A+B5),
    NDRE2_8A = (B8A-B6)/(B8A+B6),
    NDRE3_8A = (B8A-B7)/(B8A+B7),
    NDRE_NDVI_8A = ((B8A-B5)/(B8A+B5))/((B8A-B4)/(B8A+B4)),
    SIPI_8A = (B8A-B2)/(B8A-B4),
    WI1_8A = B8A/B9,
    NDMI_8A = (B8A-B11)/(B8A+B11)
  ) |> lapply(\(x) {
    list(
      min = min(global(x,min,na.rm=T)[,1],na.rm=T),
    max = max(global(x,max,na.rm=T)[,1],na.rm=T),
    mean = mean(global(x,mean,na.rm=T)[,1],na.rm=T)
    )
  })
  
}




cod_id <- list.files('data/raw/raster/sentinel_2a/')

dir_in <- 'data/raw/raster/sentinel_2a/'
dir_out <- 'data/processed/raster/indices/sentinel_2a/'

lapply(cod_id, \(x) {
  
  tif <- list.files(glue('{dir_in}{x}/'),full.names=T)
  tif <- tif[-grep('aux',tif)]
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
  SAVI <- (band_8-band_4)/(band_8+band_4+.5)*(1.5)
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



