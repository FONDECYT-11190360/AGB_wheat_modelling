library(terra)
library(tidyverse)
library(fs)
library(glue)

#sentinel-2

calc_index_s2 <- function(r,index = NULL, dates, mask_values = NULL, dir.out = NULL) {
  
  if (!is.null(mask_values)) {
    scl <- r[[grep("SCL", names(r))]] |> 
      classify(rcl = matrix(c(mask_values, rep(NA, length(mask_values))),
                            nrow = length(mask_values), ncol = 2), others = 1)
  } else {
    scl <- classify(r[[grep("SCL", names(r))]], rcl = matrix(c(NA, NA), ncol = 2, byrow = T), others = 1)
  }
  
  band_name <- grep('^B', unique(names(r)), value = T)
  B <- lapply(band_name, \(x) {
    band <- r[[which(names(r) == x)]] / 10000 * scl
    band[band > 1] <- NA
    band[band < 0.01] <- NA
    band
  })
  names(B) <- gsub('0', '', band_name)
  
  list2env(B, envir = environment())
  
  {
  
  vi <- list(
    B1 = B1,B2 = B2,B3 = B3,B4 = B4,B5 = B5,B6 = B6,B7 = B7,B8 = B8,B8A = B8A,B9 = B9,B11 = B11,B12 = B12,
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
    KNDVI = (1-exp(-(B8-B4)^2/(2*.15^2)))/(1+exp(-(B8-B4)^2/(2*.15^2))),
    
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
    NDMI_8A = (B8A-B11)/(B8A+B11),
    KNDVI_8A = (1-exp(-(B8A-B4)^2/(2*.15^2)))/(1+exp(-(B8A-B4)^2/(2*.15^2)))
  ) |> 
    lapply(\(x) {
      names(x) <- dates
      return(x)
    })
    
  }
  
  if (is.null(index)) {
    warning("Debe especificar en el argumento `index` los índices que desea calcular. Use `index = 'all'` para obtener todos los índices disponibles.")
    return(NULL)
  }
  
  if (identical(index, "all")) {
    index <- names(vi)
  }
  
  missing_indices <- setdiff(index, names(vi))
  if (length(missing_indices) > 0) {
    warning(sprintf("Los siguientes índices no se calcularon: %s", paste(missing_indices, collapse = ", ")))
  }
  vi <- vi[intersect(index, names(vi))]
  
  if (length(vi) == 0) {
    warning("No se calculó ningún índice válido, no se retornará nada.")
    return(NULL)
  }
  
  if (is.null(dir.out)) {
    return(vi)
  } else {
    if (!dir.exists(dir.out)) dir.create(dir.out, recursive = TRUE)
    
    dates_unique <- unique(dates)
    for (date in dates_unique) {
      index_stack <- rast(lapply(names(vi), function(idx) vi[[idx]][[date]]))
      names(index_stack) <- names(vi)
      writeRaster(index_stack, glue('{dir.out}vi_s2_{date}.tif'), overwrite = TRUE)
    }
    
    return(invisible(NULL))
  }
}

cod_id <- list.files('data/raw/raster/sentinel_2/')

dir_in <- 'data/raw/raster/sentinel_2/'
dir_out <- 'data/processed/raster/indices/sentinel_2/'

lapply(cod_id, \(x) {
  
  tif <- list.files(glue('{dir_in}{x}/'),full.names=T)
  tif <- setdiff(tif, grep('.aux',tif,value = T))
  dates <- str_extract(basename(tif), "\\d{4}-\\d{2}-\\d{2}")
  r <- rast(tif)
  
  calc_index_s2(r, index = 'all', dates, mask_values = c(0,1,2,3,8,9,10), dir.out = glue('{dir_out}{x}/'))
  
})

#planet

calc_index_planet <- function(r,udm,index = NULL, dates, dir.out = NULL) {
  
  mask <- udm[[which(names(udm) == 'clear')]] |> 
    classify(matrix(c(0,1,NA,1),ncol=2))
  
  names(r) <- names(r) |> 
    as_tibble() |> 
    mutate(name = case_when(value == 'blue' ~ 'B2',
                            value == 'green' ~ 'B3',
                            value == 'red' ~ 'B4',
                            value == 'rededge' ~ 'B5',
                            value == 'nir' ~ 'B8',
                            .default = 'otras')) |> 
    pull(name)
  
  if (length(which(names(r) == 'otras')) != 0) {r <- r[[-which(names(r) == 'otras')]]}
  
  B <- lapply(unique(names(r)), \(x) {
    band <- r[[which(names(r) == x)]] / 10000 * mask
    band[band > 1] <- NA
    band[band < 0.01] <- NA
    names(band) <- dates
    band
  })
  names(B) <- unique(names(r))
  
  list2env(B, envir = environment())
  
  if (length(unique(names(r))) == 5) {
    vi <- list(
      B2 = B2,
      B3 = B3,
      B4 = B4,
      B5 = B5,
      B8 = B8,
      NDVI = (B8-B4)/(B8+B4),
      SR = B8/B4,
      MSR = (B8/B4-1)/(B8/B4+1)^0.5,
      MCARI = ((B5-B4)-0.2*(B5-B3))*(B5/B4),
      TCARI = 3*((B5-B4)-0.2*(B5-B3)*(B5/B4)),
      SAVI = 1.5*(B8-B4)/(B8+B4+0.5),
      SAVI_green = 1.5*(B8-B3)/(B8+B3+0.5),
      OSAVI = 1.16*(B8-B4)/(B8+B4+0.16),
      OSAVI_green = 1.16*(B8-B3)/(B8+B3+0.16),
      OSAVI_rededge = 1.16*(B8-B5)/(B8+B5+0.16),
      TCARI_OSAVI = (3*((B5-B4)-0.2*(B5-B3)*(B5/B4)))/(1.16*(B8-B4)/(B8+B4+0.16)),
      MCARI_OSAVI = (((B5-B4)-0.2*(B5-B3))*(B5/B4))/(1.16*(B8-B4)/(B8+B4+0.16)),
      CI_green = (B8/B3)-1,
      CI_red = (B8/B4)-1,
      CI_rededge = (B8/B5)-1,
      OSAVI_CI_rededge = (1.16*(B8-B4)/(B8+B4+0.16))*((B8/B5)-1),
      CVI = (B8/B3)*(B4/B3),
      DVI = B8-B4,
      DVI_green = B8-B3,
      DVI_rededge = B8-B5,
      EVI = 2.5*(B8-B4)/((B8+6*B4-7.5*B2)+1),
      GNDVI = (B8-B3)/(B8+B3),
      REVI1 = B8-B5,
      WDVI = B8-0.5*B4,
      GRVI = (B3-B4)/(B3+B4),
      GRVI2 = B8/B3,
      GARI = (B8-(B3-1.7*(B2-B4)))/(B8+(B3-1.7*(B2-B4))),
      LCI = (B8-B5)/(B8-B4),
      NDRE1 = (B8-B5)/(B8+B5),
      NDRE_NDVI = ((B8-B5)/(B8+B5))/((B8-B4)/(B8+B4)),
      SIPI = (B8-B2)/(B8-B4),
      KNDVI = (1-exp(-(B8-B4)^2/(2*.15^2)))/(1+exp(-(B8-B4)^2/(2*.15^2)))
    )
  } else {
    vi <- list(
      B2 = B2,
      B3 = B3,
      B4 = B4,
      B8 = B8,
      NDVI = (B8-B4)/(B8+B4),
      SR = B8/B4,
      MSR = (B8/B4-1)/(B8/B4+1)^0.5,
      SAVI = 1.5*(B8-B4)/(B8+B4+0.5),
      SAVI_green = 1.5*(B8-B3)/(B8+B3+0.5),
      OSAVI = 1.16*(B8-B4)/(B8+B4+0.16),
      OSAVI_green = 1.16*(B8-B3)/(B8+B3+0.16),
      CI_green = (B8/B3)-1,
      CI_red = (B8/B4)-1,
      CVI = (B8/B3)*(B4/B3),
      DVI = B8-B4,
      DVI_green = B8-B3,
      EVI = 2.5*(B8-B4)/((B8+6*B4-7.5*B2)+1),
      GNDVI = (B8-B3)/(B8+B3),
      WDVI = B8-0.5*B4,
      GRVI = (B3-B4)/(B3+B4),
      GRVI2 = B8/B3,
      GARI = (B8-(B3-1.7*(B2-B4)))/(B8+(B3-1.7*(B2-B4))),
      SIPI = (B8-B2)/(B8-B4),
      KNDVI = (1-exp(-(B8-B4)^2/(2*.15^2)))/(1+exp(-(B8-B4)^2/(2*.15^2)))
    )
  }
  
  if (is.null(index)) {
    warning("Debe especificar en el argumento `index` los índices que desea calcular. Use `index = 'all'` para obtener todos los índices disponibles.")
    return(NULL)
  }
  
  if (identical(index, "all")) {
    index <- names(vi)
  }
  
  missing_indices <- setdiff(index, names(vi))
  if (length(missing_indices) > 0) {
    warning(sprintf("Los siguientes índices no se calcularon: %s", paste(missing_indices, collapse = ", ")))
  }
  vi <- vi[intersect(index, names(vi))]
  
  if (length(vi) == 0) {
    warning("No se calculó ningún índice válido, no se retornará nada.")
    return(NULL)
  }
  
  if (is.null(dir.out)) {
    return(vi)
  } else {
    if (!dir.exists(dir.out)) dir.create(dir.out, recursive = TRUE)
    
    dates_unique <- unique(dates)
    for (date in dates_unique) {
      print(date)
      index_stack <- rast(lapply(names(vi), function(idx) vi[[idx]][[date]]))
      names(index_stack) <- names(vi)
      writeRaster(index_stack, glue('{dir.out}vi_planetscope_{date}.tif'), overwrite = TRUE)
    }
    
    return(invisible(NULL))
  }
}

dir_in <- 'data/raw/raster/planetscope/'
dir_out <- 'data/processed/raster/indices/planetscope/'

cod_id <- list.files(dir_in)

lapply(cod_id, \(x) {
  
  print(x)
  
  tif <- list.files(glue('{dir_in}{x}/'),full.names=T)
  tif <- setdiff(tif, grep('.aux',tif,value = T))
  tif_b <- grep('Analytic',tif,value = T)
  tif_udm <- grep('udm',tif,value = T)
  dates <- str_extract(tif_b, "\\d{8}") |> as.Date(format = "%Y%m%d") |> as.character()
  
  pol <- vect('data/processed/sitios.gpkg', layer = glue('a_{x}')) |> 
    project('EPSG:32719')
  
  r <- lapply(tif_b, \(x) {
    rast(x) |> 
      crop(pol)
  }) |> rast()
  udm <- lapply(tif_udm, \(x) {
    rast(x) |> 
      crop(pol)
  }) |> rast()
  
  calc_index_planet(r, udm, index = 'all', dates, dir.out = glue('{dir_out}{x}/'))
  
})

#sentinel-1

dir_in <- 'data/raw/raster/sentinel_1/'
dir_out <- 'data/processed/raster/indices/sentinel_1/'

cod_id <- gsub('.tif','',list.files(dir_in))

lapply(cod_id, \(x) {
  
  output_dir <- glue('{dir_out}{x}/')
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  r <- rast(glue('{dir_in}{x}.tif'))  
  pattern <- ".*_(VV|VH)_(\\d{8})$"
  
  bands <- sub(pattern, "\\1", names(r))
  dates <- sub(pattern, "\\2", names(r)) |> 
    as.Date(format = "%Y%m%d") |> 
    as.character() |> 
    unique()
  
  VV <- clamp(r[[which(bands == 'VV')]], lower = -40, upper = 0)
  VV <- 10^(VV / 10)
  names(VV) <- dates
  VH <- clamp(r[[which(bands == 'VH')]], lower = -40, upper = 0)
  VH <- 10^(VH / 10)
  names(VH) <- dates
  VH_VV <- VH/VV
  
  b_list <- list(VV,VH,VH_VV)
  names(b_list) <- c('VV','VH','VH_VV')
  
  for (date in dates) {
    r_date <- lapply(names(b_list), \(b) {b_list[[b]][[date]]}) |> 
      rast()
    names(r_date) <- names(b_list)
    writeRaster(r_date,glue('{output_dir}index_s1_{date}.tif'),
                overwrite=T)
  }
})
