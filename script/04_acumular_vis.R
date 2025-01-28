library(tidyverse)
library(patchwork)
library(imputeTS)
library(glue)
library(terra)

#sentinel-2

dir <- 'data/processed/raster/indices/sentinel_2_filled'
cod_id <- list.files(dir)

sos <- read_rds('data/processed/rds/fechas_fenologia.rds') |> 
  filter(fenologia == 'SOWING') |> 
  mutate(cod_id = paste0(sitio,'_',temporada))

lapply(cod_id, \(x) {
  
  print(x)
  
  tif_f <- list.files(glue('{dir}/{x}'),full.names=T)
  fechas_raw <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", tif_f)
  fecha_sos <- sos |> 
    filter(cod_id == x) |> pull(fecha)
  
  tif_f <- tif_f[which(fechas_raw >= fecha_sos)]
  fechas <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", tif_f)
  
  r_stack <- rast(tif_f)
  names_vi <- unique(names(r_stack))
  
  vi_stack = list()
  
  for (vi in names_vi) {
    print(vi)
    gc()
    r_vi <- r_stack[[which(names(r_stack) == vi)]]
    vi_stack[[vi]] <- cumsum(r_vi)
  }
  # 
  # vi_stack <- lapply(names_vi, \(vi) {
  #   r_vi <- r_stack[[which(names(r_stack) == vi)]]
  #   r_vi_cumsum <- cumsum(r_vi)
  # })
  
  dir_out <- glue('data/processed/raster/indices/sentinel_2_filled_cumsum/{x}')
  if (!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  
  for (date in seq_along(fechas)) {
    r <- lapply(vi_stack, \(vi) {vi[[date]]}) |> rast()
    names(r) <- paste0(names(r),'_cumsum')
    writeRaster(r,glue('{dir_out}/vi_s2_cumsum_{fechas[date]}.tif'), 
                overwrite = T)
  }
})

#planetscope

dir <- 'data/processed/raster/indices/planetscope_filled'
cod_id <- list.files(dir)

sos <- read_rds('data/processed/rds/fechas_fenologia.rds') |> 
  filter(fenologia == 'SOWING') |> 
  mutate(cod_id = paste0(sitio,'_',temporada))

lapply(cod_id, \(x) {
  
  tif_f <- list.files(glue('{dir}/{x}'),full.names=T)
  fechas_raw <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", tif_f)
  fecha_sos <- sos |> 
    filter(cod_id == x) |> pull(fecha)
  
  tif_f <- tif_f[which(fechas_raw >= fecha_sos)]
  fechas <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", tif_f)
  
  r_stack <- rast(tif_f)
  names_vi <- unique(names(r_stack))
  
  vi_stack = list()
  
  for (vi in names_vi) {
    print(vi)
    gc()
    r_vi <- r_stack[[which(names(r_stack) == vi)]]
    vi_stack[[vi]] <- cumsum(r_vi)
  }
  
  dir_out <- glue('data/processed/raster/indices/planetscope_filled_cumsum/{x}')
  if (!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  
  for (date in seq_along(fechas)) {
    r <- lapply(vi_stack, \(vi) {vi[[date]]}) |> rast()
    names(r) <- paste0(names(r),'_cumsum')
    writeRaster(r,glue('{dir_out}/vi_ps_cumsum_{fechas[date]}.tif'), 
                overwrite = T)
  }
})


