library(tidyverse)
library(patchwork)
library(imputeTS)
library(glue)
library(terra)
library(parallel)

na_approx <- function(y){

  first_value <- y[which(!is.na(y))[1]]
  
  y[1:which(!is.na(y))[1]] <- first_value

  last_value <- y[which(!is.na(y))[length(which(!is.na(y)))]]
  y[which(!is.na(y))[length(which(!is.na(y)))] : length(y)] <- last_value
  y <- zoo::na.approx(y)
  y
}
kalman_fill <- function(y) {
  
  y[is.infinite(y)] <- NA
  
  if (sum(!is.na(y)) <= 3) {
    message("Menos de 4 valores válidos en 'y'. Devolviendo 'y' original.")
    return(y)
  } else {
  
    y_filled <- tryCatch(
      {imputeTS::na_kalman(y)},
      error = function(e) {
        message("Error en na_kalman, utilizando na.approx en su lugar.")
        return(zoo::na.approx(y))
      }
    )
  return(y_filled)
  }
}
naApprox_fill <- function(y) {
  
  y[is.infinite(y)] <- NA
  
  if (sum(!is.na(y)) <= 3) {
    message("Menos de 4 valores válidos en 'y'. Devolviendo 'y' original.")
    return(y)
  } else {
  
  y_filled <- na_approx(y)
  
  return(y_filled)
  }
}

#sentinel-2

dir.in <- 'data/processed/raster/indicadores/sentinel_2/'
dir.out <- 'data/processed/raster/indicadores/sentinel_2_filled/'
cod_id <- list.files(dir.in)

lapply(cod_id, \(x) {
  
  output_dir <- glue("{dir.out}{x}/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = T)
  }
  
  print(x)
  tif <- list.files(glue('{dir.in}{x}'),full.names=T)
  fechas <- str_extract(tif, "\\d{4}-\\d{2}-\\d{2}")
  fechas_continuas <- seq.Date(min(as.Date(fechas)), as.Date(max(fechas)), by = "1 day")
  fechas_faltantes <- fechas_continuas[!fechas_continuas %in% fechas]
  
  r <- rast(tif)
  names_vi <- names(r) |> unique()
  
  r_filled <- list()
  
  for (vi in names_vi) {
    
    gc()
    print(vi)
    r_vi <- r[[which(names(r) == vi)]]
    r_vi <- clamp(r_vi,-5,50,values=F)
    names(r_vi) <- fechas
    
    if (x == 'hidango_2021-2022') {
      id <- which(names(r_vi) %in% c('2021-08-21','2022-01-13'))
      values(r_vi[[id]]) <- NA
    }
    if (x == 'hidango_2022-2023') {
      id <- which(names(r_vi) %in% c('2022-09-20','2022-09-30','2022-10-05'))
      values(r_vi[[id]]) <- NA
    }
    if (x == 'la_cancha_2022-2023') {
      id <- which(names(r_vi) %in% c('2022-07-02','2022-07-27','2022-08-11','2022-09-20','2022-10-10'))
      values(r_vi[[id]]) <- NA
    }
    if (x == 'villa_baviera_2020-2021') {
      id <- which(names(r_vi) %in% c('2020-09-10'))
      values(r_vi[[id]]) <- NA
    }
    
    {ncores <- detectCores()
      cl <- makeCluster(ncores-2)
      clusterEvalQ(cl, library('tidyverse'))
      clusterExport(cl, c('kalman_fill','naApprox_fill','na_approx'))
    }
    
    r_vi_filled <- app(r_vi, kalman_fill, cores = cl)
    
    r_faltantes <- rast(nrows = nrow(r_vi_filled), ncols = ncol(r_vi_filled),
                        extent = ext(r_vi_filled), crs = crs(r_vi_filled),
                        nlyrs = length(fechas_faltantes))
    names(r_faltantes) <- fechas_faltantes
    values(r_faltantes) <- NA
    
    r_completo <- c(r_vi_filled, r_faltantes)
    r_completo <- r_completo[[order(names(r_completo))]]
    
    r_filled[[vi]] <- app(r_completo, naApprox_fill, cores = cl)
    names(r_filled[[vi]]) <- fechas_continuas
    
  }
  
  for(fecha in as.character(fechas_continuas)) {
    r_fecha <- lapply(names_vi, \(vi) {r_filled[[vi]][[fecha]]}) |> 
      rast()
    names(r_fecha) <- names_vi
    writeRaster(r_fecha,glue('{output_dir}vi_s2_filled_{fecha}.tif'),
                overwrite=T)
  }
})


#planetscope

dir.in <- 'data/processed/raster/indicadores/planetscope/'
dir.out <- 'data/processed/raster/indicadores/planetscope_filled/'
cod_id <- list.files(dir.in)

lapply(cod_id, \(x) {
  
  output_dir <- glue("{dir.out}{x}/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  print(x)
  tif <- list.files(glue('{dir.in}{x}'),full.names=T)
  fechas <- str_extract(tif, "\\d{4}-\\d{2}-\\d{2}")
  fechas_continuas <- seq.Date(min(as.Date(fechas)), as.Date(max(fechas)), by = "1 day")
  fechas_faltantes <- fechas_continuas[!fechas_continuas %in% fechas]
  
  r <- rast(tif)
  names_vi <- names(r) |> unique()
  
  r_filled <- list()
  
  for (vi in names_vi) {
    
    gc()
    print(vi)
    r_vi <- r[[which(names(r) == vi)]]
    r_vi <- clamp(r_vi,-5,50,values=F)
    names(r_vi) <- fechas
    
    if (x == 'hidango_2021-2022') {
      r_vi[[which(names(r_vi) == '2021-09-18')]] <- r_vi[[which(names(r_vi) == '2021-09-18')]]*NA
    }
    
    {ncores <- detectCores()
      cl <- makeCluster(ncores-2)
      clusterEvalQ(cl, library('tidyverse'))
      clusterExport(cl, c('kalman_fill','naApprox_fill','na_approx'))
    }
    
    r_vi_filled <- app(r_vi, kalman_fill, cores = cl)
    
    r_faltantes <- rast(nrows = nrow(r_vi_filled), ncols = ncol(r_vi_filled),
                        extent = ext(r_vi_filled), crs = crs(r_vi_filled),
                        nlyrs = length(fechas_faltantes))
    names(r_faltantes) <- fechas_faltantes
    values(r_faltantes) <- NA
    
    r_completo <- c(r_vi_filled, r_faltantes)
    r_completo <- r_completo[[order(names(r_completo))]]
    
    r_filled[[vi]] <- app(r_completo, naApprox_fill, cores = cl)
    names(r_filled[[vi]]) <- fechas_continuas
    
  }

  for(fecha in as.character(fechas_continuas)) {
    r_fecha <- lapply(names_vi, \(vi) {r_filled[[vi]][[fecha]]}) |> 
      rast()
    names(r_fecha) <- names_vi
    writeRaster(r_fecha,glue('{output_dir}vi_planetscope_filled_{fecha}.tif'),
                overwrite=T)
  }
})

#sentinel-1 (rellenar)

dir.in <- 'data/processed/raster/indicadores/sentinel_1/'
dir.out <- 'data/processed/raster/indicadores/sentinel_1_filled/'
cod_id <- list.files(dir.in)

lapply(cod_id, \(x) {
  
  output_dir <- glue("{dir.out}{x}/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  print(x)
  tif <- list.files(glue('{dir.in}{x}'),full.names=T)
  fechas <- str_extract(tif, "\\d{4}-\\d{2}-\\d{2}")
  
  df <- tibble(fecha = as.Date(fechas)) |> 
    mutate(fecha_sig = lead(fecha,1),
           fecha_int = fecha + (fecha_sig-fecha)/2,
           fecha_data = fecha) |> 
    select(-fecha_sig) |> 
    pivot_longer(c(fecha,fecha_int),names_to = 'tipo',values_to = 'fecha') |> 
    select(fecha,fecha_data) |> 
    distinct(fecha,fecha_data) |> 
    na.omit()
  
  fechas_continuas <- seq.Date(min(as.Date(df$fecha)), as.Date(max(df$fecha)), by = "1 day")
  
  df <- tibble(fecha = fechas_continuas) |> 
    left_join(df) |> 
    fill(fecha_data, .direction = "up") |> 
    select(fecha_data,fecha_fill = fecha)
  
  for (i in seq_along(fechas)) {
    r <- rast(tif[i])
    fechas_fill <- df |> 
      filter(fecha_data == fechas[i]) |> 
      pull(fecha_fill)
    lapply(fechas_fill, \(fecha_nueva) writeRaster(r,glue('{output_dir}s1_{fecha_nueva}.tif')))
  }
})

#visualizar s2

dir <- 'data/processed/raster/indicadores/sentinel_2/'
dir_filled <- 'data/processed/raster/indicadores/sentinel_2_filled/'
cod_id <- list.files(dir)

lapply(cod_id,\(x) {
  
  sp <- vect('data/processed/sitios.gpkg',layer = glue('muestreo_{x}')) |> 
    project('EPSG:32719')
  
  tif <- list.files(glue('{dir}{x}'),full.names=T)
  tif_filled <- list.files(glue('{dir_filled}{x}'),full.names=T)
  fechas <- str_extract(tif, "\\d{4}-\\d{2}-\\d{2}")
  fechas_filled <- str_extract(tif_filled, "\\d{4}-\\d{2}-\\d{2}")
  
  r <- rast(tif)
  r_filled <- rast(tif_filled)
  names_vi <- unique(names(r))
  
  output_dir <- glue('output/figs/series/suavizado/sentinel_2/{x}/')
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  lapply(names_vi,\(vi) {
    
    r_vi <- r[[which(names(r) == vi)]]
    r_vi_filled <- r_filled[[which(names(r_filled) == vi)]]
    names(r_vi) <- fechas
    names(r_vi_filled) <- fechas_filled
    
    ext_vi <- extract(r_vi,sp) |> 
      pivot_longer(cols = -c(ID), names_to = 'fecha', values_to = 'vi')
    ext_vi_filled <- extract(r_vi_filled,sp) |> 
      pivot_longer(cols = -c(ID), names_to = 'fecha', values_to = 'vi_filled')
    
    left_join(ext_vi_filled,ext_vi) |> 
      suppressMessages() |> 
      rename(muestra = ID) |>   
      ggplot(aes(as.Date(fecha))) +
      # geom_point(aes(as.Date(fecha),vi_filled, color = 'filled'),shape = 21, col = 'black') + 
      geom_line(aes(as.Date(fecha),vi_filled, color = as.factor(muestra)), 
                alpha = .5) +
      geom_point(aes(as.Date(fecha),vi, color = as.factor(muestra))) + 
      geom_hline(yintercept = 0, linetype = 'dashed', alpha = .5) +
      labs(title = vi, x = NULL, y = NULL) +
      theme_bw() +
      theme(legend.position = 'none')
    
    ggsave(glue('{output_dir}{vi}.png'),height = 6, width = 10)
    
  })
})

#visualizar planetscope

dir <- 'data/processed/raster/indicadores/planetscope/'
dir_filled <- 'data/processed/raster/indicadores/planetscope_filled/'
cod_id <- list.files(dir)

lapply(cod_id,\(x) {
  
  print(x)
  
  sp <- vect('data/processed/sitios.gpkg',layer = glue('muestreo_{x}')) |> 
    project('EPSG:32719')
  
  tif <- list.files(glue('{dir}{x}'),full.names=T)
  tif_filled <- list.files(glue('{dir_filled}{x}'),full.names=T)
  fechas <- str_extract(tif, "\\d{4}-\\d{2}-\\d{2}")
  fechas_filled <- str_extract(tif_filled, "\\d{4}-\\d{2}-\\d{2}")
  
  r <- rast(tif)
  r_filled <- rast(tif_filled)
  names_vi <- unique(names(r))
  
  output_dir <- glue('output/figs/series/suavizado/planetscope/{x}/')
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  lapply(names_vi,\(vi) {
    
    print(vi)
    r_vi <- r[[which(names(r) == vi)]]
    r_vi_filled <- r_filled[[which(names(r_filled) == vi)]]
    names(r_vi) <- fechas
    names(r_vi_filled) <- fechas_filled
    
    ext_vi <- terra::extract(r_vi,sp) |> 
      pivot_longer(cols = -c(ID), names_to = 'fecha', values_to = 'vi')
    ext_vi_filled <- terra::extract(r_vi_filled,sp) |> 
      pivot_longer(cols = -c(ID), names_to = 'fecha', values_to = 'vi_filled')
    
    left_join(ext_vi_filled,ext_vi) |> 
      suppressMessages() |> 
      rename(muestra = ID) |>   
      ggplot(aes(as.Date(fecha))) +
      # geom_point(aes(as.Date(fecha),vi_filled, color = 'filled'),shape = 21, col = 'black') + 
      geom_line(aes(as.Date(fecha),vi_filled, color = as.factor(muestra)), 
                alpha = .5) +
      geom_point(aes(as.Date(fecha),vi, color = as.factor(muestra))) + 
      geom_hline(yintercept = 0, linetype = 'dashed', alpha = .5) +
      labs(title = vi, x = NULL, y = NULL) +
      theme_bw() +
      theme(legend.position = 'none')
    
    ggsave(glue('{output_dir}{vi}.png'),height = 6, width = 10)
    
  })
})

# visualizar s1

library(fs)

cod_id <- list.files('data/processed/raster/indicadores/sentinel_1/')

lapply(cod_id, \(x) {
  
  file_raw <- dir_ls(glue('data/processed/raster/indicadores/sentinel_1/{x}'))
  file_fill <- dir_ls(glue('data/processed/raster/indicadores/sentinel_1_filled/{x}'))
  
  r_raw <- rast(file_raw)
  r_fill <- rast(file_fill)
  
  bands <- c('VV','VH','VH_VV')
  
  sample_points <- vect('data/processed/sitios.gpkg', layer = glue('muestreo_{x}'))
  
  data <- lapply(bands, \(band) {
    band_raw <- r_raw |> 
      subset(names(r_raw) == band) |> 
      setNames(str_extract(file_raw,'\\d{4}-\\d{2}-\\d{2}'))
    
    band_fill <- r_fill |> 
      subset(names(r_fill) == band) |> 
      setNames(str_extract(file_fill,'\\d{4}-\\d{2}-\\d{2}'))
    
    data_band <- bind_rows(
      terra::extract(band_raw, sample_points) |> 
        pivot_longer(c(everything(),-ID), names_to = 'fecha',values_to = band) |> 
        mutate(s1 = 'raw',
               .before = ID),
      terra::extract(band_fill, sample_points) |> 
        pivot_longer(c(everything(),-ID), names_to = 'fecha',values_to = band) |> 
        mutate(s1 = 'filled',
               .before = ID)
    )
  }) |> reduce(full_join) |> 
    pivot_longer(cols = c(VV,VH,VH_VV), names_to = 'band', values_to = 'value') |> 
    mutate(fecha = as.Date(fecha))
  
  ggplot(data = filter(data,s1=='filled'), aes(fecha,value,color = 'filled')) +
    geom_point() +
    geom_point(data = filter(data,s1=='raw'), aes(fecha,value,color = 'raw')) +
    geom_line(data = filter(data,s1=='raw'), aes(fecha,value,color = 'raw')) +
    labs(x = NULL,color = NULL, title = x) +
    facet_wrap(ID~band,ncol=3, scales = 'free_y') +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'))
  
})


