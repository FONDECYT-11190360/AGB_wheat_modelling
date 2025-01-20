library(tidyverse)
library(patchwork)
library(imputeTS)
library(glue)
library(terra)

na_lm <- function(y) {
  
  df <- data.frame(x = seq_along(y), y = y)
  
  # Ajustar modelo lineal ignorando los NA
  modelo <- lm(y ~ x + x*x + x*x*x, data = df, na.action = na.exclude)
  # modelo <- step(modelo, trace = -1)
  # Predecir los valores para las posiciones con NA
  df$y[is.na(df$y)] <- predict(modelo, newdata = df[is.na(df$y), ])
  
  df$y
  
} 

na_loess <- function(y, span = 0.75) {
  
  df <- data.frame(x = seq_along(y), y = y)
  
  modelo <- loess(y ~ x, data = df, span = span, na.action = na.exclude)
  
  df$y[is.na(df$y)] <- predict(modelo, newdata = df[is.na(df$y), ])
  
  df$y
  
}

na_approx <- function(y){
  
  y 
  
  first_value <- y[which(!is.na(y))[1]]
  y[1:which(!is.na(y))[1]] <- first_value
  
  # Replace trailing NA values with the last non-NA value
  last_value <- y[which(!is.na(y))[length(which(!is.na(y)))]]
  y[which(!is.na(y))[length(which(!is.na(y)))] : length(y)] <- last_value
  
  y <- zoo::na.approx(y)
  
  y
}

{

# # fill_data <- function(y, method = 'all', both = FALSE) {
#   
#   valid_methods <- c('na_approx', 'na_lm', 'na_loess', 'imputeTS', 'kalman', 'all')
#   if (!(method %in% valid_methods)) {
#     stop("Método inválido. Por favor, elige uno de: ", paste(valid_methods, collapse = ", "))
#   }
#   
#   # Crear tibble inicial
#   data <- tibble(id = seq_along(y), y = y)
#   
#   # Aplicar métodos de imputación
#   d2 <- data |> 
#     mutate(
#       class = is.na(y),
#       na_approx = na_approx(y),
#       na_lm = na_lm(y),
#       na_loess = na_loess(y),
#       imputeTS = imputeTS::na_interpolation(y, option = "spline"),
#       kalman = imputeTS::na_kalman(y)
#     ) |> 
#     pivot_longer(cols = -c(id, class)) |> 
#     mutate(
#       name = forcats::fct_inorder(name),
#       name = forcats::fct_relevel(name, "y")
#     ) |> 
#     arrange(name, id)
#   
#   # Filtrar y devolver según el método
#   if (method == 'all') {
#     return(d2)
#   } else {
#     if (both) {
#       d2_filtered <- d2 |> 
#         filter(name == method | name == 'y')
#       return(d2_filtered)
#     } else {
#       d2_filtered <- d2 |> 
#         filter(name == method) |> 
#         pull(value)
#       return(d2_filtered)
#     }
#   }
# }

}  # función antigua

fill_data <- function(y, method = 'all', both = FALSE) {
  
  valid_methods <- c('na_approx', 'na_lm', 'na_loess', 'imputeTS', 'kalman', 'all')
  if (!(method %in% valid_methods)) {
    stop("Método inválido. Por favor, elige uno de: ", paste(valid_methods, collapse = ", "))
  }
  
  if (sum(!is.na(y)) <= 3) {
    message("Menos de 4 valores válidos en 'y'. Devolviendo 'y' original.")
    return(y)
  }
  
  y[is.infinite(y)] <- NA
  data <- tibble(id = seq_along(y), y = y)
  
  if (method == 'na_approx') {
    data <- data |> mutate(value = na_approx(y))
  } else if (method == 'na_lm') {
    data <- data |> mutate(value = na_lm(y))
  } else if (method == 'na_loess') {
    data <- data |> mutate(value = na_loess(y))
  } else if (method == 'imputeTS') {
    data <- data |> mutate(value = imputeTS::na_interpolation(y, option = "spline"))
  } else if (method == 'kalman') {
    data <- data |> mutate(value = imputeTS::na_kalman(y))
  } else if (method == 'all') {
    data <- data |> 
      mutate(
        na_approx = na_approx(y),
        na_lm = na_lm(y),
        na_loess = na_loess(y),
        imputeTS = imputeTS::na_interpolation(y, option = "spline"),
        kalman = imputeTS::na_kalman(y)
      ) |> 
      pivot_longer(cols = -c(id, y)) |> 
      mutate(
        name = forcats::fct_inorder(name),
        name = forcats::fct_relevel(name, "y")
      ) |> 
      arrange(name, id)
    return(data)
  }
  
  if (both) {
    data <- data |> mutate(original = y) |> 
      select(id, original, value)
  } else {
    data <- data |> pull(value)
  }
  
  return(data)
}

# # fill_data_2 <- function(y, method = 'all', both = FALSE) {
#   
#   valid_methods <- c('na_approx', 'na_lm', 'na_loess', 'imputeTS', 'kalman', 'all')
#   if (!(method %in% valid_methods)) {
#     stop("Método inválido. Por favor, elige uno de: ", paste(valid_methods, collapse = ", "))
#   }
#   
#   y[is.infinite(y)] <- NA
#   
#   data <- tibble(id = seq_along(y), y = y)
#   
#   # safe_mutate <- function(.data, column_name, fn) {
#   #   tryCatch(
#   #     .data |> mutate(!!sym(column_name) := fn(y)),
#   #     error = function(e) {
#   #       warning(glue("Error en el método '{column_name}': {e$message}. Se asignarán valores NA."))
#   #       .data |> mutate(!!sym(column_name) := NA_real_)
#   #     }
#   #   )
#   # }
#   
#   safe_mutate <- function(.data, column_name, fn) {
#     .data |> mutate(!!sym(column_name) := fn(y))
#   }
#   
#   if (method == 'na_approx') {
#     data <- safe_mutate(data, "value", na_approx)
#   } else if (method == 'na_lm') {
#     data <- safe_mutate(data, "value", na_lm)
#   } else if (method == 'na_loess') {
#     data <- safe_mutate(data, "value", na_loess)
#   } else if (method == 'imputeTS') {
#     data <- safe_mutate(data, "value", \(y) imputeTS::na_interpolation(y, option = "spline"))
#   } else if (method == 'kalman') {
#     data <- safe_mutate(data, "value", \(y) imputeTS::na_kalman(y))
#   } else if (method == 'all') {
#     data <- data |> 
#       mutate(
#         na_approx = tryCatch(na_approx(y), error = function(e) NA_real_),
#         na_lm = tryCatch(na_lm(y), error = function(e) NA_real_),
#         na_loess = tryCatch(na_loess(y), error = function(e) NA_real_),
#         imputeTS = tryCatch(imputeTS::na_interpolation(y, option = "spline"), error = function(e) NA_real_),
#         kalman = tryCatch(imputeTS::na_kalman(y), error = function(e) NA_real_)
#       ) |> 
#       pivot_longer(cols = -c(id, y)) |> 
#       mutate(
#         name = forcats::fct_inorder(name),
#         name = forcats::fct_relevel(name, "y")
#       ) |> 
#       arrange(name, id)
#     return(data)
#   }
#   
#   if (both) {
#     data <- data |> mutate(original = y) |> 
#       select(id, original, value)
#   } else {
#     data <- data |> pull(value)
#   }
#   
#   return(data)
# }

#suavizar imagenes

dir.in <- 'data/processed/raster/indices/sentinel_2a/'
dir.out <- 'data/processed/raster/indices/sentinel_2a_filled/'
cod_id <- list.files(dir.in)

lapply(cod_id, \(x) {
  
  print(x)
  sitio <- str_extract(x, ".*(?=_)")
  vis <- list.files(glue('{dir.in}{x}'),full.names=T)
  
  lapply(vis, \(y) {
    
    print(basename(y))
    vi_r <- rast(y)
    if (sitio == 'hidango') {
      id <- which(names(vi_r) %in% c('2021-08-21','2022-09-20','2022-09-30','2022-10-05'))
      values(vi_r[[id]]) <- NA
    } 
    if (sitio == 'la_cancha') {
      id <- which(names(vi_r) %in% c('2022-09-20','2022-10-10'))
      values(vi_r[[id]]) <- NA
    }
    
    output_dir <- glue("{dir.out}{x}/")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    vi_r <- app(vi_r,fun = \(z) {fill_data(z,method='kalman')})
    
    fechas <- names(vi_r)
    fechas_continuas <- seq.Date(min(as.Date(fechas)), as.Date(max(fechas)), by = "1 day")
    fechas_faltantes <- fechas_continuas[!fechas_continuas %in% fechas]

    r_faltantes <- rast(nrows = nrow(vi_r), ncols = ncol(vi_r),
                        extent = ext(vi_r), crs = crs(vi_r),
                        nlyrs = length(fechas_faltantes))
    names(r_faltantes) <- fechas_faltantes
    values(r_faltantes) <- NA

    r_completo <- c(vi_r, r_faltantes)
    r_completo <- r_completo[[order(names(r_completo))]]
    
    r <- app(r_completo,fun = \(z) {fill_data(z,method='na_approx')})
    names(r) <- fechas_continuas
    
    writeRaster(r, glue('{output_dir}{basename(y)}'),overwrite=T)
    
  })
})

#graficar (beta)

dir.vi_fill <- 'data/processed/raster/indices/sentinel_2a_filled/'
cod_id <- list.files(dir.vi_fill)

lapply(cod_id, \(x) {
  
  sitio <- str_extract(x, ".*(?=_)")
  vi <- list.files(glue('{dir.vi_fill}{x}'),full.names=T)
  
  lapply(vi, \(y) {
    
    vi_r <- rast(y)
    
    cell <- sample(ncell(vi_r),10)
    
    data <- lapply(cell, \(x) {
  tibble(sample_cell = x,
         fecha = as.Date(names(vi_r)),
         vi = as.numeric(vi_r[x]), 
         vi_fill = as.numeric(vi_r_fill[x]))
}) |> 
      bind_rows()
    
    data |> 
      ggplot(aes(fecha)) +
      geom_point(aes(y = vi_fill, color = "vi_fill")) +
      geom_line(aes(y = vi_fill, color = "vi_fill")) +
      facet_wrap(~sample_cell, ncol = 5) +
      scale_color_manual(
        name = "", 
        values = c("vi_fill" = "#00BFC4", "vi" = "#F8766D")
      ) + 
      labs(title = paste0(gsub('.tif','',vi),'   - ',x)) +
      theme_bw()
    
    })
})












