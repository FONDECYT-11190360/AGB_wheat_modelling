library(tidyverse)
library(patchwork)
library(imputeTS)

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

fill_data <- function(y,method = 'all', both = F){
  
  valid_methods <- c('na_approx', 'na_lm', 'na_loess', 'imputeTS', 'kalman','all')
  if (!(method %in% valid_methods)) {
    stop("Método inválido. Por favor, elige uno de: ", paste(valid_methods, collapse = ", "))
  }
  
  data <- data_frame(id = seq_along(y), y = y) 
  
  d2 <- data |> 
    mutate(
      class = is.na(y),
      na_approx = na_approx(y),
      na_lm = na_lm(y),
      na_loess = na_loess(y),
      imputeTS = imputeTS::na_interpolation(y, option = "spline"),
      kalman = imputeTS::na_kalman(y)
    ) |> 
    pivot_longer(cols = -c(id, class)) |> 
    mutate(
      name = forcats::fct_inorder(name),
      name = forcats::fct_relevel(name, "y")
    ) |> 
    arrange(name, id)
  
  if (method == 'all') {return(d2)} else {
    if(both == T) {
      d2_filtered <- d2 |> 
        filter(name == method | name == 'y')
      return(d2_filtered)
    } else {
      d2_filtered <- d2 |> 
        filter(name == method) |> 
        pull(value)
      return(d2_filtered)
    }
  }
}

#suavizar indices
data_visps <- read_rds('rds/vi_ps.rds')
data_viss2 <- read_rds('rds/vi_s2.rds')
data_viss22 <- read_rds('rds/vi_s2_bkp.rds')

data_visps |> 
  group_by(sitio, temporada, muestra, vi) |> 
  complete(fecha = seq(min(fecha), max(fecha), by = "1 day")) |>
  mutate(valor = fill_data(valor,method = 'kalman')) |> 
  ungroup() |> 
  write_rds('rds/vi_ps_suavizado.rds')

data_viss2 |> 
  group_by(sitio, temporada, muestra, vi) |> 
  complete(fecha = seq(min(fecha), max(fecha), by = "1 day")) |>
  mutate(valor = fill_data(valor,method = 'kalman')) |> 
  ungroup() |> 
  write_rds('rds/vi_s2_suavizado.rds')

data_viss2 |> 
  group_by(sitio, temporada, muestra, vi) |> 
  complete(fecha = seq(min(fecha), max(fecha), by = "1 day")) |>
  mutate(valor = fill_data(valor,method = 'kalman')) |> 
  ungroup() |> 
  write_rds('rds/vi_s2_bkp_suavizado.rds')

data |> 
  filter(sitio == 'hidango') |> 
  ggplot(aes(fecha,valor,col = muestra)) +
  geom_line(aes(fecha,valor_fill), linewidth = .5) +
  geom_point() +
  facet_wrap(~vi)














