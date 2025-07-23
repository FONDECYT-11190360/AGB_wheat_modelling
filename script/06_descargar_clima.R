library(tidyverse)
library(terra)
library(stringr)
library(agrometR)
library(glue)

estaciones <- vect(estaciones_agromet, geom = c("longitud", "latitud"), crs = "EPSG:4326")

cod_id <- list.files('data/raw/raster/sentinel_2')
pols <- lapply(cod_id,function(x) {
  vector <- vect('data/processed/sitios.gpkg',layer = glue('a_{x}'))
})
names(pols) <- cod_id

pols[[2]] <- pols[[2]][1]

ema_id <- lapply(cod_id, \(x) {
  
  pol <- pols[[x]]
  estaciones_proj <- project(estaciones, "EPSG:32719")
  pol_proj <- project(pol, "EPSG:32719")
  distancias <- distance(estaciones_proj, pol_proj)/1000
  indice_mas_cercano <- which.min(distancias)
  estacion_mas_cercana <- estaciones[indice_mas_cercano, ]$nombre_ema
  
  cod_ema <- estaciones_agromet[which(estaciones_agromet$nombre_ema == estacion_mas_cercana),]$ema
  nom_ema <- estaciones_agromet[which(estaciones_agromet$nombre_ema == estacion_mas_cercana),]$nombre_ema
  inst <- estaciones_agromet[which(estaciones_agromet$nombre_ema == estacion_mas_cercana),]$institucion
  lat <- round(estaciones_agromet[which(estaciones_agromet$nombre_ema == estacion_mas_cercana),]$latitud,2)
  lon <- round(estaciones_agromet[which(estaciones_agromet$nombre_ema == estacion_mas_cercana),]$longitud,2)
  distance <- distancias[indice_mas_cercano]
  
  # return(c(cod_ema = cod_ema,nom_ema = nom_ema,institucion = inst,latotud = lat,longitud = lon,distance = distance))
  return(cod_ema)
  
}) |> unlist()
names(ema_id) <- cod_id

lapply(cod_id, \(x) {
  
  poligono <- pols[[x]]
  punto <- filter(estaciones, ema == ema_id[x])
  
  ext_punto <- ext(punto)
  ext_poligono <- ext(poligono)
  
  ext_actual <- ext(c(xmin = min(ext_punto[1], ext_poligono[1]),
                      xmax = max(ext_punto[2], ext_poligono[2]),
                      ymin = min(ext_punto[3], ext_poligono[3]),
                      ymax = max(ext_punto[4], ext_poligono[4])))
  
  estaciones_dentro <- crop(estaciones, ext_actual)
  
  ggplot() +
    geom_spatvector(data = poligono, fill = "lightgray", color = "black") +
    geom_spatvector(data = punto, color = "red", size = 3) +
    geom_spatvector(data = estaciones_dentro, color = "blue", size = 1) +
    coord_sf() +
    labs(title = x) +
    theme_minimal()
  
}) # graficar
  
data_h1 <- get_agro_data(stations_id = ema_id['hidango_2021-2022'],
                      date_start = '2021-05-01 01:00:00', date_end = '2022-01-31 24:00:00') |> 
  mutate(sitio = 'hidango', temporada = '2021-2022', .before = station_id)
data_h2 <- get_agro_data(stations_id = ema_id['hidango_2022-2023'],
                      date_start = '2022-05-01 01:00:00', date_end = '2023-01-05 24:00:00') |> 
  mutate(sitio = 'hidango', temporada = '2022-2023', .before = station_id)
data_lc <- get_agro_data(stations_id = ema_id['la_cancha_2022-2023'],
                      date_start = '2022-05-01 01:00:00', date_end = '2022-12-31 24:00:00') |> 
  mutate(sitio = 'la_cancha', temporada = '2022-2023', .before = station_id)
data_vb <- get_agro_data(stations_id = ema_id['villa_baviera_2020-2021'],
                      date_start = '2020-09-01 01:00:00', date_end = '2021-01-31 24:00:00') |> 
  mutate(sitio = 'villa_baviera', temporada = '2020-2021', .before = station_id)

data_clima <- bind_rows(data_h1,data_h2,data_lc,data_vb) |> 
  group_by(sitio,temporada,fecha = as.Date(fecha_hora)) |> 
  reframe(t_mean_dia = (min(temp_minima.na.rm=T)+max(temp_maxima, na.rm=T))/2,
          pp_dia = sum(precipitacion_horaria, na.rm=T)) |> 
  arrange(temporada,sitio,fecha)

write_rds(data_clima,'data/processed_old/data_clima_new.rds')
