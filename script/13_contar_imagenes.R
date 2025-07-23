#numero de imagenes

library(terra)
library(glue)
library(tidyverse)

misiones <- c('sentinel_1', 'sentinel_2', 'planetscope')

dir <- 'data/processed/raster/indicadores'

sitios <- list.files('data/processed/raster/indicadores/sentinel_1')

lapply(misiones, \(mision) {
  lapply(sitios, \(sitio) length(list.files(glue('{dir}/{mision}/{sitio}')))) |> 
    setNames(sitios)
}) |> 
  setNames(misiones)






dir <- 'data/processed/raster/indices/sentinel_2a/'
sitios <- list.files(dir)

fechas <- lapply(sitios,\(x) {
  r <- rast(list.files(glue('{dir}{x}'),full.names=T)[1])
  tibble(sitio = x, fecha = names(r))
}) |> 
  bind_rows() |> 
  mutate(sitio = gsub('_2',' 2',sitio)) |> 
  separate(sitio, into = c("sitio", "temporada"), sep = " ")

data_bio <- read_rds('data/processed/rds/biomasa.rds') |> 
  group_by(sitio,temporada) |> 
  reframe(fecha_min = min(fecha,na.rm=T),
          fecha_max = max(fecha,na.rm=T))

conteo <- fechas |> 
  left_join(data_bio) |>
  filter(fecha >= fecha_min,
         fecha <= fecha_max) |> 
  group_by(sitio,temporada) |> 
  reframe(n = n())

sum(conteo$n)

#tiles de sitios

library(rstac)
library(tidyverse)
library(earthdatalogin)
library(sf)
library(glue)
library(gdalcubes)
edl_netrc(username = 'frzambra@gmail.com',password = 'Traplozx398#')
with_gdalcubes()

cod_id <- c('hidango_2021-2022','hidango_2022-2023',
            'la_cancha_2022-2023','villa_baviera_2020-2021')
pols <- lapply(cod_id,function(x) {read_sf('data/processed/sitios.gpkg',layer = glue('a_{x}'))})
names(pols) <- cod_id

extraer_tiles_s2 <- function(pol,inicio,fin) {
  bb <- st_bbox(pol) |>
    as.numeric()
  url <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  items <- stac(url) |>
    stac_search(collections = "sentinel-2-l2a",
                bbox = bb,
                datetime = paste(inicio,fin, sep = "/")) |>
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |>
    items_fetch()
  lapply(items$features, \(x) {
    x[[1]]
  }) |> unlist() |> 
    stringr::str_extract("T\\d{2}[A-Z]{3}") |> 
    unique()
}

data_bio <- read_rds('data/processed/rds/biomasa.rds') |> 
  group_by(sitio,temporada) |> 
  reframe(fecha_min = min(fecha,na.rm=T),
          fecha_max = max(fecha,na.rm=T))

tiles <- lapply(1:4,\(i) {
  tiles <- extraer_tiles_s2(pols[[cod_id[i]]],data_bio$fecha_min[i],data_bio$fecha_max[i])
  tibble(sitio = data_bio$sitio[i],temporada = data_bio$temporada[i], tiles = tiles)
}) |> 
  bind_rows()

