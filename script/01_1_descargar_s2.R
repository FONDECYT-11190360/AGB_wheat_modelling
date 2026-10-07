# Script 01.1: vuelve a descargar Sentinel-2 L2A desde Planetary Computer (2026-10-07)
# Usa la función download_s() del script 01, que descarta las escenas duplicadas con línea base
# de procesamiento >= 04.00. No ejecuta el inicio de sesión de Earthdata del script 01, que no se
# necesita para Planetary Computer. Las carpetas de salida usan el cod_id sin el sufijo "_ext",
# que es el nombre que esperan los scripts 02 a 05.
library(tidyverse)
library(glue)
library(terra)
library(gdalcubes)
library(rstac)
library(sf)

expr_01 <- parse('script/01_descargar_imagenes.R')
es_download_s <- vapply(expr_01, \(e) {
  is.call(e) && identical(e[[1]], as.name('<-')) && identical(e[[2]], as.name('download_s'))
}, logical(1))
eval(expr_01[[which(es_download_s)]])

cod_id <- c('hidango_2021-2022', 'hidango_2022-2023',
            'la_cancha_2022-2023', 'villa_baviera_2020-2021')
pols <- lapply(cod_id, \(x) read_sf('data/processed/sitios.gpkg', layer = glue('a_{x}')))
names(pols) <- cod_id

dir_s2 <- 'data/raw/raster/sentinel_2/'
periodos <- list('hidango_2021-2022'       = c('2021-05-01', '2022-02-25'),
                 'hidango_2022-2023'       = c('2022-05-01', '2023-02-05'),
                 'la_cancha_2022-2023'     = c('2022-05-01', '2023-01-20'),
                 'villa_baviera_2020-2021' = c('2020-08-15', '2021-02-25'))

gdalcubes_options(parallel = 4)

for (x in cod_id) {
  message(x)
  download_s(pols[[x]], 'sentinel-2-l2a', inicio = periodos[[x]][1], fin = periodos[[x]][2],
             dir_out = glue('{dir_s2}{x}'))
}
