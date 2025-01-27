library(tidyverse)
library(glue)

#sentinel

library(gdalcubes)
library(earthdatalogin)
library(rstac)
library(sf)

edl_netrc(username = 'frzambra@gmail.com',password = 'Traplozx398#')
with_gdalcubes()

cod_id <- c('hidango_2021-2022','hidango_2022-2023',
         'la_cancha_2022-2023','villa_baviera_2020-2021')
pols <- lapply(cod_id,\(x) {read_sf('data/processed/sitios.gpkg',layer = glue('a_{x}'))})
names(pols) <- cod_id

download_s <- function(pol,collection,inicio,fin,dir_out) {
  
  bb <- st_bbox(pol) |>
    as.numeric()
  
  url <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  
  items <- stac(url) |>
    stac_search(collections = collection,
                bbox = bb,
                datetime = paste(inicio,fin, sep = "/")) |>
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |>
    items_fetch()
  
  bb <- pol |> 
    st_transform(4326) |>  #cambiar
    st_bbox() |>
    as.numeric()
  
  date_inicio <- last(items$features)[[8]]$datetime |> as.Date() |> as.character()
  date_final <- first(items$features)[[8]]$datetime |> as.Date() |> as.character()
  
  v = cube_view(srs = "EPSG:4326",
                extent = list(t0 = date_inicio, 
                              t1 = date_final,
                              left = bb[1], right = bb[3],
                              top = bb[4], bottom = bb[2]),
                dx = 0.0001, dy = 0.0001, dt = "P5D")  # cambiar dx y dy
  
  col <- stac_image_collection(items$features,
                               srs = 'EPSG:4326')
  
  # if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
  
  raster_cube(col, v) |>
    write_tif(dir_out)
  
}

dir_s1 <- 'data/raw/raster/sentinel_1/'
dir_s2 <- 'data/raw/raster/sentinel_2/'

download_s(pols$`hidango_2021-2022`, 'sentinel-1-grd', inicio = '2021-05-01', fin = '2022-01-31',
           dir_out = glue('{dir_s1}{cod_id[1]}'))
download_s(pols$`la_cancha_2022-2023`, 'sentinel-1-grd',inicio = '2022-05-01', fin = '2022-12-31',
           dir_out = glue('{dir_s1}{cod_id[3]}'))

download_s(pols$`hidango_2021-2022`, 'sentinel-2-l2a', inicio = '2021-05-01', fin = '2022-02-25',
            dir_out = glue('{dir_s2}{cod_id[1]}_ext'))
download_s(pols$`hidango_2022-2023`, 'sentinel-2-l2a',inicio = '2022-05-01', fin = '2023-02-05',
            dir_out = glue('{dir_s2}{cod_id[2]}_ext'))
download_s(pols$`la_cancha_2022-2023`, 'sentinel-2-l2a',inicio = '2022-05-01', fin = '2023-01-20',
            dir_out = glue('{dir_s2}{cod_id[3]}_ext'))
download_s(pols$`villa_baviera_2020-2021`, 'sentinel-2-l2a',inicio = '2020-08-15', fin = '2021-02-25',
            dir_out = glue('{dir_s2}{cod_id[4]}_ext'))
