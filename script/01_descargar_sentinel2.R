
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
names(pol) <- cod_id

download_s2 <- function(pol,inicio,fin,dir_out) {
  
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
  
  bb <- pol |> 
    st_transform(32719) |>
    st_bbox() |>
    as.numeric()
  
  rev(unlist(lapply(items$features, \(x) {as.Date(x[[8]]$datetime)})) |> as.Date() |> unique())
  
  date_inicio <- last(items$features)[[8]]$datetime |> as.Date() |> as.character()
  date_final <- first(items$features)[[8]]$datetime |> as.Date() |> as.character()
  
  v = cube_view(srs = "EPSG:32719",
                extent = list(t0 = date_inicio, 
                              t1 = date_final,
                              left = bb[1], right = bb[3],
                              top = bb[4], bottom = bb[2]),
                dx = 10, dy = 10, dt = "P5D")
  
  col <- stac_image_collection(items$features)
  
  raster_cube(col, v) |>
    write_tif(dir_out)
  
}

dir.out <- 'data/raw/sentinel_2a/'

download_s2(pols$`hidango_2021-2022`,inicio = '2021-05-01', fin = '2022-01-31',
            dir_out = glue('{dir.out}{cod_id[1]}'))
download_s2(pols$`hidango_2022-2023`,inicio = '2022-05-01', fin = '2023-01-05',
            dir_out = glue('{dir.out}{cod_id[2]}'))
download_s2(pols$`la_cancha_2022-2023`,inicio = '2022-05-01', fin = '2022-12-31',
            dir_out = glue('{dir.out}{cod_id[3]}'))
download_s2(pols$`villa_baviera_2020-2021`,inicio = '2020-09-01', fin = '2021-01-31',
            dir_out = glue('{dir.out}{cod_id[4]}'))

#SCL
r <- list.files(glue('{dir.out}{cod_id[2]}'),full.name=T) |>
  rast()

scl_r <- r[[grep('SCL',names(r))]]

lapply(1:nlyr(scl_r),\(x) {

  fecha <- str_extract(sources(scl_r[[x]]), "\\d{4}-\\d{2}-\\d{2}")
  writeRaster(scl_r[[x]],glue('scl_2/SCL_{fecha}.tif'))

  NULL
})
