library(rstac)
library(terra)
library(sf)

link <- "https://planetarycomputer.microsoft.com/api/stac/v1/"

lyrs <- st_layers("data/data_processed/sitios.gpkg")

bb <- st_read("data/data_processed/sitios.gpkg",
              lyrs$name[1]) |> 
  st_bbox()

items <- stac(link) |> 
  # stac_search(
  # collections = "modis-13Q1-061",
  # datetime = "2000-01-01/2024-03-01",
  # limit = 999,
  # bbox = bb
  # ) |> 
  collections("modis-13Q1-061") |> 
  queryables() |> 
  get_request()

items <- stac(link) |> 
  stac_search(
  collections = "modis-13Q1-061",
  datetime = "2000-01-01/2012-04-06",
  limit = 500,
  bbox = bb
  ) |>
  # collections("modis-13Q1-061") |> 
  # queryables() |> 
  get_request() |> 
  items_filter(filter_fn = function(x) {x$properties$platform == "terra"})  
  
signed_stac_query <- rstac::items_sign(
  items,
  rstac::sign_planetary_computer()
)

rstac::assets_download(signed_stac_query, output_dir = '/mnt/data_procesada/data/rasters/Proyectos/fondect_11190360/',asset_names = "250m_16_days_NDVI",overwrite = TRUE)
