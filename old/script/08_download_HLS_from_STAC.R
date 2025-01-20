library(rstac)
library(gdalcubes)

edl_netrc()
with_gdalcubes()
s <- stac('https://cmr.earthdata.nasa.gov/stac/LPCLOUD')

it <- s |> 
  stac_search("HLSL30.v2.0",
              limit = 5) |> 
  get_request()

rstac::assets_download(it, output_dir = '/mnt/data_procesada/',asset_names = "B07",overwrite = TRUE)

bbox <- c(xmin=-123, ymin=37.25, xmax=-122.0, ymax=38.25) 
start <- "2021-12-01"
end <- "2022-05-31"

# Find all assets from the desired catalog:
items <- stac("https://cmr.earthdata.nasa.gov/stac/LPCLOUD") |> 
  stac_search(collections = "HLSL30.v2.0",
              bbox = bbox,
              datetime = paste(start,end, sep = "/")) |>
  post_request() |>
  items_fetch() 

rstac::assets_download(items, output_dir = '/mnt/data_procesada/',asset_names = "B07",overwrite = TRUE)
