library(fs)
library(sen2r)
library(glue)
library(purrr)

dir_s2 <- '/mnt/raster_raw/ESA/Sentinel2/SAFE/'
sitio <- 'Pirque'

dir_out <- '/mnt/raster_raw/ESA/Sentinel2/GTIFF/'
files_safe <- dir_ls(glue('{dir_s2}{sitio}'))

files_safe |> 
  map(function(file){
    s2_translate(file,glue('{dir_out}{sitio}'),format="GTiff")
  })

dir_tif <- '/mnt/raster_raw/ESA/Sentinel2/GTIFF/'
files_tif <- dir_ls(glue('{dir_tif}{sitio}'))

dir_out <- '/mnt/raster_raw/ESA/Sentinel2/VI/'
s2_calcindices(files_tif,indices = c('NDVI','SAVI','EVI','NDWI','NDRE'),outdir =glue('{dir_out}{sitio}') )

# calcular kndvi

sigma <- 0.15

kndvi <- function(nir,red,sigma){
  knr <- exp(-(nir-red)^2/(2*sigma^2))
  kndvi <- (1-knr) / (1+knr)
  kndvi
}

sitios <- c('hidango','la_cancha','villa_baviera')

dir_tif <- '/mnt/data_procesada/data/rasters/Proyectos/fondect_11190360/GTIFF/'
dir_out <- '/mnt/data_procesada/data/rasters/Proyectos/fondect_11190360/VI/'

lapply(sitios,\(sitio){
  files_tif <- dir_ls(glue('{dir_tif}{sitio}'),regexp = 'tif$')
  lapply(files_tif,\(file){
    im <- rast(file,lyrs =c(4,8))
    im <- im / 10000
    out <- kndvi(im[[2]],im[[1]],.15)
    new_name <- gsub('BOA.*','kNDVI',names(out))
    writeRaster(out,glue('{dir_out}{sitio}/kNDVI/{new_name}.tif'))
  })
})
  