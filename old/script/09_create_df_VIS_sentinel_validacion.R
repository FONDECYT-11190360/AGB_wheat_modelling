library(sf)
library(fs)
library(terra)
library(tidyverse)
library(glue)

read_csv2('data/data_raw/ptos_validacion_hidango_HS1_HS2.csv') |> 
  select(site:biomass) |> 
  write_rds('data/data_processed/data_biomasa_validacion.rds')

data <- read_csv2('data/data_raw/ptos_validacion_hidango_HS1_HS2.csv') |> 
  separate(sample,4,into = c('sample','muestra'))

data_sf <- data |>
  st_as_sf(coords = c('longitude','latitude'),crs=4326) |> 
  group_split(season) 
  
dir <- '/mnt/data_procesada/data/rasters/Proyectos/fondect_11190360/VI/'

sit <- c('hidango','hidango')
code <- c('HIS1','HIS2')

range_date <- list(c("2021-05-20","2022-01-04"),c("2022-05-20","2023-01-04"))

dir <- '/mnt/data_procesada/data/rasters/Proyectos/fondect_11190360/VI/'

data <- map_df(1:2,function(i){
  files <- dir_ls(glue('{dir}{sit[i]}'),recurse = TRUE,type = 'file')
  dates <- ymd(str_extract(basename(files),'[0-9]{8}'))
  
  files <- files[dates >= range_date[[i]][1] & dates <= range_date[[i]][2]]
  
  ptos <- data_sf[[i]]
  ptos <- st_transform(ptos,crs(rast(files[1])))
  
  map_df(files,function(file){
    iv <- rast(file)
    names(iv) <- 'layer'
    df <- terra::extract(iv,ptos) 
    df |> 
      mutate(sitio = sit[i],
             sample = paste0(code[i],ptos$muestra),
             date=ymd(str_extract(basename(file),'[0-9]{8}')),
             index = str_split_i(str_split_i(basename(file),'_',5),'\\.',1)) |> 
      rename(value = layer) |> 
      relocate(value,.after = index)
  },.progress = list(
    type = "iterator", 
    format = "Calculating {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
})
data |> 
  select(-ID) |> 
  write_rds('data/data_processed/data_vis_sentinel2_validacion.rds')
