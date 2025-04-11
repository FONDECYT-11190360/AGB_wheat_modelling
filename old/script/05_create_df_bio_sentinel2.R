library(terra)
library(dplyr)
library(fs)
library(glue)
library(purrr)
library(sf)
library(stringr)
library(lubridate)

sitio <- c('hidango_2021-2022','hidango_2022-2023','la_cancha','villa_baviera')
sit <- c('hidango','hidango','la_cancha','villa_baviera')
code <- c('HIS1','HIS2','LCS1','VBS1')

range_date <- list(c("2021-05-23","2022-01-04"),c("2022-05-28","2023-01-04"),
  c("2022-05-20","2022-12-31"),c("2020-09-01","2021-01-20"))

dir <- '/mnt/data_raw/ESA/Sentinel2/BiophysicalOp/'

data <- map_df(1:4,function(i){
  files <- dir_ls(glue('{dir}{sit[i]}'),recurse = TRUE,type = 'file') 
  files <- files[str_detect(files,'(fapar|lai.|lai_cw|lai_cab|fcover).*img$') & !str_detect(files,'flags')]
  dates <- ymd(str_extract(files,'[0-9]{8}'))
  
  files <- files[dates > range_date[[i]][1] & dates < range_date[[i]][2]]
  
  ptos <- read_sf('data/data_processed/muestreo_trigo.gpkg',layer =glue('muestreo_{sitio[i]}'))
  ptos <- st_transform(ptos,crs(rast(files[1])))

  map_df(files,function(file){
    bio <- rast(file)
    df <- terra::extract(bio,ptos) 
    df |> 
      mutate(sitio = sit[i],
             sample = paste0(code[i],ptos$muestra),
             date=ymd(str_extract(file,'[0-9]{8}')),
             index = names(bio)) |> 
      rename(value := !!names(bio)) |> 
      relocate(value,.after = index)
  },.progress = list(
    type = "iterator", 
    format = "Calculating {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
})

library(readr)

nubes <- read_csv('data/data_processed/fechas_nubes.csv') |> 
  mutate(date_sentinel = dmy(date_sentinel))

data_bio <- data  |>  
  left_join(nubes,by=c('sitio' = 'site','date' = 'date_sentinel'))  |> 
  filter(cloud_percent < 20)  |> 
  select(-cloud_percent)

write_rds(data_bio,'data/data_processed/data_bio_sentinel2.rds')


library(ggplot2)

data_bio |> filter(sitio == 'villa_baviera') |> 
  ggplot(aes(date,value,color=sample)) +
  geom_point() +
  geom_line() +
  geom_smooth(span=0.07,se=FALSE,color='orange') +
  scale_x_date(date_breaks = '1 week')+
  facet_grid(index~sitio,scales = 'free') +
  theme(axis.text.x = element_text(angle = 90))
