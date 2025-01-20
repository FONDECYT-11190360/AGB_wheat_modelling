
library(fs)
library(terra)
library(stringr)
library(lubridate)
library(purrr)
library(sf)

#función para calcular el kndvi
fun_kndvi <- function(nir,red,sigma){
  # knr <- exp(-(nir-red)^2/(2*sigma^2))
  # kndvi <- (1-knr) / (1+knr)
  # kndvi
  tanh(((nir-red)/(2*sigma))^2)
}

fun_ndvi <- function(nir,red){
  (nir-red)/(nir+red)
}

dir <- '/mnt/data_raw/Planetscope'
files_names <- str_split_i(dir_ls(dir,regexp = 'psscene'),'/',5)
sitios <- str_remove(str_extract(files_names,'.*_2'),'_2')
temporadas <- str_extract(files_names,'[0-9]{4}-[0-9]{4}')

bands <- list(c(1,3:4),c(2,6,8),c(2,6,8),c(1,3:4))
lyr_names <- st_layers('data/data_processed/sitios.gpkg')

data_unida <- seq_along(files_names) |> 
  map_df(function(i){
    brnir <- bands[[i]]
    temporada <- temporadas[i]
    sitio <- sitios[i]
    lyr_sel <- lyr_names$name[str_detect(lyr_names$name,paste0(sitio,'_',temporada))] |> sort()
    
    pol <- st_read('data/data_processed/sitios.gpkg',layer=lyr_sel[1])
    ptos <- st_read('data/data_processed/sitios.gpkg',layer=lyr_sel[2])
    
    pol <- st_transform(pol,32719)
    ptos <- st_transform(ptos,32719)
    names(ptos) <- tolower(names(ptos))
    dir <- '/mnt/data_raw/Planetscope'
    
    dir_sel <- dir_ls(dir)[str_detect(dir_ls(dir),paste0(sitio,'_',temporada))]
    # /hidango_2021_2022_psscene_analytic_sr_udm2/'

    files <- dir_ls(dir_sel,recurse = TRUE,regexp = '.*AnalyticMS.*tif$')

    fecha_hora <- ymd_hms(str_extract(files,'[0-9]{8}_[0-9]{6}'))

  vi <- seq_along(files) |> 
    map(function(j){
      im <- rast(files[j])
      ndvi <- fun_ndvi(im[[brnir[3]]],im[[brnir[2]]])
      kndvi <- fun_kndvi(im[[brnir[3]]]/10000,im[[brnir[2]]]/10000,.15)
      evi <- 2.5*((im[[brnir[3]]]-im[[brnir[2]]])/(im[[brnir[3]]]+6*im[[brnir[2]]]-7.5*im[[brnir[1]]]+1))
      vi <- c(ndvi,kndvi,evi)
      names(vi) <- c('ndvi','kndvi','evi')
      time(vi) <- rep(fecha_hora[j],3)
      vi <- crop(vi,vect(pol))
      return(vi)
    })
  
  vi <- rast(vi)
  
  ndvi_df <- terra::extract(vi['^ndvi'],ptos)
  kndvi_df <- terra::extract(vi['kndvi'],ptos)
  evi_df <- terra::extract(vi['evi'],ptos)
  
  ndvi_df$muestra <- ptos$muestra
  kndvi_df$muestra <- ptos$muestra
  evi_df$muestra <- ptos$muestra
  names(ndvi_df)[2:(dim(ndvi_df)[2]-1)] <- as.character(fecha_hora)
  names(kndvi_df)[2:(dim(kndvi_df)[2]-1)] <- as.character(fecha_hora)
  names(evi_df)[2:(dim(evi_df)[2]-1)] <- as.character(fecha_hora)

  library(tidyr)
  library(dplyr)
  library(ggplot2)
  
  ndvi_df <- ndvi_df |> 
    #mutate(muestra = str_trim(muestra)) |> 
    pivot_longer(-c(muestra,ID),names_to = 'fecha_hora',values_to ='ndvi') |> 
    select(-ID) |> 
    mutate(fecha_hora = as.POSIXct(fecha_hora)) 
  
  kndvi_df <- kndvi_df |> 
    #mutate(muestra = str_trim(muestra)) |> 
    pivot_longer(-c(muestra,ID),names_to = 'fecha_hora',values_to ='kndvi') |> 
    select(-ID) |> 
    mutate(fecha_hora = as.POSIXct(fecha_hora)) 
  
  evi_df <- evi_df |> 
    #mutate(muestra = str_trim(muestra)) |> 
    pivot_longer(-c(muestra,ID),names_to = 'fecha_hora',values_to ='evi') |> 
    select(-ID) |> 
    mutate(fecha_hora = as.POSIXct(fecha_hora)) 
  
  data <- left_join(ndvi_df,evi_df) |> 
    left_join(kndvi_df) |> 
    pivot_longer(ndvi:kndvi,names_to='vi') |> 
    mutate(sitio = sitio,temporada = temporada,muestra = as.numeric(muestra)) |> 
    relocate(sitio,.before = 'muestra') |> 
    relocate(temporada,.before = 'muestra') |> 
    relocate(fecha_hora,.before = 'muestra')
 return(data)
  })

data_unida |> 
  rename(site = sitio,
         season = temporada,
         dates = fecha_hora, 
         sample = muestra) |> 
  write_rds('data/data_processed/data_vis_planetscope.rds')


library(ggplot2)
library(dplyr)
data_unida |> 
  filter(vi == 'ndvi') |> 
  # group_by(sitio,vi,temporada,fecha_hora) |>
  # summarize(value = mean(value,na.rm = TRUE)) |>
  ggplot(aes(fecha_hora,value,colour=vi)) +
  geom_point() +
  geom_line() +
  facet_grid(as.factor(muestra)~sitio,scales = 'free')
