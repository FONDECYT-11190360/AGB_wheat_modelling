library(sf)
library(terra)
library(fs)
library(tidyverse)

# 1. Muestreo regular en los sitios ----

files_sitios <- dir_ls('data/processed/shp/sitios',regexp = 'shp$')

set.seed(8765)
muestreo <- files_sitios |> 
  map(\(file){
    read_sf(file) |> 
      #st_transform(4326) |> 
      st_sample(100,type='hexagonal') |> 
      st_as_sf() |> 
      mutate(sitio = basename(file) |> str_remove('.shp'))
  })

# 2. Extraer biomasa en la fecha de cosecha ----

dirs <- dir_ls('data/processed/raster/predicciones')

data_cosecha <- map_df(1:4,\(i){
  bio <- rast(sort(dir_ls(dirs[i]),decreasing = TRUE)[1])
  df <- bind_cols(muestreo[[i]]['sitio'] |> st_drop_geometry(), 
                  terra::extract(bio,muestreo[[i]],ID = FALSE))
  
})

data_cosecha <- data_cosecha |> 
  rename(cosecha = biomasa)

# 3. Extraer biomasa desde meses antes de la cosecha
# 
data_biomasa <- map(1:4,\(lead){
  map_df(1:4,\(i){
    files <- dir_ls(dirs[i]) 
    dates <- files |> 
      str_extract('[0-9]{4}-[0-9]{2}-[0-9]{2}') |> 
      ymd()
    
    date <- sort(dates,decreasing = TRUE)[1] 
    date_lead <- date - months(lead)
    
    files_pred <- files[which(dates <= date_lead)]
    
    bio <- rast(files_pred)
    names(bio) <- dates[dates <= date_lead]
    
    df <- bind_cols(muestreo[[i]]['sitio'] |> st_drop_geometry(), 
                    terra::extract(bio,muestreo[[i]],ID = FALSE)) |> 
      as_tibble() |> 
      mutate(lead_month = lead) |> 
      relocate(lead_month,.after = sitio)
  })
    
})

# 4. Guardar set de datos de biomasa para prediccion ----

1:4 |> 
  walk(\(i){
    data <- bind_cols(data_biomasa[[i]],data_cosecha['cosecha'])
    write_rds(data,glue::glue('data/processed/rds/data_biomasa_prediccion_lead_{i}_mes.rds'))
  })


# 5. Extrae datos de predictores satelitales ----

dirs <- dir_ls('data/processed/raster/predictores')

lead <- 3
data_indices <- 1:4 |> 
  map_df(\(i){
    lead <- lead*30
    sitio <- basename(dirs[i])
    files <- dir_ls(dirs[i])
    dates <- ymd(str_extract(basename(files),"[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    date_last <- sort(dates,decreasing = TRUE)[1]
    date_lead <- date_last-days(c(lead,lead+20,lead+40))
    im <- rast(files[dates %in% date_lead])
    data <- terra::extract(im,muestreo[[i]],ID = FALSE) |> 
      mutate(sitio = sitio) |> 
      relocate(sitio,.before=pp_cumsum) |> 
      as_tibble()

    })

data_indices <- bind_cols(data_indices,data_cosecha['cosecha'])

write_rds(data_indices,glue::glue('data/processed/rds/data_indices_prediccion_lead_{lead}_mes.rds'))
