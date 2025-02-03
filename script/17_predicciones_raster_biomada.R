library(tidyverse)
library(terra)
library(fs)

modelo <- read_rds('data/processed/modelos/modelo_ensamblado.rds')

files <- dir_ls('data/processed/raster/predictores',recurse = TRUE,
                type = 'file')

walk(files,\(file){
  im <- rast(file)
  out <- im[[1]]
  names(out) <- 'biomasa'
  

  im_df <- as.data.frame(im) |> 
    mutate(sitio = 'A',fecha = ymd("2020-01-01"))
  
  pred_df <- predict(modelo,im_df)
  values(out) <- pred_df$.pred
  
  file_new <- str_replace(file,'predictores','predicciones')
  writeRaster(out,file_new)
})
