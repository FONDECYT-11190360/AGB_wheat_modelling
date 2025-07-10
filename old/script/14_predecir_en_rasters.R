library(tidymodels)
library(tidyverse)
library(terra)
library(fs)

#cargar modelo con mejor performance
xgb_wflow_fit  <- read_rds('data/processed/modelos/xgboost.rds')

#nombre de las variables predictoras
names_preds <- xgb_wflow_fit$pre$mold$predictors |> names()
names_preds <- paste0(names_preds,'.tif')

dir <- '/home/francisco/Documentos/data_trigo/indices/hidango_2021-2022/'

preds_rast <- names_preds |> 
  map(possibly(
    \(n){
    rast(paste0(dir,n))
  })
)


names_preds|> lapply(\(x) str_match(dir_ls(dir) ,x))

                      