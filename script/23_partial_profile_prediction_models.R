library(DALEX)
library(DALEXtra)
library(tidymodels)
library(tidyverse)
library(xgboost)
library(glue)
library(fs)

files <- dir_ls('data/processed/modelos/',regexp = 'prediccion_(rec1)(.*)XGBoost')

labels_models <-  files |> basename() |> str_remove('prediccion_') |> str_remove('.rds')

data <- files |> 
  map(\(file){
    lead <- str_extract(file,'lead_[0-9]')
    data <- read_rds(glue('data/processed/rds/data_indices_prediccion_{lead}_mes.rds')) |> 
      select(1:44,cosecha)
    name <- file |> basename() |> str_remove('prediccion_') |> str_remove('.rds')
    #filter(sitio != "villa_baviera_2020-2021")
    
    #2. Definir subgrupos de datos para el modelado ----
    set.seed(987)
    #splits <- group_initial_split(data,group = 'sitio')
    splits <- initial_split(data)
    #splits <- group_initial_split(data,group = fecha)
    
    biom_train <- training(splits)
    biom_test <- testing(splits)
    
    model_rec <- recipe(cosecha~.,data = biom_train ) |>
      update_role(sitio, new_role = 'dont_use') |> 
      #update_role(fecha, new_role = 'dont_use') |> 
      step_impute_knn(all_numeric_predictors()) |> 
      step_normalize(all_numeric_predictors()) |> 
      step_corr(starts_with("S2|PS|S1")) |> 
      step_dummy(all_nominal_predictors(), one_hot = TRUE)
    
    explainer_rf <- 
      explain_tidymodels(
        read_rds(file) , 
        data = model_rec |> prep() |> bake(new_data = NULL), 
        y = biom_train$cosecha,
        label = name,
        verbose = TRUE
      )
    
    vip_rf <- model_parts(explainer_rf, 
                          loss_function = loss_root_mean_square,
                          label = name)
    model_profile_rf1 <- model_profile(explainer_rf, type = "partial", variables = c("sm_mm", "S1_VH", "S1_VV", "S1_VH_VV"))
    model_profile_rf1$agr_profiles
    #plot(model_profile_rf1,variables = c("sm_mm", "S1_VH", "S1_VV", "S1_VH_VV"))
  })

do.call(rbind,data) |> plot() + theme_bw()
