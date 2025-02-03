library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)

#0. Variables a seleccionar del análisis de correlacion

vars <- c("sitio", "fecha", "pp_cumsum", "sm_mm", "S2_B1", "S2_B6", "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI2", "S2_SWIR11_MCARI", "S2_SWIR11_TCARI","S2_SWIR12_MCARI", "S2_CVI", "S2_NDRE3", "S2_NDRE_NDVI", "S2_WI1", "S2_TCARI_OSAVI_8A", "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A", "S1_VV", "S1_VH", "S1_VH_VV", "PS_B3", "PS_B8", "PS_SR", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI", "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum", "S2_B1_cumsum","S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum", "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum", "PS_B5", "PS_TCARI_OSAVI", "PS_MCARI_OSAVI", "PS_NDRE_NDVI", "biomasa")

#1. Leer los datos ----

data <- read_rds('data/processed/rds/dataset_full_index.rds') |> 
  select(-muestra,-temporada,fecha) |> 
  select(all_of(vars))

#2. Definir subgrupos de datos para el modelado ----
set.seed(987)
splits <- initial_split(data,strata = 'sitio')

#splits <- group_initial_split(data,group = fecha)

biom_train <- training(splits)
biom_test <- testing(splits)

#3. Especificación de los modelos a utilizar ----

## XGBoost
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),                    ## first three: model complexity
  sample_size = tune(), mtry = tune(),        ## randomness
  learn_rate = tune()                         ## step size
) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

### LightGBM

lgbm_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),                    ## first three: model complexity
  sample_size = tune(), mtry = tune(),        ## randomness
  learn_rate = tune()                         ## step size
) |> 
  set_engine("lightgbm") |> 
  set_mode("regression")

## Random Forest

rf_spec <-rand_forest(
  trees = 1000,
  mtry = tune(),
  min_n = tune()) |> 
  set_engine("ranger",importance = 'impurity') |> 
  set_mode("regression")

## Support Vector Machine

svm_spec <-
  svm_rbf(cost = tune(), 
          rbf_sigma = tune())  |> 
  set_mode("regression")  |> 
  set_engine("kernlab")

## Multilayer perceptron

glmnet_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine("glmnet") |> 
  set_mode("regression") 

#4. Preprocesamiento

model_rec_todo <- recipe(biomasa~.,data = biom_train ) |>
  #update_role(sitio_temporada,new_role = 'dont_use') |> 
  update_role(sitio, new_role = 'dont_use') |> 
  update_role(fecha, new_role = 'dont_use') |> 
  step_impute_knn(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

model_rec_s2 <- model_rec_todo |> 
  step_rm(pp_cumsum, sm_mm,starts_with('S1|PS'))

model_rec_s2_clima <- model_rec_todo |>
  step_rm(starts_with('S1|PS'))

model_rec_ps <- model_rec_todo |> 
  step_rm(pp_cumsum, sm_mm,starts_with('S1|S2'))

model_rec_ps_clima <- model_rec_todo |> 
  step_rm(starts_with('S1|S2'))

model_rec_s1 <- model_rec_todo |> 
  step_rm(pp_cumsum, sm_mm,starts_with('S2|PS'))

model_rec_s1_clima <- model_rec_todo |> 
  step_rm(starts_with('S1|PS'))

model_rec_clima <- model_rec_todo |> 
  step_rm(starts_with('S1|S2|PS'))

# filt_obj <- prep(model_rec_todo,training = biom_train)
# filt_te <- bake(filt_obj,biom_test)

#4. Resampling y tunning

ctrl <- control_stack_grid()

set.seed(453)
vb_folds <- vfold_cv(biom_train,strata = 'sitio')

library(bonsai)
biom_res <- 
  workflow_set(
    preproc = list(rec1 = model_rec_todo,
                   rec2 = model_rec_s1,
                   rec3 = model_rec_s1_clima,
                   rec4 = model_rec_clima,
                   rec5 = model_rec_s2,
                   rec6 = model_rec_s2_clima,
                   rec7 = model_rec_ps,
                   rec8 = model_rec_ps_clima,
                   rec9 = model_rec_clima), 
    models = list(
      RF = rf_spec,
      SVM = svm_spec,
      XGBoost = xgb_spec,
      lgbm = lgbm_spec,
      glm = glmnet_spec
      #MLP = mlp_spec
    )
  ) |>  
  workflow_map(
    verbose = TRUE,
    seed = 1603,
    resamples = vb_folds,
    grid = 10,
    metrics = metric_set(rsq,rmse, mae),
    control = ctrl
  )

autoplot(biom_res,select_best = TRUE)

#5. Rankear modelos ----

rankings <- 
  rank_results(biom_res, select_best = TRUE) |> 
  mutate(method = map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[1])) 

tidymodels_prefer()
df_rank <- rankings |>  
  dplyr::select(rank, mean, model, wflow_id, .metric,std_err) |> 
  filter(.metric == c('rsq','rmse')[1]) |> 
  rename(Model = wflow_id) |> 
  mutate(Model = str_remove(Model,'rec_')) 

#6. Ultimo entrenamiento de los modelos ----

models_name <- df_rank$Model

models_lfit <- models_name |> 
  map(\(model){
    biom_res |>  
    extract_workflow(model) |>  
    finalize_workflow(
      biom_res |>  
        extract_workflow_set_result(model) |>  
        select_best(metric = "rsq")
    ) |>  
    last_fit(split = splits, metrics = metric_set(rsq,rmse,mae)) 
  })

# 7. Metricas de los modelos ----

df_metrics <- seq_along(models_name) |> 
  map_df(\(i){
    tibble(collect_metrics(models_lfit[[i]]),model = models_name[i])
  })

# df_metrics <- df_metrics |> 
#   mutate(
#     .metric = toupper(.metric),
#     x = -1,
#     y = case_when(
#       .metric == 'RSQ' ~ -2.5,
#       .metric == 'MAE' ~ -2.7,
#       .default = -2.9),
#     unit = case_when(
#       .metric == 'RSQ' ~ '',
#       .default = '~~MPa'),
#     
#     .metric = case_when(.metric == 'RSQ' ~ 'R^2',
#                         .default = .metric)
#   )

write_rds(df_metrics,'data/processed/modelos/metrics.rds')

#8. Extraer y guardar modelos ----

seq_along(models_name) |> 
  walk(\(i){
    mname <- models_name[i]
    models_lfit[[i]] |> 
      extract_fit_parsnip() |> 
      write_rds(glue('data/processed/modelos/{mname}.rds'))
  })

#9. ensamblar modelos ----

library(stacks)

model_ensemble <- stacks() |> 
  add_candidates(biom_res) |> 
  blend_predictions() |> 
  fit_members()

write_rds(model_ensemble,'data/processed/modelos/modelo_ensamblado.rds')

autoplot(model_ensemnble)

res <- models_lfit |> 
  map(possibly(\(model){
    model_ex <- extract_workflow(model)
    model_bio_test <- 
    biom_test['biomasa'] |>
    bind_cols(predict(model_ex,biom_test))
    
  },NA_real_))
  

  
# Explicación del modelo
# 
library(DALEX)
library(DALEXtra)
explainer_rf <- 
  explain_tidymodels(
    model_ensemnble, 
    data = model_rec_todo  |>  prep()  |>  bake(new_data = biom_train), 
    y = biom_train$biomasa,
    label = "Ensamblado",
    verbose = FALSE
  )

vip_rf <- model_parts(explainer_rf, loss_function = loss_root_mean_square)
plot(vip_rf)
