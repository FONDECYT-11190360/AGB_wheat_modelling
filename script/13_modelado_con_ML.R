library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)

#1. Leer los datos ----

data <- read_rds('data/processed/rds/dataset_full_index.rds') |> 
  select(-muestra,-temporada,fecha)

data |> glimpse()

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

model_rec <- recipe(biomasa~.,data = biom_train ) |>
  #update_role(sitio_temporada,new_role = 'dont_use') |> 
  update_role(sitio, new_role = 'dont_use') |> 
  update_role(fecha, new_role = 'dont_use') |> 
  step_normalize(all_numeric_predictors()) |> 
  step_impute_knn(all_numeric_predictors()) |> 
  step_corr(all_numeric_predictors())
  
filt_obj <- prep(model_rec,training = biom_train)
filt_te <- bake(filt_obj,biom_test)

#4. Resampling y tunning

ctrl <- control_grid(parallel_over = "everything")

set.seed(453)
vb_folds <- vfold_cv(biom_train,strata = 'sitio')

library(bonsai)
biom_res <- 
  workflow_set(
    preproc = list(rec = model_rec), 
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

#5. Rankear modelos ----

rankings <- 
  rank_results(biom_res, select_best = TRUE) |> 
  mutate(method = map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[1])) 

tidymodels_prefer()
rankings |>  
  dplyr::select(rank, mean, model, wflow_id, .metric,std_err) |> 
  filter(.metric == c('rsq','rmse')[1]) |> 
  rename(Model = wflow_id) |> 
  mutate(Model = str_remove(Model,'rec_')) 

#6. Ultimo entrenamiento de los modelos ----

xgb_res <- 
  biom_res |>  
  extract_workflow("rec_XGBoost") |>  
  finalize_workflow(
    biom_res |>  
      extract_workflow_set_result("rec_XGBoost") |>  
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))

lgbm_res <- 
  biom_res |>  
  extract_workflow("rec_lgbm") |>  
  finalize_workflow(
    biom_res |>  
      extract_workflow_set_result("rec_lgbm") |>  
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))

rf_res <- 
  biom_res |>  
  extract_workflow("rec_RF") |>  
  finalize_workflow(
    biom_res |>  
      extract_workflow_set_result("rec_RF") |>  
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))

svm_res <- 
  biom_res |>  
  extract_workflow("rec_SVM") |>  
  finalize_workflow(
    biom_res |>  
      extract_workflow_set_result("rec_SVM") |>  
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))

# 7. Metricas de los modelos ----
 
df_metrics <- bind_rows(
  tibble(collect_metrics(xgb_res),model = 'XGBoost'),
  tibble(collect_metrics(lgbm_res),model = 'lgbm'),
  tibble(collect_metrics(rf_res),model = 'RF'),
  tibble(collect_metrics(svm_res),model = 'SVM')
)

df_metrics <- df_metrics |> 
  mutate(
    .metric = toupper(.metric),
    x = -1,
    y = case_when(
      .metric == 'RSQ' ~ -2.5,
      .metric == 'MAE' ~ -2.7,
      .default = -2.9),
    unit = case_when(
      .metric == 'RSQ' ~ '',
      .default = '~~MPa'),
    
    .metric = case_when(.metric == 'RSQ' ~ 'R^2',
                        .default = .metric)
  )

write_rds(df_metrics,'data/processed/modelos/metrics.rds')

#8. Extraer y guardar modelos ----

rf_wflow_fit <- extract_workflow(rf_res)
lgbm_wflow_fit <- extract_workflow(lgbm_res)
xgb_wflow_fit <- extract_workflow(xgb_res)
svm_wflow_fit <- extract_workflow(svm_res)

write_rds(xgb_wflow_fit,'data/processed/modelos/xgboost.rds')
write_rds(lgbm_wflow_fit,'data/processed/modelos/lgbm.rds')
write_rds(rf_wflow_fit,'data/processed/modelos/random_forest.rds')
write_rds(svm_wflow_fit,'data/processed/modelos/support_vector_machine.rds')

test_results <- 
  biom_test |> 
  select(biomasa) %>%
  bind_cols(
    predict(svm_wflow_fit, new_data = biom_test),
    predict(rf_wflow_fit, new_data = biom_test),
    predict(xgb_wflow_fit, new_data = biom_test)
  ) |> bind_cols(biom_test['sitio']) |> 
  set_names(c('biomasa','pred_svm','pred_rf','pred_xgb','sitio'))

test_results|>
  pivot_longer(-c(biomasa,sitio)) |> 
  ggplot(aes(biomasa,value,colour = sitio)) + 
  geom_abline(col = 'darkblue',lwd=1,alpha =.6) +
  geom_point(alpha=.5) +
  facet_grid(.~name) +
  theme_bw()
