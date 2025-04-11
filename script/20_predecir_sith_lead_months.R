library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)
library(stacks)

#1. Leer los datos ----

data <- read_rds('data/processed/rds/data_indices_prediccion_lead_4_mes.rds') |> 
  filter(sitio != "villa_baviera_2020-2021")
  
#2. Definir subgrupos de datos para el modelado ----
set.seed(987)
#splits <- group_initial_split(data,group = 'sitio')
splits <- initial_split(data)
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

## glm regression models

glmnet_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine("glmnet") |> 
  set_mode("regression") 

# multilayer preceptron NN

mlp_spec <- mlp(
  hidden_units =tune(),
  penalty = tune(),
) |> 
  set_engine("keras") |> 
  set_mode("regression")  |>  
  translate()

#4. Preprocesamiento

model_rec_todo <- recipe(cosecha~.,data = biom_train ) |>
  update_role(sitio, new_role = 'dont_use') |> 
  step_impute_knn(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_zv(all_numeric_predictors())
  
model_rec_s2 <-  recipe(cosecha~.,data = biom_train |> select(sitio,cosecha,starts_with('S2'))) |>
  update_role(sitio, new_role = 'dont_use') |> 
  step_impute_linear(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_zv()

model_rec_s2_clima <- recipe(cosecha~.,data = biom_train |> select(sitio,cosecha,pp_cumsum,sm_mm,starts_with('S2'))) |>
  update_role(sitio, new_role = 'dont_use') |> 
  step_impute_linear(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_zv()

model_rec_ps <- recipe(cosecha~.,data = biom_train |> select(sitio,cosecha,starts_with('PS'))) |>
  update_role(sitio, new_role = 'dont_use') |> 
  step_impute_linear(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_zv()
  
model_rec_ps_clima <- recipe(cosecha~.,data = biom_train |> select(sitio,cosecha,pp_cumsum,sm_mm,starts_with('PS'))) |>
  update_role(sitio, new_role = 'dont_use') |> 
  step_impute_linear(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_zv()

model_rec_s1 <- recipe(cosecha~.,data = biom_train |> select(sitio,cosecha,starts_with('S1'))) |>
  update_role(sitio, new_role = 'dont_use') |> 
  step_impute_linear(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_zv()

model_rec_s1_clima <- recipe(cosecha~.,data = biom_train |> select(sitio,cosecha,pp_cumsum,sm_mm,starts_with('S1'))) |>
  update_role(sitio, new_role = 'dont_use') |> 
  step_impute_knn(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_zv()

model_rec_clima <- recipe(cosecha~.,data = biom_train |> select(sitio,cosecha,pp_cumsum,sm_mm)) |>
  update_role(sitio, new_role = 'dont_use') |> 
  step_impute_knn(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_zv()


# filt_obj <- prep(model_rec_todo,training = biom_train)
# filt_te <- bake(filt_obj,biom_test)

#4. Resampling y tunning

ctrl <- control_stack_grid()

set.seed(453)
vb_folds <- group_vfold_cv(biom_train,group = 'sitio')
vb_folds <- vfold_cv(biom_train)

library(bonsai)
library(doMC)
registerDoMC(cores = parallel::detectCores()-1)

biom_res <- 
  workflow_set(
    preproc = list(rec1 = model_rec_todo
                   # rec2 = model_rec_s1,
                   # rec3 = model_rec_s1_clima,
                   # rec4 = model_rec_s2,
                   # rec5 = model_rec_s2_clima,
                   # rec6 = model_rec_ps,
                   # rec7 = model_rec_ps_clima
                   ), 
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

# 7. Metricas de los modelos en el set de testeo ----

df_metrics <- seq_along(models_name) |> 
  map_df(\(i){
    tibble(collect_metrics(models_lfit[[i]]),model = models_name[i])
  })

#8. Explicación del modelo con mejor performance
# 
library(DALEX)
library(DALEXtra)
explainer_rf <- 
  explain_tidymodels(
    biom_res, 
    data = prep(model_rec_todo,training = biom_train) |> bake(biom_test), 
    y = biom_train$cosecha,
    label = "Ensamblado",
    verbose = FALSE
  )

vip_rf <- model_parts(explainer_rf, loss_function = loss_root_mean_square)
plot(vip_rf)


#9. ensamblar modelos ----

library(stacks)

model_ensemble <- stacks() |> 
  add_candidates(biom_res) |> 
  blend_predictions() |> 
  fit_members()

#write_rds(model_ensemble,'data/processed/modelos/modelo_ensamblado_prediccion.rds')

autoplot(model_ensemble)

res <- models_lfit |> 
  map(possibly(\(model){
    model_ex <- extract_workflow(model)
    model_bio_test <- 
      biom_test['biomasa'] |>
      bind_cols(predict(model_ex,biom_test))
    
  },NA_real_))

#10. Explicación del modelo
# 
library(DALEX)
library(DALEXtra)
explainer_rf <- 
  explain_tidymodels(
    model_ensemble, 
    data = prep(model_rec_todo,training = biom_train) |> bake(biom_test), 
    y = biom_train$cosecha,
    label = "Ensamblado",
    verbose = FALSE
  )

vip_rf <- model_parts(explainer_rf, loss_function = loss_root_mean_square)
plot(vip_rf)

