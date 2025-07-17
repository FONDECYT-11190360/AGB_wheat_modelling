library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)
library(stacks)
library(baguette)

#1. Especificación de los modelos a utilizar ----

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

## linear models with regularization
glmnet_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine("glmnet") |> 
  set_mode("regression") 

# neural networks
bagMLP_spec <- bag_mlp(
  hidden_units = tune(), # Tune the number of hidden units
  penalty = tune(), # Tune the regularization penalty
  epochs = tune()  # Tune the dropout rate
)  |> 
  set_engine("nnet") |> 
  set_mode("regression")

# K- Nearest neighbour

knn_spec <- nearest_neighbor(
  mode = "regression",
  neighbors = tune(),         # k
  weight_func = tune(),       # "rectangular" or "triangular" (uniform vs. distance)
  dist_power = tune()         # 1 = Manhattan, 2 = Euclidean
)  |> 
  set_engine("kknn")


1:4 |> #####
  map(\(lead){
  data <- read_rds(glue('data/processed/rds/data_indices_prediccion_lead_{lead}_mes.rds')) |> 
    select(1:44,cosecha)
  #filter(sitio != "villa_baviera_2020-2021")
    
  #2. Definir subgrupos de datos para el modelado ----
  set.seed(987)
  #splits <- group_initial_split(data,group = 'sitio')
  splits <- initial_split(data)
  #splits <- group_initial_split(data,group = fecha)
  
  biom_train <- training(splits)
  biom_test <- testing(splits)
  
  #4. Preprocesamiento
  
  model_rec_todo <- recipe(cosecha~.,data = biom_train ) |>
    update_role(sitio, new_role = 'dont_use') |> 
    #update_role(fecha, new_role = 'dont_use') |> 
    step_impute_knn(all_numeric_predictors()) |> 
    step_normalize(all_numeric_predictors()) |> 
    step_corr(starts_with("S2|PS|S1")) |> 
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
  
  model_rec_s1_clima <- model_rec_todo |> 
    step_rm(starts_with(c('S2','PS')))

  #4. Resampling y tunning
  
  ctrl <- control_stack_grid()
  
  set.seed(453)
  vb_folds <- vfold_cv(biom_train)
  
  library(bonsai)
  # library(doMC)
  # registerDoMC(cores = parallel::detectCores()-1)
  
  biom_res <- 
    workflow_set(
      preproc = list(rec1 = model_rec_todo
                     # rec2 = model_rec_s1,
                     #rec3 = model_rec_s1_clima
                     # rec4 = model_rec_s2,
                     # rec5 = model_rec_s2_clima,
                     # rec6 = model_rec_ps,
                     # rec7 = model_rec_ps_clima
                     ), 
      models = list(
        RF = rf_spec,
        #SVM = svm_spec,
        XGBoost = xgb_spec,
        #lgbm = lgbm_spec
        GLM = glmnet_spec,
        bagMLP = bagMLP_spec,
        KNN = knn_spec
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
  
  write_rds(df_rank,glue('data/processed/modelos/ranking_modelos_prediccion_resampling_lead_{lead}_meses.rds'))
  
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
  
  write_rds(df_metrics,glue('data/processed/modelos/metrics_modelos_prediccion_testing_lead_{lead}_meses.rds'))

#8. Extraer y guardar modelos ----

seq_along(models_name) |> 
  walk(\(i){
    mname <- models_name[i]
    models_lfit[[i]] |> 
      extract_fit_parsnip() |> 
      write_rds(glue('data/processed/modelos/prediccion_{mname}_lead_{lead}.rds'))
    
    })
  })




  
 