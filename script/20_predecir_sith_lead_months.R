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

rf_spec <- rand_forest(
  trees = 1000,
  mtry = tune(),
  min_n = tune()) |>
  set_engine("ranger", importance = 'impurity') |>
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
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune()
)  |>
  set_engine("nnet") |>
  set_mode("regression")

# K-Nearest Neighbour

knn_spec <- nearest_neighbor(
  mode = "regression",
  neighbors = tune(),         # k
  weight_func = tune(),       # "rectangular" or "triangular" (uniform vs. distance)
  dist_power = tune()         # 1 = Manhattan, 2 = Euclidean
)  |>
  set_engine("kknn")


1:4 |>
  map(\(lead) {
    data <- read_rds(glue('data/processed/rds/data_indices_prediccion_lead_{lead}_mes.rds')) |>
      select(1:44, cosecha)

    # No se hace train/test split — LOSO es la estrategia de evaluación
    # sitio ya contiene el identificador de sitio-temporada

    #2. Preprocesamiento ----
    # sitio recibe rol ID para no ser usado como predictor pero sí para LOSO grouping.
    # Los pasos step_* se estiman solo sobre el fold de entrenamiento.

    model_rec_todo <- recipe(cosecha ~ ., data = data) |>
      update_role(sitio, new_role = 'ID') |>
      step_impute_knn(all_numeric_predictors()) |>
      step_normalize(all_numeric_predictors()) |>
      step_corr(starts_with("S2|PS|S1")) |>
      step_dummy(all_nominal_predictors(), one_hot = TRUE)

    model_rec_s1_clima <- model_rec_todo |>
      step_rm(starts_with(c('S2', 'PS')))

    #3. Resampling LOSO y tuning ----
    # 4 folds = 4 sitio-temporadas

    ctrl <- control_stack_grid()

    loso_folds <- group_vfold_cv(data, group = "sitio")

    library(bonsai)

    biom_res <-
      workflow_set(
        preproc = list(rec1 = model_rec_todo),
        models = list(
          RF      = rf_spec,
          #SVM    = svm_spec,
          XGBoost = xgb_spec,
          #lgbm   = lgbm_spec,
          GLM     = glmnet_spec,
          bagMLP  = bagMLP_spec,
          KNN     = knn_spec
        )
      ) |>
      workflow_map(
        verbose   = TRUE,
        seed      = 1603,
        resamples = loso_folds,
        grid      = 10,
        metrics   = metric_set(rsq, rmse, mae),
        control   = ctrl
      )

    autoplot(biom_res, select_best = TRUE)

    #4. Rankear modelos ----

    rankings <-
      rank_results(biom_res, select_best = TRUE) |>
      mutate(method = map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[1]))

    tidymodels_prefer()
    df_rank <- rankings |>
      dplyr::select(rank, mean, model, wflow_id, .metric, std_err) |>
      filter(.metric == c('rsq', 'rmse')[1]) |>
      rename(Model = wflow_id) |>
      mutate(Model = str_remove(Model, 'rec_'))

    write_rds(df_rank,
      glue('data/processed/modelos/ranking_modelos_prediccion_resampling_lead_{lead}_meses.rds'))

    #5. Métricas por fold (LOSO) ----

    metrics_per_fold <- collect_metrics(biom_res, summarize = FALSE)

    write_rds(metrics_per_fold,
      glue('data/processed/modelos/loso_metrics_per_fold_prediccion_lead_{lead}_meses.rds'))

    #6. Predicciones out-of-fold ----

    oof_preds <- collect_predictions(biom_res) |>
      left_join(
        data |>
          mutate(row_id = row_number()) |>
          select(row_id, sitio),
        by = c(".row" = "row_id")
      )

    write_rds(oof_preds,
      glue('data/processed/modelos/loso_oof_predictions_prediccion_lead_{lead}_meses.rds'))

    #7. Métricas agregadas (promedio sobre folds LOSO) ----

    df_metrics <- metrics_per_fold |>
      group_by(wflow_id, .metric) |>
      summarize(mean    = mean(.estimate),
                std_err = sd(.estimate) / sqrt(n()),
                .groups = "drop") |>
      rename(model = wflow_id)

    write_rds(df_metrics,
      glue('data/processed/modelos/metrics_modelos_prediccion_testing_lead_{lead}_meses.rds'))

    #8. Ajuste final de modelos sobre datos completos ----
    # Se finalizan con los mejores hiperparámetros del LOSO y se ajustan sobre todos los datos.

    models_name <- df_rank$Model

    models_lfit <- models_name |>
      map(\(model) {
        biom_res |>
          extract_workflow(model) |>
          finalize_workflow(
            biom_res |>
              extract_workflow_set_result(model) |>
              select_best(metric = "rsq")
          ) |>
          fit(data = data)
      })

    #9. Extraer y guardar modelos ----

    seq_along(models_name) |>
      walk(\(i) {
        mname <- models_name[i]
        models_lfit[[i]] |>
          extract_fit_parsnip() |>
          write_rds(glue('data/processed/modelos/prediccion_{mname}_lead_{lead}.rds'))
      })
  })
