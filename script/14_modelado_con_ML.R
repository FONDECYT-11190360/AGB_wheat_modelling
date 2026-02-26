library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)

#0. Variables a seleccionar del análisis de correlacion

vars <- c("sitio", "fecha", "pp_cumsum", "sm_mm","gdd_cumsum","S2_B1", "S2_B6", "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI2", "S2_SWIR11_MCARI", "S2_SWIR11_TCARI","S2_SWIR12_MCARI", "S2_CVI", "S2_NDRE3", "S2_NDRE_NDVI", "S2_WI1", "S2_TCARI_OSAVI_8A", "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A", "S1_VV", "S1_VH", "S1_VH_VV", "PS_B3", "PS_B8", "PS_SR", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI", "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum", "S2_B1_cumsum","S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum", "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum", "PS_B5", "PS_TCARI_OSAVI", "PS_MCARI_OSAVI", "PS_NDRE_NDVI", "biomasa")

#1. Leer los datos ----
# Se agrega sitio_temporada y temporada como columnas de identificación (no predictores)

data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  select(-muestra) |>
  select(all_of(vars), sitio_temporada, temporada)

# No se hace train/test split — LOSO es la estrategia de evaluación

#2. Especificación de los modelos a utilizar ----

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

##
glmnet_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) |>
  set_engine("glmnet") |>
  set_mode("regression")

# neural network

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
) |>
  set_engine("kknn")

#3. Preprocesamiento ----
# Recetas definidas sobre `data` completo; sitio, fecha, sitio_temporada y temporada
# reciben roles no-predictor para que no sean usados como features.
# Los pasos step_* se estiman únicamente sobre el fold de entrenamiento en cada split LOSO.

model_rec_todo <- recipe(biomasa ~ ., data = data) |>
  update_role(sitio,           new_role = 'dont_use') |>
  update_role(fecha,           new_role = 'dont_use') |>
  update_role(sitio_temporada, new_role = 'ID') |>
  update_role(temporada,       new_role = 'ID') |>
  step_impute_knn(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

model_rec_s2 <- model_rec_todo |>
  step_rm(pp_cumsum, sm_mm, starts_with(c('S1', 'PS')))

model_rec_s2_clima <- model_rec_todo |>
  step_rm(starts_with(c('S1', 'PS')))

model_rec_ps <- model_rec_todo |>
  step_rm(pp_cumsum, sm_mm, starts_with(c('S1', 'S2')))

model_rec_ps_clima <- model_rec_todo |>
  step_rm(starts_with(c('S1', 'S2')))

model_rec_s1 <- model_rec_todo |>
  step_rm(pp_cumsum, sm_mm, starts_with(c('S2', 'PS')))

model_rec_s1_clima <- model_rec_todo |>
  step_rm(starts_with(c('S2', 'PS')))

model_rec_clima <- model_rec_todo |>
  step_rm(starts_with(c('S1', 'S2', 'PS')))

#4. Resampling LOSO y tuning ----
# 4 folds = 4 sitio-temporadas; el tuning se realiza dentro de cada fold de entrenamiento.

library(stacks)
ctrl <- control_stack_grid()

loso_folds <- group_vfold_cv(data, group = "sitio_temporada")

fold_labels <- loso_folds |>
  mutate(sitio_test = map_chr(splits, ~ unique(assessment(.x)$sitio_temporada)))

library(bonsai)
library(baguette)
biom_res <-
  workflow_set(
    preproc = list(
      rec1 = model_rec_todo,
      rec2 = model_rec_s1,
      rec3 = model_rec_s1_clima,
      rec4 = model_rec_clima,
      rec5 = model_rec_s2,
      rec6 = model_rec_s2_clima,
      rec7 = model_rec_ps,
      rec8 = model_rec_ps_clima
    ),
    models = list(
      RF     = rf_spec,
      #SVM   = svm_spec,
      XGBoost = xgb_spec,
      #lgbm  = lgbm_spec,
      GLM    = glmnet_spec,
      bagMLP = bagMLP_spec,
      KNN    = knn_spec
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

write_rds(biom_res, 'data/processed/modelos/workflow_estimacion.rds')

autoplot(biom_res, select_best = TRUE)
df_rank <- rank_results(biom_res, select_best = TRUE)

df_rank |>
  separate(wflow_id, sep = '_', into = c('rec', 'model')) |>
  ggplot(aes(rank, mean, colour = model, shape = rec)) +
  geom_point() +
  geom_errorbar(aes(ymax = mean + std_err, ymin = mean - std_err)) +
  scale_color_manual(name = 'Model',
                     values = cols4all::c4a('color_blind'),
                     labels = c('bagMLP', 'GLMnet', 'KNN', 'RF', 'XGBoost')) +
  scale_shape_manual(name = 'Recipe',
                     labels = c('rec1 (all)',
                                'rec2 (S1)',
                                'rec3 (S1+W)',
                                'rec4 (W)',
                                'rec5 (S2)',
                                'rec6 (S2+W)',
                                'rec7 (PS)',
                                'rec8 (PS+W)'),
                     values = 10:17) +
  labs(y = 'value') +
  facet_grid(.metric ~ ., scales = 'free',
             labeller = labeller(.metric = c(mae = "MAE", rmse = "RMSE", rsq = "R²"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/figs/ranking_estimacion_modelos_rec.png',
       width = 6, height = 4, dpi = 300, scale = 1.2)

#5. Rankear modelos ----

rankings <-
  rank_results(biom_res, select_best = TRUE) |>
  mutate(method = map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[1]))

tidymodels_prefer()
df_rank <- rankings |>
  dplyr::select(rank, mean, model, wflow_id, .metric, std_err) |>
  filter(.metric == c('rsq', 'rmse')[1]) |>
  rename(Model = wflow_id) |>
  mutate(Model = str_remove(Model, 'rec_'))

#6. Métricas por fold (LOSO) ----

metrics_per_fold <- collect_metrics(biom_res, summarize = FALSE)

write_rds(metrics_per_fold, 'data/processed/modelos/loso_metrics_per_fold.rds')

#7. Predicciones out-of-fold ----
# .row es el índice de fila en el dataset original

oof_preds <- collect_predictions(biom_res) |>
  left_join(
    data |>
      mutate(row_id = row_number()) |>
      select(row_id, sitio, fecha, temporada, sitio_temporada),
    by = c(".row" = "row_id")
  )

write_rds(oof_preds, 'data/processed/modelos/loso_oof_predictions.rds')

#8. Métricas agregadas (promedio sobre folds LOSO) ----

df_metrics <- metrics_per_fold |>
  group_by(wflow_id, .metric) |>
  summarize(mean    = mean(.estimate),
            std_err = sd(.estimate) / sqrt(n()),
            .groups = "drop") |>
  rename(model = wflow_id)

write_rds(df_metrics, 'data/processed/modelos/metrics.rds')

#9. Ajuste final de modelos sobre datos completos ----
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

#10. Extraer y guardar modelos ----

seq_along(models_name) |>
  walk(\(i) {
    mname <- models_name[i]
    models_lfit[[i]] |>
      extract_fit_parsnip() |>
      write_rds(glue('data/processed/modelos/{mname}.rds'))
  })

#11. Ensamblar modelos ----

library(stacks)

model_ensemble <- stacks() |>
  add_candidates(biom_res) |>
  blend_predictions() |>
  fit_members()

write_rds(model_ensemble, 'data/processed/modelos/modelo_ensamblado.rds')
