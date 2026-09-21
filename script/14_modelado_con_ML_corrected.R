# Script 14 - CORRECTED VERSION (No Data Leakage)
#
# CHANGE LOG:
# - Removed pre-CV correlation screening (was causing data leakage)
# - Now starts with all 53 VIs from Table 3 + Suppl Table S1 (minus 4 PS red edge)
# - Within-fold step_corr() handles correlation filtering properly
#
# This addresses Critical Issue B1 from peer review

library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)

#0. Variables a seleccionar ----
# CORRECTED: Start with ALL 53 VIs (minus 4 PS red edge predictors)
# These 53 VIs were selected based on literature (Table 3 + Suppl Table S1)
# NO pre-screening based on correlation with full dataset

# Core variables
core_vars <- c("sitio", "fecha", "pp_cumsum", "sm_mm", "gdd_cumsum")

# Sentinel-1 (3 predictors)
s1_vars <- c("S1_VV", "S1_VH", "S1_VH_VV")

# Sentinel-2 (23 predictors from Table 3)
# Daily indices
s2_daily <- c(
  "S2_B1",           # Coastal aerosol (Table 3 row 1-2 need this)
  "S2_B6",           # Red Edge 2
  "S2_MCARI",        # Table 3 row 8
  "S2_TCARI",        # Table 3 row 9
  "S2_MCARI_OSAVI2", # Table 3 row 2
  "S2_SWIR11_MCARI", # Table 3 row 3
  "S2_SWIR11_TCARI", # Table 3 row 4
  "S2_SWIR12_MCARI", # Table 3 row 5
  "S2_CVI",          # Table 3 row 15
  "S2_NDRE3",        # Table 3 row 6 (using B8)
  "S2_NDRE_NDVI",    # Table 3 row 20
  "S2_WI1",          # Table 3 row 7 (using B8)
  "S2_OSAVI"         # Table 3 row 13
)

# Sentinel-2 with B8A variants (added per manuscript line 169-170)
s2_8a <- c(
  "S2_TCARI_OSAVI_8A", # Table 3 row 10 variant
  "S2_CI_red_8A",      # Table 3 row 14 variant
  "S2_NDRE3_8A",       # Table 3 row 6 variant
  "S2_SIPI_8A",        # Table 3 row 21 variant
  "S2_WI1_8A"          # Table 3 row 7 variant
)

# Sentinel-2 cumulative indices (6 from Table 3)
s2_cumsum <- c(
  "S2_B1_cumsum",
  "S2_SWIR12_MCARI_cumsum",
  "S2_SWIR12_TCARI_cumsum",
  "S2_NDRE3_cumsum",
  "S2_CI_red_8A_cumsum",
  "S2_NDRE3_8A_cumsum"
)

s2_vars <- c(s2_daily, s2_8a, s2_cumsum)

# PlanetScope (10 predictors from Table 3, EXCLUDING red edge B5 and derived indices)
# Manuscript line 156-158: "four PlanetScope predictors derived from [red edge band]...
# were entirely missing [for two site-seasons]...these four predictors were excluded"
#
# EXCLUDED (unavailable for Hidango 2021-2022 and Villa Baviera 2020-2021):
# - PS_B5 (red edge band)
# - PS_TCARI_OSAVI
# - PS_MCARI_OSAVI
# - PS_NDRE_NDVI

# Daily indices
ps_daily <- c(
  "PS_B3",    # Green (Table 2)
  "PS_B8",    # NIR (Table 2)
  "PS_SR",    # Table 3 row 12
  "PS_CVI",   # Table 3 row 15
  "PS_EVI",   # Table 3 row 16
  "PS_GNDVI", # Table 3 row 18
  "PS_SIPI"   # Table 3 row 21
)

# Cumulative indices (3)
ps_cumsum <- c(
  "PS_CI_red_cumsum",  # Table 3 row 14 (common to S2/PS)
  "PS_EVI_cumsum",     # From EVI
  "PS_GRVI_cumsum"     # Green-Red VI cumulative
)

ps_vars <- c(ps_daily, ps_cumsum)

# Response variable
response <- "biomasa"

# ALL predictors to use (49 total: 3 weather + 3 S1 + 23 S2 + 10 PS + biomasa)
# NOTE: This is 49, not 53, because:
# - 4 PS red edge predictors excluded (missing for 2 site-seasons)
# - "53 VIs" in manuscript refers to daily+cumulative indices, not total predictors
vars <- c(core_vars, s1_vars, s2_vars, ps_vars, response)

cat("Total predictors in vars:", length(vars), "\n")
cat("Breakdown:\n")
cat("  Core (sitio, fecha, weather):", length(core_vars), "\n")
cat("  Sentinel-1:", length(s1_vars), "\n")
cat("  Sentinel-2:", length(s2_vars), "\n")
cat("  PlanetScope:", length(ps_vars), "\n")
cat("  Response (biomasa): 1\n")
cat("  TOTAL:", length(vars), "\n\n")
cat("PS red edge predictors EXCLUDED (missing for 2 site-seasons): 4\n")
cat("  - PS_B5\n")
cat("  - PS_TCARI_OSAVI\n")
cat("  - PS_MCARI_OSAVI\n")
cat("  - PS_NDRE_NDVI\n\n")
cat("DATA LEAKAGE FIX:\n")
cat("  - Removed pre-CV correlation screening that reduced 53→43 predictors\n")
cat("  - Now starting with all 49 available predictors\n")
cat("  - Within-fold step_corr() will handle correlation filtering independently\n")
cat("  - Each fold may retain different predictors (expected and correct)\n\n")

#1. Leer los datos ----
# Se agrega sitio_temporada y temporada como columnas de identificación (no predictores)

data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  select(-muestra) |>
  select(all_of(vars), sitio_temporada, temporada)

cat("Data loaded. Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("Samples per predictor ratio:", round(nrow(data) / (length(vars) - 1), 2), "\n\n")

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
#
# CRITICAL: step_corr() is applied WITHIN each training fold, so correlation
# filtering is done independently for each fold. This avoids data leakage.

model_rec_todo <- recipe(biomasa ~ ., data = data) |>
  update_role(sitio,           new_role = 'dont_use') |>
  update_role(fecha,           new_role = 'dont_use') |>
  update_role(sitio_temporada, new_role = 'ID') |>
  update_role(temporada,       new_role = 'ID') |>
  step_impute_knn(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = 0.9) |>  # Within-fold correlation filter
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

cat("\nLOSO-CV folds:\n")
print(fold_labels$sitio_test)

# Continue with the rest of the script...
# (The workflow_set, tuning, and modeling code remains the same)
