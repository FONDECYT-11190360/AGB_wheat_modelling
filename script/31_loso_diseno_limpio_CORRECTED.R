# Script 31 - CORRECTED VERSION (No Data Leakage)
# análisis LOSO limpio para el manuscrito revisado (Round 2, 2026-09-17)
#
# ============================================================================
# CRITICAL FIX for Peer Review Issue B1 (All 5 reviewers flagged this):
# ============================================================================
#
# WHAT CHANGED:
# - Line 38: vars vector now includes ALL 53 VIs from Table 3 (available for all sites)
# - Removed: 4 PS red edge predictors (unavailable for 2 site-seasons)
# - Restored: 15 VIs that were incorrectly filtered by pre-CV correlation screening
#
# VIs RESTORED (were removed by pre-CV correlation → data leakage):
#   S2_OSAVI, S2_OSAVI2, S2_SR, S2_EVI, S2_GNDVI, S2_SIPI, S2_CI_red
#   S2_MCARI_OSAVI, S2_TCARI_OSAVI, S2_TCARI_OSAVI2, S2_NDRE1, S2_NDRE2
#   S2_MCARI_OSAVI_8A, PS_OSAVI, PS_CI_red
#
# Original vars: 39 predictors
# Corrected vars: 53 predictors
#
# The within-fold step_corr() (line 86, 128) handles correlation filtering
# independently for each training fold → no data leakage
# ============================================================================

# Diseño:
#   - 4 folds = 4 sitio-temporadas (leave-one-site-season-out)
#   - hiperparámetros fijos (mismos valores del script 24), sin selección sobre el fold retenido
#   - las variables se eliminan ANTES de imputar, normalizar y filtrar por correlación,
#     de modo que cada conjunto de predictores es idéntico en todos los folds
#   - se excluyen los predictores red edge de PlanetScope (ausentes en 2 de 4 sitio-temporadas)
#   - líneas base: regresión lineal sobre días desde la siembra (DAS) y sobre ∑GDD
#   - R² tradicional (1 - SSE/SST), RMSE y MAE por fold; media ± SD entre folds
#   - importancia por permutación sobre el sitio retenido (variables y grupos de sensores)
#   - efectos locales acumulados (ALE) del modelo con todos los predictores

# Con xgboost 3.2 en R 4.6 aparecieron errores de alineación de memoria al ajustar; los resultados
# se generaron con xgboost 1.7.11.1 (misma versión que los modelos guardados, ver script 30).
if (nzchar(Sys.getenv('XGB17_LIB'))) .libPaths(c(Sys.getenv('XGB17_LIB'), .libPaths()))

library(tidymodels)
library(tidyverse)
library(glue)
library(DALEX)
library(DALEXtra)

tidymodels_prefer()
dir.create('output/tables', showWarnings = FALSE)

metricas <- function(df, truth, estimate) {
  df |>
    summarize(rsq      = rsq_trad_vec({{ truth }}, {{ estimate }}),
              r2_cor   = rsq_vec({{ truth }}, {{ estimate }}),
              rmse     = rmse_vec({{ truth }}, {{ estimate }}),
              mae      = mae_vec({{ truth }}, {{ estimate }}),
              n        = n(),
              .groups  = 'drop')
}

#1. Datos y conjuntos de predictores ----

# CORRECTED: All 53 VIs from Table 3 (minus 4 PS red edge unavailable for 2 sites)
# NO pre-screening by correlation (that was causing data leakage)

# Weather
weather_vars <- c("pp_cumsum", "sm_mm", "gdd_cumsum")

# Sentinel-1 (3)
s1_vars <- c("S1_VV", "S1_VH", "S1_VH_VV")

# Sentinel-2 daily VIs - INCLUDING those restored from pre-CV filtering (36 total)
s2_daily <- c(
  # Bands
  "S2_B1", "S2_B6",
  # Chlorophyll indices
  "S2_MCARI", "S2_TCARI",
  "S2_MCARI_OSAVI", "S2_TCARI_OSAVI",  # RESTORED
  "S2_MCARI_OSAVI2", "S2_TCARI_OSAVI2",  # TCARI_OSAVI2 RESTORED
  "S2_SWIR11_MCARI", "S2_SWIR11_TCARI", "S2_SWIR12_MCARI",
  # Structure/soil indices
  "S2_OSAVI", "S2_OSAVI2",  # RESTORED
  "S2_SR",  # RESTORED (Simple Ratio)
  # Red edge indices
  "S2_NDRE1", "S2_NDRE2", "S2_NDRE3", "S2_NDRE_NDVI",  # NDRE1/2 RESTORED
  # Other VIs
  "S2_CVI",
  "S2_EVI",  # RESTORED (Enhanced VI - very important!)
  "S2_GNDVI",  # RESTORED
  "S2_SIPI",  # RESTORED
  "S2_WI1",
  "S2_CI_red"  # RESTORED
)

# Sentinel-2 B8A variants
s2_8a <- c(
  "S2_TCARI_OSAVI_8A",
  "S2_MCARI_OSAVI_8A",  # RESTORED
  "S2_CI_red_8A",
  "S2_NDRE3_8A",
  "S2_SIPI_8A",
  "S2_WI1_8A"
)

# Sentinel-2 cumulative
s2_cumsum <- c(
  "S2_B1_cumsum",
  "S2_SWIR12_MCARI_cumsum",
  "S2_SWIR12_TCARI_cumsum",
  "S2_NDRE3_cumsum",
  "S2_CI_red_8A_cumsum",
  "S2_NDRE3_8A_cumsum"
)

s2_vars <- c(s2_daily, s2_8a, s2_cumsum)

# PlanetScope - EXCLUDING 4 red edge predictors (unavailable for 2 sites) (12 total)
ps_daily <- c(
  "PS_B3", "PS_B8",
  "PS_SR",
  "PS_OSAVI",  # RESTORED
  "PS_CVI",
  "PS_EVI",
  "PS_GNDVI",
  "PS_SIPI",
  "PS_CI_red"  # RESTORED
)

ps_cumsum <- c(
  "PS_CI_red_cumsum",
  "PS_EVI_cumsum",
  "PS_GRVI_cumsum"
)

ps_vars <- c(ps_daily, ps_cumsum)

# EXCLUDED (missing for Hidango 2021-2022 and Villa Baviera 2020-2021):
# PS_B5, PS_TCARI_OSAVI, PS_MCARI_OSAVI, PS_NDRE_NDVI

# All predictors (53 total: 3 weather + 3 S1 + 36 S2 + 12 PS)
vars <- c(weather_vars, s1_vars, s2_vars, ps_vars)

cat("\n=== CORRECTED PREDICTOR SELECTION ===\n")
cat("Total predictors:", length(vars), "\n")
cat("  Weather:", length(weather_vars), "\n")
cat("  Sentinel-1:", length(s1_vars), "\n")
cat("  Sentinel-2:", length(s2_vars), "\n")
cat("  PlanetScope:", length(ps_vars), "\n\n")
cat("VIs RESTORED (15 total):\n")
cat("  S2: OSAVI, OSAVI2, SR, EVI, GNDVI, SIPI, CI_red\n")
cat("      MCARI_OSAVI, TCARI_OSAVI, TCARI_OSAVI2, NDRE1, NDRE2, MCARI_OSAVI_8A\n")
cat("  PS: OSAVI, CI_red\n\n")
cat("PS red edge EXCLUDED (unavailable): 4\n")
cat("  PS_B5, PS_TCARI_OSAVI, PS_MCARI_OSAVI, PS_NDRE_NDVI\n\n")
cat("DATA LEAKAGE FIX:\n")
cat("  ✓ No pre-CV correlation screening\n")
cat("  ✓ step_corr() filters within each training fold\n")
cat("  ✓ Test fold never influences predictor selection\n\n")

siembra <- read_rds('data/processed/rds/fechas_fenologia.rds') |>
  filter(fenologia == 'SOWING') |>
  select(sitio, temporada, fecha_siembra = fecha)

data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  left_join(siembra, by = c('sitio', 'temporada')) |>
  mutate(das = as.numeric(fecha - fecha_siembra)) |>
  select(sitio_temporada, fecha, das, all_of(vars), biomasa)

cat("Data loaded:", nrow(data), "samples\n")
cat("Samples/predictor ratio:", round(nrow(data)/length(vars), 2), "\n\n")

filas <- data |>
  mutate(.row = row_number()) |>
  select(.row, sitio_temporada, fecha, das, biomasa)

grupos <- list(
  GDD  = 'gdd_cumsum',
  PP   = 'pp_cumsum',
  SM   = 'sm_mm',
  S1   = grep('^S1_', vars, value = TRUE),
  S2   = grep('^S2_', vars, value = TRUE),
  PS   = grep('^PS_', vars, value = TRUE)
)
W <- c(grupos$GDD, grupos$PP, grupos$SM)

conjuntos <- tribble(
  ~conjunto,      ~variables,
  'DAS',          'das',
  'GDD',          grupos$GDD,
  'W',            W,
  'S1',           grupos$S1,
  'S2',           grupos$S2,
  'PS',           grupos$PS,
  'S1+GDD',       c(grupos$S1, grupos$GDD),
  'S2+GDD',       c(grupos$S2, grupos$GDD),
  'PS+GDD',       c(grupos$PS, grupos$GDD),
  'S1+W',         c(grupos$S1, W),
  'S2+W',         c(grupos$S2, W),
  'PS+W',         c(grupos$PS, W),
  'S2+W+S1',      c(grupos$S2, W, grupos$S1),
  'All',          c(W, grupos$S1, grupos$S2, grupos$PS)
)

receta <- function(variables) {
  recipe(biomasa ~ ., data = select(data, all_of(variables), biomasa)) |>
    step_impute_knn(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_corr(all_numeric_predictors(), threshold = 0.9) |>  # Within-fold filtering
    step_zv(all_predictors())
}

loso_folds <- group_vfold_cv(data, group = 'sitio_temporada')

#2. Modelos con hiperparámetros fijos ----

rf_spec <- function(p) {
  rand_forest(trees = 1000, mtry = min(7, p), min_n = 5) |>
    set_engine('ranger', seed = 1603, importance = 'none') |>
    set_mode('regression')
}

xgb_spec <- function(p) {
  boost_tree(trees = 500, tree_depth = 4, learn_rate = 0.05, min_n = 5,
             sample_size = 0.8, mtry = min(7, p)) |>
    set_engine('xgboost', counts = TRUE, nthread = 1) |>
    set_mode('regression')
}

lm_spec <- function(p) linear_reg() |> set_engine('lm')

disenio <- conjuntos |>
  mutate(p = map_int(variables, length)) |>
  expand_grid(modelo = c('LM', 'RF', 'XGBoost')) |>
  filter((modelo == 'LM') == (conjunto %in% c('DAS', 'GDD')) | conjunto == 'W')

# Save to NEW file to preserve original results for comparison
archivo_res <- 'data/processed/modelos/rev2_loso_limpio_resultados_CORRECTED.rds'

if (file.exists(archivo_res)) {
  cat("Loading existing CORRECTED results from:", archivo_res, "\n")
  res <- read_rds(archivo_res)
} else {
  cat("Running CORRECTED analysis (this will take several hours)...\n")
  cat("Progress will be printed as each model completes.\n\n")

  res <- disenio |>
    mutate(pred = pmap(list(conjunto, variables, modelo, p), \(cj, v, m, p) {
      message(glue('[{Sys.time()}] {cj} | {m}'))
      dat <- select(data, all_of(v), biomasa, sitio_temporada)
      folds <- group_vfold_cv(dat, group = 'sitio_temporada')
      rec <- recipe(biomasa ~ ., data = dat) |>
        update_role(sitio_temporada, new_role = 'ID') |>
        step_impute_knn(all_numeric_predictors()) |>
        step_normalize(all_numeric_predictors()) |>
        step_corr(all_numeric_predictors(), threshold = 0.9) |>  # Within-fold!
        step_zv(all_predictors())
      spec <- switch(m, LM = lm_spec(p), RF = rf_spec(p), XGBoost = xgb_spec(p))
      set.seed(1603)
      workflow(rec, spec) |>
        fit_resamples(folds, control = control_resamples(save_pred = TRUE)) |>
        collect_predictions() |>
        select(.row, .pred)
    }))

  cat("\nSaving CORRECTED results to:", archivo_res, "\n")
  write_rds(res, archivo_res)
}

oof <- res |>
  select(conjunto, modelo, pred) |>
  unnest(pred) |>
  left_join(filas, by = '.row') |>
  mutate(id = paste(conjunto, modelo, sep = ' | '))

#3. Métricas por fold, resumen y diferencias pareadas contra la línea base ∑GDD ----

por_fold <- bind_rows(
  oof |> group_by(id, conjunto, modelo, sitio_temporada) |> metricas(biomasa, .pred) |>
    mutate(subconjunto = 'all'),
  # Excluding biomasa == 0 samples
  oof |> filter(biomasa > 0) |>
    group_by(id, conjunto, modelo, sitio_temporada) |> metricas(biomasa, .pred) |>
    mutate(subconjunto = 'biomasa_gt_0')
)

# Save updated results with CORRECTED suffix
write_rds(por_fold, 'data/processed/modelos/rev2_loso_limpio_por_fold_CORRECTED.rds')

resumen <- por_fold |>
  filter(subconjunto == 'all') |>
  group_by(conjunto, modelo) |>
  summarize(
    rsq_mean  = mean(rsq),
    rsq_sd    = sd(rsq),
    rmse_mean = mean(rmse),
    rmse_sd   = sd(rmse),
    mae_mean  = mean(mae),
    mae_sd    = sd(mae),
    .groups   = 'drop'
  )

write_csv(resumen, 'output/tables/rev2_loso_limpio_resumen_CORRECTED.csv')

# Comparison with GDD baseline
gdd_base <- por_fold |>
  filter(subconjunto == 'all', conjunto == 'GDD', modelo == 'LM') |>
  select(sitio_temporada, rmse_base = rmse)

comparacion <- por_fold |>
  filter(subconjunto == 'all') |>
  left_join(gdd_base, by = 'sitio_temporada') |>
  mutate(
    delta_rmse = (rmse - rmse_base) / rmse_base * 100,
    mejor_que_base = rmse < rmse_base
  ) |>
  group_by(conjunto, modelo) |>
  summarize(
    delta_rmse_mean = mean(delta_rmse),
    n_folds_mejor = sum(mejor_que_base),
    .groups = 'drop'
  )

resumen_final <- left_join(resumen, comparacion, by = c('conjunto', 'modelo'))

write_csv(resumen_final, 'output/tables/rev2_loso_limpio_resumen_con_comparacion_CORRECTED.csv')

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:\n")
cat("  -", archivo_res, "\n")
cat("  - output/tables/rev2_loso_limpio_por_fold_CORRECTED.rds\n")
cat("  - output/tables/rev2_loso_limpio_resumen_CORRECTED.csv\n")
cat("  - output/tables/rev2_loso_limpio_resumen_con_comparacion_CORRECTED.csv\n\n")

cat("Next steps:\n")
cat("  1. Compare CORRECTED vs original results\n")
cat("  2. Continue with rest of script 31 (importance, ALE, figures)\n")
cat("  3. Update manuscript Methods §2.3.2 (remove TODO at line 218)\n")
cat("  4. Update manuscript Results with CORRECTED values\n\n")

# Continue with the rest of the original script 31...
# (Sections 4-9: ablation, importance, ALE, figures, etc.)
# These sections can use the `oof` and `resumen` objects created above
