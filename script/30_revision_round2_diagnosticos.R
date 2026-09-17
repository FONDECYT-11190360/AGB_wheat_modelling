# Script 30: diagnósticos del pipeline original para la revisión Round 2 (2026-09-17)
# Responde a output/revision/2026-09-17_manuscript_review_round2.md
#   C2  R² tradicional (1 - SSE/SST) además de rsq (correlación al cuadrado)
#   C3  ranking de los 40 workflows por RSQ y por RMSE, métricas por fold
#   C4  ablación del script 24 con los tres algoritmos y R² tradicional
#   C5  diagnóstico del ensamble apilado (miembro degenerado, desempeño LOSO)
#   M1  disponibilidad de los predictores red edge de PlanetScope por sitio
#   M3  métricas sin los puntos AGB = 0 a la siembra
#   Recetas del script 14: variables que sobreviven step_corr en cada fold
#   Stage 2  R² tradicional y selección por RMSE
# El análisis limpio usado en el manuscrito revisado está en el script 31.

# Los modelos guardados (workflow_estimacion.rds, modelo_ensamblado.rds) se ajustaron con xgboost 1.7.x.
# xgboost >= 2 no puede cargarlos; si es necesario, instalar xgboost 1.7.11.1 en una librería aparte
# y definir la variable de entorno XGB17_LIB con su ruta antes de correr este script.
if (nzchar(Sys.getenv('XGB17_LIB'))) .libPaths(c(Sys.getenv('XGB17_LIB'), .libPaths()))

library(tidymodels)
library(tidyverse)
library(stacks)
library(baguette)
library(glue)
library(glmnet)

tidymodels_prefer()
dir.create('output/tables', showWarnings = FALSE)

metricas <- function(df, truth, estimate) {
  df |>
    summarize(rsq      = rsq_vec({{ truth }}, {{ estimate }}),
              rsq_trad = rsq_trad_vec({{ truth }}, {{ estimate }}),
              rmse     = rmse_vec({{ truth }}, {{ estimate }}),
              mae      = mae_vec({{ truth }}, {{ estimate }}),
              n        = n(),
              .groups  = 'drop')
}

resumir_folds <- function(df, ...) {
  df |>
    pivot_longer(c(rsq, rsq_trad, rmse, mae), names_to = '.metric', values_to = '.estimate') |>
    group_by(..., .metric) |>
    summarize(mean = mean(.estimate, na.rm = TRUE), sd = sd(.estimate, na.rm = TRUE), .groups = 'drop')
}

#1. Datos ----

vars <- c("sitio", "fecha", "pp_cumsum", "sm_mm","gdd_cumsum","S2_B1", "S2_B6", "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI2", "S2_SWIR11_MCARI", "S2_SWIR11_TCARI","S2_SWIR12_MCARI", "S2_CVI", "S2_NDRE3", "S2_NDRE_NDVI", "S2_WI1", "S2_TCARI_OSAVI_8A", "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A", "S1_VV", "S1_VH", "S1_VH_VV", "PS_B3", "PS_B8", "PS_SR", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI", "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum", "S2_B1_cumsum","S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum", "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum", "PS_B5", "PS_TCARI_OSAVI", "PS_MCARI_OSAVI", "PS_NDRE_NDVI", "biomasa")

ps_rededge <- c("PS_B5", "PS_TCARI_OSAVI", "PS_MCARI_OSAVI", "PS_NDRE_NDVI")

data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  select(-muestra) |>
  select(all_of(vars), sitio_temporada, temporada)

filas <- data |>
  mutate(.row = row_number()) |>
  select(.row, sitio_temporada, biomasa)

#2. Disponibilidad de predictores y resumen de AGB por sitio (M1, M5) ----

na_por_sitio <- data |>
  group_by(sitio_temporada) |>
  summarize(across(c(all_of(ps_rededge), PS_B3, PS_B8, sm_mm, S1_VH_VV),
                   ~ mean(is.na(.x))),
            n = n(), agb_media = mean(biomasa), agb_max = max(biomasa),
            n_agb_cero = sum(biomasa == 0))

write_csv(na_por_sitio, 'output/tables/rev2_na_predictores_por_sitio.csv')

#3. Métricas de los 40 workflows desde las predicciones out-of-fold (C2, C3) ----

biom_res <- read_rds('data/processed/modelos/workflow_estimacion.rds')

oof <- collect_predictions(biom_res, summarize = FALSE) |>
  left_join(select(filas, .row, sitio_temporada), by = '.row')

por_fold <- oof |>
  group_by(wflow_id, .config, sitio_temporada) |>
  metricas(biomasa, .pred)

por_config <- resumir_folds(por_fold, wflow_id, .config)

mejor_por <- function(metrica, sentido = c('max', 'min')) {
  sentido <- match.arg(sentido)
  por_config |>
    filter(.metric == metrica) |>
    group_by(wflow_id) |>
    slice(if (sentido == 'max') which.max(mean) else which.min(mean)) |>
    ungroup() |>
    select(wflow_id, .config)
}

tabla_ranking <- function(sel) {
  sel |>
    left_join(por_config, by = c('wflow_id', '.config')) |>
    pivot_wider(names_from = .metric, values_from = c(mean, sd)) |>
    separate(wflow_id, into = c('rec', 'modelo'), sep = '_', remove = FALSE) |>
    arrange(mean_rmse) |>
    mutate(rank_rmse = row_number()) |>
    arrange(desc(mean_rsq)) |>
    mutate(rank_rsq = row_number()) |>
    arrange(rank_rmse)
}

rank_sel_rsq  <- tabla_ranking(mejor_por('rsq', 'max'))   # criterio original del manuscrito
rank_sel_rmse <- tabla_ranking(mejor_por('rmse', 'min'))

write_csv(rank_sel_rsq,  'output/tables/rev2_ranking_40_modelos_sel_rsq.csv')
write_csv(rank_sel_rmse, 'output/tables/rev2_ranking_40_modelos_sel_rmse.csv')

rank_sel_rsq |>
  select(wflow_id, .config) |>
  left_join(por_fold, by = c('wflow_id', '.config')) |>
  write_csv('output/tables/rev2_metricas_por_fold_40_modelos_sel_rsq.csv')

rank_sel_rsq |>
  select(wflow_id, .config) |>
  left_join(oof, by = c('wflow_id', '.config')) |>
  filter(biomasa > 0) |>
  group_by(wflow_id, sitio_temporada) |>
  metricas(biomasa, .pred) |>
  resumir_folds(wflow_id) |>
  pivot_wider(names_from = .metric, values_from = c(mean, sd)) |>
  arrange(mean_rmse) |>
  write_csv('output/tables/rev2_metricas_sin_agb_cero_40_modelos.csv')

#4. Variables que sobreviven step_corr en cada fold de las recetas del script 14 ----
# step_corr se estima sobre los 43 candidatos antes de step_rm, por lo que el conjunto
# efectivo de cada receta cambia entre folds.

loso_folds <- group_vfold_cv(data, group = 'sitio_temporada')

vars_efectivas <- expand_grid(rec = paste0('rec', 1:8), split = seq_along(loso_folds$splits)) |>
  mutate(sitio_temporada = map_chr(split, \(i) unique(assessment(loso_folds$splits[[i]])$sitio_temporada)),
         variables = map2(rec, split, \(r, i) {
           extract_preprocessor(biom_res, paste0(r, '_RF')) |>
             prep(training = analysis(loso_folds$splits[[i]])) |>
             bake(new_data = NULL) |>
             select(-any_of(c('sitio', 'fecha', 'sitio_temporada', 'temporada', 'biomasa'))) |>
             names()
         }),
         n_predictores = map_int(variables, length),
         gdd_presente = map_lgl(variables, \(v) 'gdd_cumsum' %in% v),
         variables = map_chr(variables, paste, collapse = ';'))

write_csv(select(vars_efectivas, -split), 'output/tables/rev2_script14_variables_efectivas_por_fold.csv')

#5. Ablación del script 24 con los tres algoritmos y R² tradicional (C4) ----

abl <- read_rds('data/processed/modelos/loso_results.rds')

collect_predictions(abl, summarize = FALSE) |>
  left_join(select(filas, .row, sitio_temporada), by = '.row') |>
  group_by(wflow_id, sitio_temporada) |>
  metricas(biomasa, .pred) |>
  resumir_folds(wflow_id) |>
  pivot_wider(names_from = .metric, values_from = c(mean, sd)) |>
  separate(wflow_id, into = c('receta', 'modelo'), sep = '_') |>
  write_csv('output/tables/rev2_ablacion_tres_algoritmos.csv')

#6. Diagnóstico del ensamble apilado (C5) ----

model_ensemble <- read_rds('data/processed/modelos/modelo_ensamblado.rds')

pesos <- model_ensemble$coefs |> tidy() |> filter(estimate != 0)

pred_miembros <- predict(model_ensemble, data, members = TRUE)

diag_miembros <- pesos |>
  filter(term != '(Intercept)') |>
  mutate(
    rmse_en_muestra = map_dbl(term, \(m) rmse_vec(data$biomasa, pred_miembros[[m]])),
    sd_pred_en_muestra = map_dbl(term, \(m) sd(pred_miembros[[m]])),
    sd_pred_oof_dentro_fold = map_dbl(term, \(m) {
      oof |>
        filter(paste0(wflow_id, .config) == m) |>
        group_by(sitio_temporada) |>
        summarize(s = sd(.pred)) |>
        pull(s) |>
        mean()
    })) |>
  bind_rows(filter(pesos, term == '(Intercept)'))

write_csv(diag_miembros, 'output/tables/rev2_ensamble_miembros.csv')

# Desempeño LOSO con pesos del meta modelo estimados sin el sitio retenido
candidatos <- oof |>
  mutate(miembro = paste0(wflow_id, .config)) |>
  select(.row, miembro, .pred) |>
  pivot_wider(names_from = miembro, values_from = .pred) |>
  arrange(.row) |>
  left_join(filas, by = '.row')

X <- as.matrix(select(candidatos, -c(.row, sitio_temporada, biomasa)))
y <- candidatos$biomasa

ens_oof <- map_dfr(unique(candidatos$sitio_temporada), \(st) {
  train <- candidatos$sitio_temporada != st
  fit <- glmnet(X[train, ], y[train], alpha = 1, lambda = model_ensemble$penalty$penalty,
                lower.limits = 0)
  tibble(sitio_temporada = st, biomasa = y[!train],
         .pred = as.numeric(predict(fit, X[!train, ])))
})

bind_rows(
  ens_oof |> group_by(sitio_temporada) |> metricas(biomasa, .pred) |>
    mutate(tipo = 'LOSO, pesos del meta modelo sin el sitio retenido'),
  filas |> mutate(.pred = pred_miembros$.pred) |> group_by(sitio_temporada) |>
    metricas(biomasa, .pred) |> mutate(tipo = 'ensamble final, en la muestra')) |>
  write_csv('output/tables/rev2_ensamble_desempeno.csv')

#7. Stage 2: R² tradicional y selección por RSQ y RMSE ----

stage2 <- map_dfr(1:4, \(lead) {
  read_rds(glue('data/processed/modelos/loso_oof_predictions_prediccion_lead_{lead}_meses.rds')) |>
    group_by(wflow_id, .config, sitio) |>
    metricas(cosecha, .pred) |>
    mutate(lead = lead)
})

stage2_resumen <- stage2 |>
  resumir_folds(lead, wflow_id, .config) |>
  pivot_wider(names_from = .metric, values_from = c(mean, sd))

bind_rows(
  stage2_resumen |> group_by(lead, wflow_id) |> slice_max(mean_rsq, n = 1, with_ties = FALSE) |>
    mutate(criterio = 'rsq'),
  stage2_resumen |> group_by(lead, wflow_id) |> slice_min(mean_rmse, n = 1, with_ties = FALSE) |>
    mutate(criterio = 'rmse')) |>
  ungroup() |>
  arrange(criterio, wflow_id, lead) |>
  write_csv('output/tables/rev2_stage2_metricas.csv')
