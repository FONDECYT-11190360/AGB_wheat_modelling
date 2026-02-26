library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

#0. Variables a seleccionar ----

vars <- c("sitio", "fecha", "pp_cumsum", "sm_mm", "gdd_cumsum",
          "S2_B1", "S2_B6", "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI2",
          "S2_SWIR11_MCARI", "S2_SWIR11_TCARI", "S2_SWIR12_MCARI", "S2_CVI",
          "S2_NDRE3", "S2_NDRE_NDVI", "S2_WI1", "S2_TCARI_OSAVI_8A",
          "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A",
          "S1_VV", "S1_VH", "S1_VH_VV",
          "PS_B3", "PS_B8", "PS_SR", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI",
          "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum",
          "S2_B1_cumsum", "S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum",
          "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum",
          "PS_B5", "PS_TCARI_OSAVI", "PS_MCARI_OSAVI", "PS_NDRE_NDVI", "biomasa")

#1. Leer los datos ----
# Se agrega sitio_temporada y temporada como columnas de identificación (no predictores)

data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  select(-muestra) |>
  select(all_of(vars), sitio_temporada, temporada)

#2. Recetas de preprocesamiento — diseño de ablación ----
# Los pasos step_impute_knn, step_normalize y step_corr se estiman
# ÚNICAMENTE sobre el fold de entrenamiento (sin filtración al fold de validación).
# sitio, fecha, sitio_temporada y temporada reciben roles no-predictor.

rec_base <- recipe(biomasa ~ ., data = data) |>
  update_role(sitio,           new_role = 'dont_use') |>
  update_role(fecha,           new_role = 'dont_use') |>
  update_role(sitio_temporada, new_role = 'ID') |>
  update_role(temporada,       new_role = 'ID') |>
  step_impute_knn(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

rec_clima    <- rec_base |> step_rm(starts_with(c('S1', 'S2', 'PS')))  # (a) Climate only
rec_baseline <- rec_base |> step_rm(starts_with(c('S1', 'PS')))        # (b) S2 + Climate
rec_radar    <- rec_base |> step_rm(starts_with('PS'))                  # (c) S2 + S1 + Climate
rec_fusion   <- rec_base                                                 # (d) S2 + S1 + PS + Climate

#3. Especificaciones de modelos (hiperparámetros fijos — no tuneados en LOSO) ----
# Parámetros tomados como valores razonables del dominio, no extraídos del tuneo
# de script 14 (para evitar filtración de datos entre folds).

rf_spec <- rand_forest(trees = 1000, mtry = 7, min_n = 5) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

xgb_spec <- boost_tree(
  trees = 500, tree_depth = 4, learn_rate = 0.05,
  sample_size = 0.8, mtry = 7, min_n = 5) |>
  set_engine("xgboost") |>
  set_mode("regression")

glm_spec <- linear_reg(penalty = 0.01, mixture = 0.5) |>
  set_engine("glmnet") |>
  set_mode("regression")

#4. Workflow set: 3 recetas × 3 modelos = 9 workflows ----

wfs <- workflow_set(
  preproc = list(
    clima    = rec_clima,     # (a) Climate only
    baseline = rec_baseline,  # (b) S2 + Climate
    radar    = rec_radar,     # (c) S2 + S1 + Climate
    fusion   = rec_fusion     # (d) S2 + S1 + PS + Climate
  ),
  models = list(RF = rf_spec, XGBoost = xgb_spec, GLM = glm_spec)
)

#5. Folds LOSO: Leave-One-Site-Out (4 folds = 4 sitio-temporadas) ----

loso_folds <- group_vfold_cv(data, group = "sitio_temporada")

fold_labels <- loso_folds |>
  mutate(sitio_test = map_chr(splits, ~ unique(assessment(.x)$sitio_temporada)))

#6. Ajustar modelos con validación cruzada LOSO ----

loso_res <- wfs |>
  workflow_map(
    "fit_resamples",
    resamples = loso_folds,
    metrics   = metric_set(rsq, rmse, mae),
    control   = control_resamples(save_pred = TRUE, verbose = TRUE)
  )

write_rds(loso_res, 'data/processed/modelos/loso_results.rds')

#7. Métricas por fold ----
# Debe tener 108 filas = 9 workflows × 4 folds × 3 métricas

metrics_per_fold <- collect_metrics(loso_res, summarize = FALSE)

write_rds(metrics_per_fold, 'data/processed/modelos/loso_metrics_per_fold.rds')

message("Filas en metrics_per_fold: ", nrow(metrics_per_fold),
        " (esperado: 144 = 12 workflows × 4 folds × 3 métricas)")

#8. Predicciones out-of-fold ----
# .row es el índice de fila en el dataset original (data), usando 1-based index

oof_preds <- collect_predictions(loso_res) |>
  left_join(
    data |>
      mutate(row_id = row_number()) |>
      select(row_id, sitio, fecha, temporada, sitio_temporada),
    by = c(".row" = "row_id")
  )

write_rds(oof_preds, 'data/processed/modelos/loso_oof_predictions.rds')

#9. Tabla resumen de ablación ----

ablation_summary <- metrics_per_fold |>
  separate(wflow_id, into = c("recipe", "model"), sep = "_") |>
  group_by(recipe, model, .metric) |>
  summarize(mean = mean(.estimate), sd = sd(.estimate), .groups = "drop") |>
  pivot_wider(names_from = .metric, values_from = c(mean, sd))

message("Filas en ablation_summary: ", nrow(ablation_summary),
        " (esperado: 12 = 4 recetas × 3 modelos)")

# Cuantificar aporte marginal de cada sensor (XGBoost, usando RMSE)
rmse_clima    <- ablation_summary |> filter(recipe == "clima",    model == "XGBoost") |> pull(mean_rmse)
rmse_baseline <- ablation_summary |> filter(recipe == "baseline", model == "XGBoost") |> pull(mean_rmse)
rmse_radar    <- ablation_summary |> filter(recipe == "radar",    model == "XGBoost") |> pull(mean_rmse)
rmse_fusion   <- ablation_summary |> filter(recipe == "fusion",   model == "XGBoost") |> pull(mean_rmse)

s2_improvement_pct <- (rmse_clima  - rmse_baseline) / rmse_clima  * 100
s1_improvement_pct <- (rmse_baseline - rmse_radar)  / rmse_baseline * 100
ps_improvement_pct <- (rmse_radar  - rmse_fusion)   / rmse_radar   * 100

message("Aporte marginal de S2 (baseline vs clima, XGBoost):    ",
        round(s2_improvement_pct, 1), "% mejora RMSE")
message("Aporte marginal de S1 (radar vs baseline, XGBoost):    ",
        round(s1_improvement_pct, 1), "% mejora RMSE")
message("Aporte marginal de PS (fusion vs radar, XGBoost):      ",
        round(ps_improvement_pct, 1), "% mejora RMSE")

if (ps_improvement_pct < 5) {
  message("CRITICAL FINDING: PlanetScope mejora RMSE en solo ",
          round(ps_improvement_pct, 1), "% — revisar necesidad en pipeline")
}

# Tabla resumen de mejoras marginales por sensor
# Nota: se usan nombres sin ambigüedad para evitar data-masking en tibble()
sensor_gains <- tibble(
  sensor           = c("S2 (vs Climate-only)", "S1 (vs S2+Climate)", "PS (vs S2+S1+Climate)"),
  rmse_without     = c(rmse_clima, rmse_baseline, rmse_radar),
  rmse_with        = c(rmse_baseline, rmse_radar, rmse_fusion),
  improvement_pct  = c(s2_improvement_pct, s1_improvement_pct, ps_improvement_pct)
)
write_csv(sensor_gains, 'output/tables/sensor_marginal_gains.csv')

write_csv(ablation_summary, 'output/tables/ablation_loso_summary.csv')

#10. Figuras ----

## Figura 1: Dot-plot con barras de error — métricas promedio por receta y modelo

metrics_per_fold |>
  separate(wflow_id, into = c("recipe", "model"), sep = "_") |>
  group_by(recipe, model, .metric) |>
  summarize(mean_val = mean(.estimate), sd_val = sd(.estimate), .groups = "drop") |>
  mutate(
    recipe = factor(recipe,
                    levels = c("clima", "baseline", "radar", "fusion"),
                    labels = c("(a) Climate", "(b) S2+Climate",
                               "(c) S2+S1+Climate", "(d) Fusion (all)"))
  ) |>
  ggplot(aes(recipe, mean_val, color = model, group = model)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymax = mean_val + sd_val, ymin = mean_val - sd_val),
                width = 0.2, position = position_dodge(width = 0.4)) +
  scale_color_brewer(palette = "Set2", name = "Modelo") +
  labs(y = "Valor", x = "Combinación de sensores") +
  facet_grid(.metric ~ ., scales = "free_y",
             labeller = labeller(.metric = c(mae = "MAE", rmse = "RMSE", rsq = "R²"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave('output/figs/loso_ablation_metrics.png',
       width = 6, height = 5, dpi = 300, scale = 1.2)

## Figura 2: Mapa de calor RMSE por sitio-temporada (XGBoost — mejor modelo por defecto)

metrics_per_fold |>
  filter(str_detect(wflow_id, "XGBoost"), .metric == "rmse") |>
  separate(wflow_id, into = c("recipe", "model"), sep = "_") |>
  left_join(fold_labels |> select(id, sitio_test), by = "id") |>
  mutate(
    recipe = factor(recipe,
                    levels = c("clima", "baseline", "radar", "fusion"),
                    labels = c("(a) Climate", "(b) S2+Climate",
                               "(c) S2+S1+Climate", "(d) Fusion"))
  ) |>
  ggplot(aes(recipe, sitio_test, fill = .estimate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(.estimate, 1)), color = "white", size = 3.5) +
  scale_fill_viridis_c(direction = -1, name = "RMSE\n(t/ha)") +
  labs(x = "Combinación de sensores",
       y = "Sitio-Temporada (fold de validación)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave('output/figs/loso_metricas_por_sitio.png',
       width = 6, height = 4, dpi = 300, scale = 1.2)

#11. Diagnóstico La Cancha vs otros sitios ----
# Objective: diagnose why the model underperforms at La Cancha by comparing
# per-site RMSE/R² across all ablation recipes and models.

la_cancha_metrics <- metrics_per_fold |>
  left_join(fold_labels |> select(id, sitio_test), by = "id") |>
  separate(wflow_id, into = c("recipe", "model"), sep = "_") |>
  mutate(
    site_group = if_else(str_detect(sitio_test, "la_cancha"), "La Cancha", "Other sites"),
    recipe     = factor(recipe,
                        levels = c("clima", "baseline", "radar", "fusion"),
                        labels = c("(a) Climate", "(b) S2+Climate",
                                   "(c) S2+S1+Climate", "(d) Fusion"))
  ) |>
  group_by(recipe, model, .metric, site_group) |>
  summarize(
    mean_estimate = mean(.estimate),
    sd_estimate   = sd(.estimate),
    .groups       = "drop"
  )

write_csv(la_cancha_metrics, 'output/tables/la_cancha_diagnostico.csv')

message("\nRMSE La Cancha vs otros sitios (fusion_XGBoost):")
la_cancha_metrics |>
  filter(model == "XGBoost", .metric == "rmse", recipe == "(d) Fusion") |>
  select(site_group, mean_estimate, sd_estimate) |>
  print()

# Figure: RMSE per site-season for fusion recipe (all models) — La Cancha highlighted
metrics_per_fold |>
  filter(str_starts(wflow_id, "fusion"), .metric == "rmse") |>
  separate(wflow_id, into = c("recipe", "model"), sep = "_") |>
  left_join(fold_labels |> select(id, sitio_test), by = "id") |>
  mutate(is_la_cancha = str_detect(sitio_test, "la_cancha"),
         sitio_label  = str_replace_all(sitio_test, "_", "\n")) |>
  ggplot(aes(model, .estimate, fill = is_la_cancha)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("FALSE" = "#4393C3", "TRUE" = "#D73027"),
                    labels  = c("Other sites", "La Cancha"),
                    name    = NULL) +
  facet_wrap(~ sitio_label, ncol = 2) +
  labs(x = "Model", y = "RMSE (t/ha)",
       title = "LOSO RMSE by site — Fusion recipe") +
  theme_bw() +
  theme(strip.background  = element_rect(fill = "white"),
        legend.position   = "top",
        axis.text.x       = element_text(angle = 20, hjust = 1))

ggsave('output/figs/la_cancha_vs_otros_rmse.png',
       width = 6, height = 5, dpi = 300, scale = 1.2)

# Additional diagnostic: residuals for La Cancha vs others (fusion_XGBoost)
oof_preds |>
  filter(wflow_id == "fusion_XGBoost") |>
  mutate(
    residual     = .pred - biomasa,
    site_group   = if_else(str_detect(sitio_temporada, "la_cancha"), "La Cancha", "Other sites")
  ) |>
  ggplot(aes(biomasa, .pred, color = site_group)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(alpha = 0.65, size = 1.8) +
  scale_color_manual(values = c("La Cancha" = "#D73027", "Other sites" = "#4393C3"),
                     name   = NULL) +
  labs(x = "Observed AGB (t/ha)", y = "Predicted AGB (t/ha)",
       title = "LOSO OOF: observed vs predicted (fusion_XGBoost)") +
  theme_bw() +
  theme(legend.position = "top")

ggsave('output/figs/la_cancha_obs_vs_pred.png',
       width = 5, height = 5, dpi = 300, scale = 1.2)
