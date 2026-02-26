library(tidymodels)
library(tidyverse)
library(glue)

dir.create("output/figs",   showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# ── Approach ───────────────────────────────────────────────────────────────────
# Prediction uncertainty is estimated from the spread of LOSO out-of-fold (OOF)
# predictions across the three models in the fusion recipe (RF, XGBoost, GLM).
# For each observation, the ensemble mean and inter-model standard deviation are
# used to construct a 95% confidence interval: mean ± 1.96 × SD.
#
# This is an *epistemic* uncertainty estimate: it reflects model disagreement
# rather than aleatoric noise. It is interpretable as: "how much would our AGB
# estimate change if we chose a different algorithm?"
#
# Empirical coverage (% of observations where true biomasa falls within the CI)
# quantifies calibration. A well-calibrated set of models should approach 95%.
# ──────────────────────────────────────────────────────────────────────────────

#1. Cargar predicciones OOF — filtrar a receta fusion ----

oof_all <- read_rds('data/processed/modelos/loso_oof_predictions.rds')

fusion_wflows <- oof_all |>
  filter(str_starts(wflow_id, "fusion")) |>
  pull(wflow_id) |>
  unique()

message("Workflows fusion encontrados: ", paste(fusion_wflows, collapse = ", "))

oof_fusion <- oof_all |>
  filter(wflow_id %in% fusion_wflows) |>
  select(.row, wflow_id, .pred, biomasa, sitio, fecha, temporada, sitio_temporada)

#2. Pivotear a wide: una columna de predicción por modelo ----
# .row es el índice de fila en el dataset original (globalmente único en LOSO).

oof_wide <- oof_fusion |>
  # En workflow_map con fit_resamples sin tune, cada (.row, wflow_id) es único.
  # Usamos distinct() como salvaguarda.
  distinct(.row, wflow_id, .keep_all = TRUE) |>
  pivot_wider(
    id_cols   = c(.row, biomasa, sitio, fecha, temporada, sitio_temporada),
    names_from  = wflow_id,
    values_from = .pred
  )

model_cols <- fusion_wflows
n_models   <- length(model_cols)
message("Columnas de modelos para CI: ", paste(model_cols, collapse = ", "))

#3. Calcular IC 95% inter-modelos ----

oof_ci <- oof_wide |>
  rowwise() |>
  mutate(
    pred_mean  = mean(c_across(all_of(model_cols)), na.rm = TRUE),
    pred_sd    = sd(c_across(all_of(model_cols)),   na.rm = TRUE),
    ci_lower   = pred_mean - 1.96 * pred_sd,
    ci_upper   = pred_mean + 1.96 * pred_sd,
    ci_width   = ci_upper - ci_lower,
    residual   = pred_mean - biomasa,
    covered    = (biomasa >= ci_lower) & (biomasa <= ci_upper)
  ) |>
  ungroup()

#4. Cobertura empírica global ----

coverage_global <- mean(oof_ci$covered, na.rm = TRUE) * 100
mean_ci_width   <- mean(oof_ci$ci_width, na.rm = TRUE)

message("\n── IC 95% inter-modelos ──────────────────────────────")
message("  Cobertura empírica global:     ", round(coverage_global, 1), "%")
message("  Anchura media del IC:          ", round(mean_ci_width,   2), " t/ha")
message("  (IC nominal = 95% | modelos = ", n_models, ")")

#5. Cobertura y métricas por sitio ----

coverage_by_site <- oof_ci |>
  group_by(sitio) |>
  summarize(
    n              = n(),
    coverage_pct   = mean(covered, na.rm = TRUE) * 100,
    mean_ci_width  = mean(ci_width, na.rm = TRUE),
    rmse_ensemble  = sqrt(mean(residual^2, na.rm = TRUE)),
    mean_bias      = mean(residual, na.rm = TRUE),
    .groups        = "drop"
  )

write_csv(coverage_by_site, 'output/tables/ci_coverage_by_site.csv')

message("\nCobertura IC 95% por sitio:")
print(coverage_by_site)

#6. Calcular Días Después de Siembra (DAS) ----

feno      <- read_rds('data/processed/rds/fechas_fenologia.rds')
sos_dates <- feno |>
  filter(fenologia == "SOWING") |>
  select(sitio, temporada, fecha_sos = fecha)

stage_das_mean <- feno |>
  left_join(sos_dates, by = c("sitio", "temporada")) |>
  mutate(DAS_stage = as.integer(fecha - fecha_sos)) |>
  filter(fenologia != "SOWING") |>
  group_by(fenologia) |>
  summarize(mean_DAS = mean(DAS_stage, na.rm = TRUE), .groups = "drop") |>
  arrange(mean_DAS)

oof_ci <- oof_ci |>
  left_join(sos_dates, by = c("sitio", "temporada")) |>
  mutate(DAS = as.integer(fecha - fecha_sos))

message("\nRango DAS: ", min(oof_ci$DAS, na.rm = TRUE),
        " – ", max(oof_ci$DAS, na.rm = TRUE))

#7. Figura A: predicciones + IC 95% + observaciones vs DAS (por sitio) ----

site_labels <- oof_ci |>
  distinct(sitio) |>
  mutate(label = str_replace_all(sitio, "_", " ") |> str_to_title())

oof_ci_labelled <- oof_ci |>
  left_join(site_labels, by = "sitio")

p_ci_das <- ggplot(
  oof_ci_labelled |> arrange(sitio, DAS),
  aes(DAS, color = label, fill = label)
) +
  geom_vline(data        = stage_das_mean,
             aes(xintercept = mean_DAS),
             linetype = "dotted", color = "gray60", linewidth = 0.5) +
  geom_text(data         = stage_das_mean,
            aes(x = mean_DAS, y = Inf, label = fenologia),
            angle        = 90, vjust = -0.3, hjust = 1.1, size = 2.2,
            color        = "gray45", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              alpha = 0.20, color = NA) +
  geom_line(aes(y = pred_mean), linewidth = 0.9, alpha = 0.85) +
  geom_point(aes(y = biomasa), shape = 16, size = 1.5, alpha = 0.75) +
  scale_color_brewer(palette = "Set1", name = "Site") +
  scale_fill_brewer(palette  = "Set1", name = "Site") +
  labs(
    x       = "Days after sowing (DAS)",
    y       = "AGB (t/ha)",
    title   = glue("LOSO predictions with 95% CI  (empirical coverage: {round(coverage_global,1)}%)"),
    caption = paste0(
      "Line = ensemble mean (RF + XGBoost + GLM); ribbon = mean ± 1.96 × inter-model SD.\n",
      "Points = observed AGB. Dashed lines = mean phenological stage transitions."
    )
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    plot.caption     = element_text(size = 7.5, color = "gray40"),
    strip.background = element_rect(fill = "white")
  )

ggsave('output/figs/ci_predicciones_DAS.png',
       plot = p_ci_das,
       width = 8, height = 5, dpi = 300, scale = 1.2)

#8. Figura B: anchura del IC vs DAS (incertidumbre temporal) ----
# Permite detectar en qué fase del ciclo el modelo es más incierto y si
# la incertidumbre está relacionada con alguna etapa fenológica específica.

p_ci_width <- oof_ci_labelled |>
  ggplot(aes(DAS, ci_width, color = label)) +
  geom_vline(data        = stage_das_mean,
             aes(xintercept = mean_DAS),
             linetype = "dotted", color = "gray60", linewidth = 0.5) +
  geom_text(data         = stage_das_mean,
            aes(x = mean_DAS, y = Inf, label = fenologia),
            angle        = 90, vjust = -0.3, hjust = 1.1, size = 2.2,
            color        = "gray45", inherit.aes = FALSE) +
  geom_point(alpha = 0.50, size = 1.4) +
  geom_smooth(method = "loess", formula = y ~ x,
              aes(group = 1),
              color = "black", linewidth = 1, se = TRUE, alpha = 0.12) +
  scale_color_brewer(palette = "Set1", name = "Site") +
  labs(
    x       = "Days after sowing (DAS)",
    y       = "CI width (t/ha)",
    title   = "Model uncertainty over the growing season",
    caption = paste0(
      "CI width = 1.96 × 2 × inter-model SD. Increasing width during ",
      "grain-fill stages may indicate\n",
      "algorithm-specific responses to rapid canopy changes."
    )
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    plot.caption     = element_text(size = 7.5, color = "gray40")
  )

ggsave('output/figs/ci_anchura_vs_DAS.png',
       plot = p_ci_width,
       width = 7, height = 4, dpi = 300, scale = 1.2)

message("Figuras guardadas: output/figs/ci_predicciones_DAS.png, ci_anchura_vs_DAS.png")

#9. Tabla de resumen para el paper ----

ci_summary <- tibble(
  metric                  = c("Models in ensemble",
                               "Uncertainty type",
                               "CI formula",
                               "Empirical coverage (global)",
                               "Mean CI width (global)",
                               "Site with highest uncertainty",
                               "Site with lowest uncertainty"),
  value = c(
    paste(fusion_wflows, collapse = " + "),
    "Epistemic (inter-model disagreement)",
    "mean ± 1.96 × SD across fusion models",
    paste0(round(coverage_global, 1), "%"),
    paste0(round(mean_ci_width, 2), " t/ha"),
    coverage_by_site |> slice_max(mean_ci_width, n = 1) |>
      mutate(v = glue("{sitio} ({round(mean_ci_width,2)} t/ha)")) |> pull(v),
    coverage_by_site |> slice_min(mean_ci_width, n = 1) |>
      mutate(v = glue("{sitio} ({round(mean_ci_width,2)} t/ha)")) |> pull(v)
  )
)

write_csv(ci_summary, 'output/tables/ci_summary.csv')
message("\nResumen del IC para el paper guardado en output/tables/ci_summary.csv")
