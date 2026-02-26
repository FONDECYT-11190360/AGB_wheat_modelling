library(car)
library(tidymodels)
library(tidyverse)

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figs",   showWarnings = FALSE, recursive = TRUE)

#0. Variables (identical to script 24) ----

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

#1. Cargar datos ----

data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  select(-muestra) |>
  select(all_of(vars), sitio_temporada, temporada)

message("Dataset: ", nrow(data), " obs × ", ncol(data), " columnas")

#2. Receta base SIN step_corr — para auditar correlaciones pre-remoción ----
# step_impute_knn y step_normalize se aplican para que la correlación y el VIF
# se calculen sobre los mismos datos procesados que usará el modelo.

rec_nocorr <- recipe(biomasa ~ ., data = data) |>
  update_role(sitio,           new_role = 'dont_use') |>
  update_role(fecha,           new_role = 'dont_use') |>
  update_role(sitio_temporada, new_role = 'ID') |>
  update_role(temporada,       new_role = 'ID') |>
  step_impute_knn(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

prepped_nocorr  <- prep(rec_nocorr, training = data)
baked_nocorr    <- bake(prepped_nocorr, new_data = NULL,
                        all_predictors(), all_outcomes())

message("Predictores antes de step_corr: ", ncol(baked_nocorr) - 1)

#3. Matriz de correlación — pares con |r| >= 0.9 (umbral de step_corr) ----

cor_matrix <- baked_nocorr |>
  select(-biomasa) |>
  as.matrix() |>
  cor(use = "pairwise.complete.obs")

high_cor_pairs <- cor_matrix |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "r") |>
  filter(var1 < var2, abs(r) >= 0.9) |>
  arrange(desc(abs(r)))

message("Pares de variables con |r| >= 0.9: ", nrow(high_cor_pairs))
write_csv(high_cor_pairs, 'output/tables/pares_alta_correlacion.csv')

#4. Aplicar step_corr y auditar variables removidas ----

rec_with_corr <- rec_nocorr |>
  step_corr(all_numeric_predictors(), threshold = 0.9)

prepped_corr <- prep(rec_with_corr, training = data)

# tidy() extrae exactamente qué variables eliminó step_corr durante prep
# (el número del step depende del orden en la receta; step_corr es el paso 3)
corr_step_info <- tidy(prepped_corr) |>
  filter(str_detect(type, "corr"))

corr_step_number <- corr_step_info$number[1]

removed_by_corr <- tidy(prepped_corr, number = corr_step_number) |>
  filter(terms != "retained") |>
  rename(variable = terms) |>
  mutate(reason = "step_corr: |r| ≥ 0.9 con al menos un predictor retenido")

message("Variables removidas por step_corr: ", nrow(removed_by_corr))
if (nrow(removed_by_corr) > 0) {
  message("  ", paste(removed_by_corr$variable, collapse = ", "))
}

write_csv(removed_by_corr, 'output/tables/variables_removidas_step_corr.csv')

#5. Predictores retenidos tras step_corr ----

baked_after <- bake(prepped_corr, new_data = NULL,
                    all_predictors(), all_outcomes())

vars_before  <- names(baked_nocorr  |> select(-biomasa))
vars_after   <- names(baked_after   |> select(-biomasa))
vars_removed <- setdiff(vars_before, vars_after)

tibble(
  stage              = c("Before step_corr", "After step_corr"),
  n_predictors       = c(length(vars_before), length(vars_after)),
  n_removed_this_step = c(NA_integer_, length(vars_removed))
) |>
  write_csv('output/tables/corr_removal_summary.csv')

#6. VIF después de step_corr ----
# Se ajusta un modelo lineal sobre los predictores retenidos y se calcula VIF.
# Un VIF alto (> 10) indica multicolinealidad residual que step_corr no capturó
# (e.g., relaciones no lineales o correlaciones entre más de dos variables).

lm_after <- lm(biomasa ~ ., data = baked_after)

vif_vals <- tryCatch(
  car::vif(lm_after),
  error = function(e) {
    message("car::vif() falló: ", e$message)
    message("Intentando con subconjunto sin alias...")
    # Remover variables con coeficiente NA del modelo lineal
    coef_ok <- names(na.omit(coef(lm_after)))[-1]  # excluir intercept
    lm_clean <- lm(biomasa ~ ., data = baked_after |> select(all_of(c(coef_ok, "biomasa"))))
    car::vif(lm_clean)
  }
)

vif_df <- tibble(
  variable = names(vif_vals),
  VIF      = as.numeric(vif_vals),
  high_vif = VIF > 10,
  sensor   = case_when(
    str_starts(variable, "S2") ~ "Sentinel-2",
    str_starts(variable, "S1") ~ "Sentinel-1",
    str_starts(variable, "PS") ~ "PlanetScope",
    TRUE                       ~ "Climate/Soil"
  )
) |>
  arrange(desc(VIF))

message("\nVIF post step_corr:")
message("  Variables con VIF >  5: ", sum(vif_df$VIF >  5))
message("  Variables con VIF > 10: ", sum(vif_df$VIF > 10))

write_csv(vif_df, 'output/tables/vif_post_step_corr.csv')

#7. VIF iterativo: remoción complementaria de variables con VIF > 10 ----
# Documenta qué variables adicionales se eliminarían si se aplica el criterio VIF > 10
# como paso posterior a step_corr. No modifica el pipeline principal: sirve como
# referencia para el revisor del journal.

vif_iterativo <- function(df, threshold = 10) {
  removed  <- character(0)
  step_log <- tibble(step = integer(), variable = character(),
                     VIF = numeric())
  step <- 0L

  repeat {
    step    <- step + 1L
    lm_it   <- lm(biomasa ~ ., data = df)
    vif_it  <- tryCatch(car::vif(lm_it), error = function(e) NULL)

    if (is.null(vif_it)) {
      message("Iteración ", step, ": colinealidad perfecta — deteniendo")
      break
    }

    max_var <- names(which.max(vif_it))
    max_val <- max(vif_it)

    if (max_val <= threshold) {
      message("Convergencia en iteración ", step,
              ": VIF máximo = ", round(max_val, 1))
      break
    }

    step_log <- bind_rows(step_log,
                          tibble(step = step, variable = max_var, VIF = max_val))
    message("Iteración ", step, ": removiendo '", max_var,
            "' (VIF = ", round(max_val, 1), ")")
    removed <- c(removed, max_var)
    df      <- df |> select(-all_of(max_var))
  }

  list(log = step_log, retained = names(df |> select(-biomasa)), df = df)
}

vif_iter_result <- vif_iterativo(baked_after, threshold = 10)

write_csv(vif_iter_result$log, 'output/tables/vif_iterativo_removidas.csv')

message("\nVIF iterativo (VIF > 10):")
message("  Variables removidas:  ", nrow(vif_iter_result$log))
message("  Predictores retenidos: ", length(vif_iter_result$retained))

# Tabla resumen completa para el paper
vif_pipeline_summary <- tibble(
  stage = c(
    "Full predictor set (post-imputation/normalization)",
    "After step_corr (|r| ≥ 0.9)",
    "After iterative VIF > 10 removal"
  ),
  n_predictors = c(
    length(vars_before),
    length(vars_after),
    length(vif_iter_result$retained)
  ),
  n_removed = c(
    NA_integer_,
    length(vars_removed),
    nrow(vif_iter_result$log)
  )
)

write_csv(vif_pipeline_summary, 'output/tables/vif_pipeline_summary.csv')
message("\nResumen del pipeline de remoción de variables:")
print(vif_pipeline_summary)

#8. Figura: VIF de los predictores retenidos tras step_corr ----

vif_df |>
  slice_head(n = 30) |>
  ggplot(aes(VIF, fct_reorder(variable, VIF), fill = sensor)) +
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = 10, linetype = "dashed",
             color = "firebrick", linewidth = 0.8) +
  geom_vline(xintercept = 5, linetype = "dotted",
             color = "darkorange", linewidth = 0.6) +
  annotate("text", x = 10.3, y = 0.5, label = "VIF = 10",
           hjust = 0, color = "firebrick", size = 3) +
  annotate("text", x = 5.3,  y = 0.5, label = "VIF = 5",
           hjust = 0, color = "darkorange", size = 3) +
  scale_fill_brewer(palette = "Set2", name = "Sensor") +
  labs(
    x     = "Variance Inflation Factor (VIF)",
    y     = NULL,
    title = "VIF after step_corr (threshold |r| = 0.9)",
    caption = paste0("n = ", length(vars_after), " predictors retained; ",
                     length(vars_removed), " removed by step_corr")
  ) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        legend.position  = "top")

ggsave('output/figs/vif_post_step_corr.png',
       width = 7, height = 6, dpi = 300, scale = 1.2)
