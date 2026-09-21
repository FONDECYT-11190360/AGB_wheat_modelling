# Compare Original vs CORRECTED Results
#
# This script compares the results from:
# - Original analysis (with data leakage): 39 predictors
# - Corrected analysis (no leakage): 53 predictors
#
# Use this to assess impact of the data leakage fix for Peer Review Issue B1

library(tidyverse)
library(glue)

cat("\n=== COMPARING ORIGINAL VS CORRECTED RESULTS ===\n\n")

# Load results
original_file <- 'data/processed/modelos/rev2_loso_limpio_resultados.rds'
corrected_file <- 'data/processed/modelos/rev2_loso_limpio_resultados_CORRECTED.rds'

if (!file.exists(original_file)) {
  stop("Original results not found. Run script 31 first.")
}

if (!file.exists(corrected_file)) {
  stop("CORRECTED results not found. Run script 31_loso_diseno_limpio_CORRECTED.R first.")
}

cat("Loading original results...\n")
original <- read_rds(original_file)

cat("Loading corrected results...\n")
corrected <- read_rds(corrected_file)

# Load data for metrics
siembra <- read_rds('data/processed/rds/fechas_fenologia.rds') |>
  filter(fenologia == 'SOWING') |>
  select(sitio, temporada, fecha_siembra = fecha)

data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  left_join(siembra, by = c('sitio', 'temporada')) |>
  mutate(das = as.numeric(fecha - fecha_siembra),
         .row = row_number()) |>
  select(.row, sitio_temporada, fecha, das, biomasa)

# Compute metrics
metricas <- function(df, truth, estimate) {
  df |>
    summarize(rsq  = 1 - sum(({{truth}} - {{estimate}})^2) / sum(({{truth}} - mean({{truth}}))^2),
              rmse = sqrt(mean(({{truth}} - {{estimate}})^2)),
              mae  = mean(abs({{truth}} - {{estimate}})),
              n    = n(),
              .groups = 'drop')
}

# Original metrics
cat("\nComputing original metrics...\n")
orig_metrics <- original |>
  select(conjunto, modelo, pred) |>
  unnest(pred) |>
  left_join(data, by = '.row') |>
  group_by(conjunto, modelo, sitio_temporada) |>
  metricas(biomasa, .pred) |>
  group_by(conjunto, modelo) |>
  summarize(rsq_mean = mean(rsq), rsq_sd = sd(rsq),
            rmse_mean = mean(rmse), rmse_sd = sd(rmse),
            mae_mean = mean(mae), mae_sd = sd(mae),
            .groups = 'drop') |>
  mutate(version = 'Original (39 vars)')

# Corrected metrics
cat("Computing corrected metrics...\n")
corr_metrics <- corrected |>
  select(conjunto, modelo, pred) |>
  unnest(pred) |>
  left_join(data, by = '.row') |>
  group_by(conjunto, modelo, sitio_temporada) |>
  metricas(biomasa, .pred) |>
  group_by(conjunto, modelo) |>
  summarize(rsq_mean = mean(rsq), rsq_sd = sd(rsq),
            rmse_mean = mean(rmse), rmse_sd = sd(rmse),
            mae_mean = mean(mae), mae_sd = sd(mae),
            .groups = 'drop') |>
  mutate(version = 'Corrected (53 vars)')

# Combine and compute differences
comparison <- bind_rows(orig_metrics, corr_metrics) |>
  pivot_wider(names_from = version,
              values_from = c(rsq_mean, rsq_sd, rmse_mean, rmse_sd, mae_mean, mae_sd)) |>
  mutate(
    delta_rsq = `rsq_mean_Corrected (53 vars)` - `rsq_mean_Original (39 vars)`,
    delta_rmse = `rmse_mean_Corrected (53 vars)` - `rmse_mean_Original (39 vars)`,
    delta_rmse_pct = delta_rmse / `rmse_mean_Original (39 vars)` * 100
  )

cat("\n=== KEY COMPARISONS ===\n\n")

# GDD baseline
gdd_orig <- comparison |> filter(conjunto == 'GDD', modelo == 'LM')
cat("GDD Baseline (Linear Model):\n")
cat(sprintf("  Original: R²=%.3f, RMSE=%.2f t/ha\n",
            gdd_orig$`rsq_mean_Original (39 vars)`,
            gdd_orig$`rmse_mean_Original (39 vars)`))
cat(sprintf("  Corrected: R²=%.3f, RMSE=%.2f t/ha\n",
            gdd_orig$`rsq_mean_Corrected (53 vars)`,
            gdd_orig$`rmse_mean_Corrected (53 vars)`))
cat(sprintf("  Change: ΔR²=%.3f, ΔRMSE=%.2f t/ha (%.1f%%)\n\n",
            gdd_orig$delta_rsq, gdd_orig$delta_rmse, gdd_orig$delta_rmse_pct))

# Best model (S1+GDD RF in original)
best_orig <- comparison |> filter(conjunto == 'S1+GDD', modelo == 'RF')
cat("Best Model (S1+GDD with Random Forest):\n")
cat(sprintf("  Original: R²=%.3f, RMSE=%.2f t/ha\n",
            best_orig$`rsq_mean_Original (39 vars)`,
            best_orig$`rmse_mean_Original (39 vars)`))
cat(sprintf("  Corrected: R²=%.3f, RMSE=%.2f t/ha\n",
            best_orig$`rsq_mean_Corrected (53 vars)`,
            best_orig$`rmse_mean_Corrected (53 vars)`))
cat(sprintf("  Change: ΔR²=%.3f, ΔRMSE=%.2f t/ha (%.1f%%)\n\n",
            best_orig$delta_rsq, best_orig$delta_rmse, best_orig$delta_rmse_pct))

# All predictors
all_rf <- comparison |> filter(conjunto == 'All', modelo == 'RF')
cat("All Predictors (Random Forest):\n")
cat(sprintf("  Original: R²=%.3f, RMSE=%.2f t/ha\n",
            all_rf$`rsq_mean_Original (39 vars)`,
            all_rf$`rmse_mean_Original (39 vars)`))
cat(sprintf("  Corrected: R²=%.3f, RMSE=%.2f t/ha\n",
            all_rf$`rsq_mean_Corrected (53 vars)`,
            all_rf$`rmse_mean_Corrected (53 vars)`))
cat(sprintf("  Change: ΔR²=%.3f, ΔRMSE=%.2f t/ha (%.1f%%)\n\n",
            all_rf$delta_rsq, all_rf$delta_rmse, all_rf$delta_rmse_pct))

# Save full comparison
write_csv(comparison, 'output/tables/comparison_original_vs_corrected.csv')

cat("Full comparison saved to: output/tables/comparison_original_vs_corrected.csv\n\n")

# Summary statistics
cat("=== OVERALL IMPACT ===\n\n")
summary_stats <- comparison |>
  summarize(
    mean_abs_delta_rsq = mean(abs(delta_rsq), na.rm = TRUE),
    mean_abs_delta_rmse_pct = mean(abs(delta_rmse_pct), na.rm = TRUE),
    max_abs_delta_rmse_pct = max(abs(delta_rmse_pct), na.rm = TRUE),
    n_improved = sum(delta_rmse < 0, na.rm = TRUE),
    n_worse = sum(delta_rmse > 0, na.rm = TRUE),
    n_total = n()
  )

cat(sprintf("Mean absolute ΔR²: %.3f\n", summary_stats$mean_abs_delta_rsq))
cat(sprintf("Mean absolute ΔRMSE: %.1f%%\n", summary_stats$mean_abs_delta_rmse_pct))
cat(sprintf("Max absolute ΔRMSE: %.1f%%\n", summary_stats$max_abs_delta_rmse_pct))
cat(sprintf("Models improved: %d / %d\n", summary_stats$n_improved, summary_stats$n_total))
cat(sprintf("Models worse: %d / %d\n", summary_stats$n_worse, summary_stats$n_total))

cat("\n=== INTERPRETATION FOR REVIEWERS ===\n\n")

if (summary_stats$mean_abs_delta_rmse_pct < 5) {
  cat("RESULT: Results are VERY SIMILAR (mean ΔRMSE < 5%)\n")
  cat("INTERPRETATION: The data leakage had minimal impact.\n")
  cat("                Main conclusions are ROBUST.\n")
  cat("RESPONSE: 'Performance was nearly identical after correcting\n")
  cat("           the data leakage, confirming that our main\n")
  cat("           conclusions are robust to predictor selection.'\n")
} else if (summary_stats$mean_abs_delta_rmse_pct < 10) {
  cat("RESULT: Results show MODERATE changes (mean ΔRMSE 5-10%)\n")
  cat("INTERPRETATION: The data leakage had some impact but\n")
  cat("                main patterns likely hold.\n")
  cat("RESPONSE: Describe which results changed and whether\n")
  cat("          the core conclusion (thermal time dominates) holds.\n")
} else {
  cat("RESULT: Results show SUBSTANTIAL changes (mean ΔRMSE > 10%)\n")
  cat("INTERPRETATION: The data leakage had significant impact.\n")
  cat("                Conclusions may need revision.\n")
  cat("RESPONSE: Carefully describe how conclusions changed.\n")
}

cat("\n=== DONE ===\n")
