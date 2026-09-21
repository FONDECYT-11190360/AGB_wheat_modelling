# Script 33: Extract step_corr() behavior per LOSO-CV fold
# Purpose: Document which predictors were retained/removed in each fold
# for Supplementary Table (Revision R3)
#
# Author: Francisco Zambrano
# Date: 2026-09-18

library(tidymodels)
library(tidyverse)
library(glue)

# Load corrected LOSO results ----
cat("Loading LOSO-CV results...\n")
results <- readRDS("data/processed/modelos/rev2_loso_limpio_resultados_CORRECTED.rds")

# Get workflow set
wf_set <- results$workflow_set

# Get the folds information
folds <- results$resamples
fold_labels <- c(
  "Hidango 2021-2022",
  "Hidango 2022-2023",
  "La Cancha 2022-2023",
  "Villa Baviera 2020-2021"
)

# Extract predictor info for each fold ----
cat("\nExtracting predictors retained per fold...\n")
cat("Note: Using 'all_predictors_RF' configuration as representative example\n\n")

# Function to extract retained predictors from fitted workflow
extract_fold_predictors <- function(wf_id, fold_num) {
  tryCatch({
    # Get the fitted workflow for this configuration and fold
    fit_obj <- wf_set %>%
      filter(wflow_id == wf_id) %>%
      pull(result) %>%
      pluck(1)

    # Extract the specific fold fit
    fold_fit <- fit_obj$.extracts[[fold_num]]$.extracts[[1]]

    # Get prepped recipe
    prepped <- extract_recipe(fold_fit) %>% prep()

    # Get retained predictors
    retained <- prepped$term_info %>%
      filter(role == "predictor") %>%
      pull(variable)

    # Get removed predictors (those with role != "predictor" or removed by step_corr)
    # Note: step_corr marks removed variables
    removed <- prepped$steps %>%
      keep(~inherits(.x, "step_corr")) %>%
      pluck(1, "removals", .default = character(0))

    list(
      retained = retained,
      removed = removed,
      n_retained = length(retained),
      n_removed = length(removed)
    )
  }, error = function(e) {
    warning(glue("Could not extract for {wf_id}, fold {fold_num}: {e$message}"))
    list(retained = NA, removed = NA, n_retained = NA, n_removed = NA)
  })
}

# Extract for all 4 folds using a representative configuration
config <- "all_predictors_RF"  # Adjust if needed
total_input <- 54  # Total predictors before step_corr

fold_summary <- map_dfr(1:4, function(i) {
  info <- extract_fold_predictors(config, i)

  tibble(
    Fold = i,
    Site_Season = fold_labels[i],
    Total_Predictors_Input = total_input,
    Predictors_After_step_corr = info$n_retained,
    Predictors_Removed = info$n_removed,
    Removal_Rate_Percent = round(100 * info$n_removed / total_input, 1)
  )
})

# Display summary
cat("\n=== Per-Fold Predictor Summary ===\n")
print(fold_summary)

# Save summary table
write_csv(fold_summary, "output/tables/supplementary_table_step_corr.csv")
cat("\nSummary table saved to: output/tables/supplementary_table_step_corr.csv\n")

# Analyze which predictors were removed most frequently ----
cat("\n=== Analyzing frequently removed predictors ===\n")

all_removed <- map(1:4, ~extract_fold_predictors(config, .x)$removed) %>%
  set_names(fold_labels)

# Count removal frequency
removal_freq <- all_removed %>%
  unlist() %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  rename(Predictor = 1, Removal_Count = Freq) %>%
  mutate(
    Removal_Frequency = paste0(Removal_Count, "/4 folds"),
    Sensor_Group = case_when(
      str_detect(Predictor, "^S2_") ~ "Sentinel-2",
      str_detect(Predictor, "^PS_") ~ "PlanetScope",
      str_detect(Predictor, "^S1_|^VV|^VH") ~ "Sentinel-1",
      TRUE ~ "Weather"
    )
  ) %>%
  arrange(desc(Removal_Count))

cat("\nPredictors removed in multiple folds (r > 0.9 correlation):\n")
print(removal_freq %>% filter(Removal_Count > 1))

# Save detailed removal table
write_csv(removal_freq, "output/tables/supplementary_predictors_removed_frequency.csv")
cat("\nDetailed removal frequency saved to: output/tables/supplementary_predictors_removed_frequency.csv\n")

# Summary statistics for Methods section ----
cat("\n=== Summary for Methods Section ===\n")
cat(glue("
Range of predictors retained after step_corr(): {min(fold_summary$Predictors_After_step_corr, na.rm=TRUE)}–{max(fold_summary$Predictors_After_step_corr, na.rm=TRUE)} predictors
Mean predictors retained: {round(mean(fold_summary$Predictors_After_step_corr, na.rm=TRUE), 1)} (SD = {round(sd(fold_summary$Predictors_After_step_corr, na.rm=TRUE), 1)})
Mean removal rate: {round(mean(fold_summary$Removal_Rate_Percent, na.rm=TRUE), 1)}%

Most frequently removed predictor groups:
{removal_freq %>% filter(Removal_Count >= 3) %>% pull(Sensor_Group) %>% table() %>% names() %>% paste(collapse=", ")}
"))

cat("\n=== Script completed successfully ===\n")
