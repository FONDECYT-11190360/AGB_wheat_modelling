# Quick check: verify all corrected predictors exist in dataset

library(tidyverse)

# Core predictors
core_vars <- c("sitio", "fecha", "pp_cumsum", "sm_mm", "gdd_cumsum")

# Sentinel-1
s1_vars <- c("S1_VV", "S1_VH", "S1_VH_VV")

# Sentinel-2 - INCLUDING those filtered out by pre-CV correlation
s2_daily <- c(
  "S2_B1", "S2_B6",
  "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI", "S2_TCARI_OSAVI",
  "S2_MCARI_OSAVI2", "S2_TCARI_OSAVI2",
  "S2_SWIR11_MCARI", "S2_SWIR11_TCARI", "S2_SWIR12_MCARI",
  "S2_OSAVI", "S2_OSAVI2", "S2_SR",
  "S2_NDRE1", "S2_NDRE2", "S2_NDRE3", "S2_NDRE_NDVI",
  "S2_CVI", "S2_EVI", "S2_GNDVI", "S2_SIPI", "S2_WI1",
  "S2_CI_red"
)

s2_8a <- c(
  "S2_TCARI_OSAVI_8A", "S2_MCARI_OSAVI_8A",
  "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A"
)

s2_cumsum <- c(
  "S2_B1_cumsum",
  "S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum",
  "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum"
)

s2_vars <- c(s2_daily, s2_8a, s2_cumsum)

# PlanetScope
ps_daily <- c(
  "PS_B3", "PS_B8",
  "PS_SR", "PS_OSAVI", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI",
  "PS_CI_red"
)

ps_cumsum <- c(
  "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum"
)

ps_vars <- c(ps_daily, ps_cumsum)

# All vars
vars_corrected <- c(core_vars, s1_vars, s2_vars, ps_vars, "biomasa")

# Check
data <- read_rds('data/processed/rds/dataset.rds')
missing <- setdiff(vars_corrected, names(data))

cat("\n=== CORRECTED PREDICTOR LIST ===\n\n")
cat("Total predictors:", length(vars_corrected), "\n")
cat("Missing from dataset:", length(missing), "\n")

if (length(missing) > 0) {
  cat("\nMISSING PREDICTORS:\n")
  print(missing)
  stop("Some predictors are missing!")
} else {
  cat("\n✓ All predictors exist in dataset!\n\n")
  cat("Breakdown:\n")
  cat("  Core:", length(core_vars), "\n")
  cat("  S1:", length(s1_vars), "\n")
  cat("  S2:", length(s2_vars), "\n")
  cat("  PS:", length(ps_vars), "\n")
  cat("  Total (incl. biomasa):", length(vars_corrected), "\n\n")

  # Compare to original
  vars_original <- c("sitio", "fecha", "pp_cumsum", "sm_mm","gdd_cumsum","S2_B1", "S2_B6", "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI2", "S2_SWIR11_MCARI", "S2_SWIR11_TCARI","S2_SWIR12_MCARI", "S2_CVI", "S2_NDRE3", "S2_NDRE_NDVI", "S2_WI1", "S2_TCARI_OSAVI_8A", "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A", "S1_VV", "S1_VH", "S1_VH_VV", "PS_B3", "PS_B8", "PS_SR", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI", "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum", "S2_B1_cumsum","S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum", "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum", "PS_B5", "PS_TCARI_OSAVI", "PS_MCARI_OSAVI", "PS_NDRE_NDVI", "biomasa")

  added <- setdiff(vars_corrected, vars_original)
  removed <- setdiff(vars_original, vars_corrected)

  cat("CHANGES from original:\n")
  cat("  Added (restored from pre-CV filtering):", length(added), "\n")
  if (length(added) > 0) print(added)
  cat("\n  Removed (PS red edge, unavailable):", length(removed), "\n")
  if (length(removed) > 0) print(removed)
}
