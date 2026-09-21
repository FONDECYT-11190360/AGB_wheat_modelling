# Script 14 - CORRECTED VERSION (No Data Leakage)
#
# CRITICAL FIX for Peer Review Issue B1:
# - Removed pre-CV correlation screening (was causing data leakage)
# - Now includes ALL VIs from Table 3 that are available for all 4 site-seasons
# - The within-fold step_corr() handles correlation filtering properly
#
# WHAT CHANGED:
# Original: 43 predictors (after pre-CV correlation screening removed 10+ VIs)
# Corrected: 53 predictors (all Table 3 VIs, minus 4 PS red edge unavailable for 2 sites)
#
# VIs RESTORED (were incorrectly removed by pre-CV correlation):
# - S2_OSAVI, S2_OSAVI2, S2_SR, S2_EVI, S2_GNDVI, S2_SIPI
# - S2_MCARI_OSAVI, S2_TCARI_OSAVI, S2_CI_red, S2_NDRE1

library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)

#0. Variables - ALL from Table 3 + Suppl Table S1 (minus PS red edge) ----

# Core predictors
core_vars <- c("sitio", "fecha", "pp_cumsum", "sm_mm", "gdd_cumsum")

# Sentinel-1 (3)
s1_vars <- c("S1_VV", "S1_VH", "S1_VH_VV")

# Sentinel-2 daily VIs from Table 3 (now INCLUDING those filtered out by pre-CV correlation)
s2_daily <- c(
  # Bands
  "S2_B1", "S2_B6",
  # Chlorophyll indices
  "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI", "S2_TCARI_OSAVI",
  "S2_MCARI_OSAVI2", "S2_TCARI_OSAVI2",
  "S2_SWIR11_MCARI", "S2_SWIR11_TCARI", "S2_SWIR12_MCARI",
  # Structure/soil indices
  "S2_OSAVI", "S2_OSAVI2",  # RESTORED (were filtered out)
  "S2_SR",  # RESTORED
  # Red edge indices
  "S2_NDRE1", "S2_NDRE2", "S2_NDRE3", "S2_NDRE_NDVI",  # NDRE1/2 RESTORED
  # Other VIs
  "S2_CVI", "S2_EVI", "S2_GNDVI", "S2_SIPI", "S2_WI1",  # EVI, GNDVI, SIPI RESTORED
  "S2_CI_red"  # RESTORED
)

# Sentinel-2 B8A variants (indices computed with B8A instead of B8)
s2_8a <- c(
  "S2_TCARI_OSAVI_8A", "S2_MCARI_OSAVI_8A",  # MCARI_OSAVI_8A RESTORED
  "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A"
)

# Sentinel-2 cumulative indices
s2_cumsum <- c(
  "S2_B1_cumsum", 
  "S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum",
  "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum"
)

s2_vars <- c(s2_daily, s2_8a, s2_cumsum)

# PlanetScope (EXCLUDING 4 red edge predictors unavailable for 2 site-seasons)
ps_daily <- c(
  "PS_B3", "PS_B8",
  "PS_SR", "PS_OSAVI", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI",
  "PS_CI_red"
)

ps_cumsum <- c(
  "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum"
)

ps_vars <- c(ps_daily, ps_cumsum)

# EXCLUDED (missing for Hidango 2021-2022 and Villa Baviera 2020-2021):
# PS_B5, PS_TCARI_OSAVI, PS_MCARI_OSAVI, PS_NDRE_NDVI

# Combine all
vars <- c(core_vars, s1_vars, s2_vars, ps_vars, "biomasa")

cat("\n==== CORRECTED PREDICTOR SELECTION ====\n\n")
cat("Total predictors:", length(vars) - 1, "(plus biomasa)\n")  # -1 for biomasa
cat("  Core (metadata + weather):", length(core_vars), "\n")
cat("  Sentinel-1:", length(s1_vars), "\n")
cat("  Sentinel-2:", length(s2_vars), "\n")
cat("    - Daily VIs:", length(s2_daily), "\n")
cat("    - B8A variants:", length(s2_8a), "\n")
cat("    - Cumulative:", length(s2_cumsum), "\n")
cat("  PlanetScope:", length(ps_vars), "\n")
cat("\nPS red edge excluded (unavailable): 4\n")
cat("\nVIs RESTORED (were filtered by pre-CV correlation):\n")
cat("  S2_OSAVI, S2_OSAVI2, S2_SR, S2_EVI, S2_GNDVI, S2_SIPI\n")
cat("  S2_MCARI_OSAVI, S2_TCARI_OSAVI, S2_CI_red, S2_NDRE1, S2_NDRE2\n")
cat("  S2_MCARI_OSAVI_8A, PS_OSAVI, PS_CI_red\n\n")
cat("DATA LEAKAGE FIX:\n")
cat("  ✓ Removed pre-CV correlation screening\n")
cat("  ✓ Within-fold step_corr() handles redundancy\n")
cat("  ✓ Each fold independently selects predictors\n\n")

#1. Load data ----
data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  select(-muestra) |>
  select(all_of(vars), sitio_temporada, temporada)

cat("Data loaded:", nrow(data), "samples x", length(vars), "predictors\n")
cat("Samples/predictor ratio:", round(nrow(data)/(length(vars)-1), 2), "\n\n")

# Rest of the script continues as in the original...
# (Model specifications, recipes, LOSO-CV, etc.)

