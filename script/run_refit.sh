#!/usr/bin/env bash
# Reajusta los modelos LOSSO-CV (script 31 sin caché), la figura 7 y el script 35.
# Uso: XGB17_LIB=~/R_libs_xgb17 setsid nohup bash script/run_refit.sh > output/refit.log 2>&1 &
set -u
cd "$(dirname "$0")/.."
: "${XGB17_LIB:?Definir XGB17_LIB con una librería que contenga solo xgboost 1.7.11.1}"
paso() { echo "[$(date '+%F %H:%M:%S')] $*"; }
paso "script 31 (sin caché)"
rm -f data/processed/modelos/rev2_loso_limpio_resultados.rds \
      data/processed/modelos/rev2_vip_heldout_rf_all.rds \
      data/processed/modelos/rev2_ale_rf_all.rds
Rscript script/31_loso_diseno_limpio.R > output/31_run.log 2>&1 || { paso "ERROR en 31 (ver output/31_run.log)"; exit 1; }
paso "figura 7 y script 35"
Rscript script/UPDATE_FIG7_LABELS.R > output/fig7_run.log 2>&1
Rscript script/35_gdd_rf_xgb.R > output/35_run.log 2>&1 || { paso "ERROR en 35 (ver output/35_run.log)"; exit 1; }
paso "listo"
