#!/usr/bin/env bash
# Rehace los predictores Sentinel-2 (corrección BOA_ADD_OFFSET) y reajusta los modelos.
# Uso: XGB17_LIB=<carpeta con solo xgboost 1.7.11.1> setsid nohup bash script/run_s2_chain.sh > output/s2_chain.log 2>&1 &
set -u
cd "$(dirname "$0")/.."
: "${XGB17_LIB:?Definir XGB17_LIB con una librería que contenga solo xgboost 1.7.11.1}"

paso() { echo "[$(date '+%F %H:%M:%S')] $*"; }

paso "inicio: Sentinel-2 (scripts 03 a 05 y 07)"
SKIP_02=1 Rscript script/02_05_rehacer_s2.R > output/02_05_run_s2.log 2>&1 || { paso "ERROR en 02_05 (ver output/02_05_run_s2.log)"; exit 1; }

paso "script 08: dataset.rds"
Rscript script/08_armar_dataset.R > output/08_run_s2.log 2>&1   # el gráfico exploratorio final puede fallar después de escribir dataset.rds

paso "script 31: reajuste LOSSO-CV (sin caché)"
rm -f data/processed/modelos/rev2_loso_limpio_resultados.rds \
      data/processed/modelos/rev2_vip_heldout_rf_all.rds \
      data/processed/modelos/rev2_ale_rf_all.rds
Rscript script/31_loso_diseno_limpio.R > output/31_run_s2.log 2>&1 || { paso "ERROR en 31 (ver output/31_run_s2.log)"; exit 1; }

paso "figura 7, script 35 y figura de fenología"
Rscript script/UPDATE_FIG7_LABELS.R > output/fig7_run_s2.log 2>&1
Rscript script/35_gdd_rf_xgb.R > output/35_run_s2.log 2>&1 || { paso "ERROR en 35 (ver output/35_run_s2.log)"; exit 1; }
Rscript script/fig_graficar_fenologia_biomasa.R > output/fen_run_s2.log 2>&1

paso "listo"
