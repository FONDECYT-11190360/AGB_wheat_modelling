# Script 07.1: recalcula clima.rds (∑GDD y ∑PP) tras corregir Tmin en el script 06 (2026-10-07)
# Ejecuta solo el bloque "#clima" del script 07. Las demás entradas de 07 (data/processed_old/
# data_fenologia.rds, data_biomasa_estructuras.rds, data_soil.rds) ya no están en el repositorio,
# así que las fechas fenológicas se leen de su salida procesada (fechas_fenologia.rds).
library(tidyverse)

fechas_feno <- read_rds('data/processed/rds/fechas_fenologia.rds')

lineas_07 <- readLines('script/07_procesar_datos.R')
ini <- grep('^#clima', lineas_07)
fin <- grep('^#humedad de suelo', lineas_07) - 1
eval(parse(text = lineas_07[ini:fin]))
