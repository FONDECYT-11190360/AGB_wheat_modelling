# Script 02-05 (Sentinel-2): rehace los predictores Sentinel-2 tras corregir el desplazamiento de la
# línea base de procesamiento 04.00 (2026-10-07).
# Ejecuta los bloques Sentinel-2 de los scripts 02, 03 y 04 sin cambios, y la extracción del script 05
# solo para Sentinel-2. Los rásteres de PlanetScope y Sentinel-1 ya no están en el repositorio, así que
# las columnas S2_ de vi_extract.rds se reemplazan y las columnas PS_ y S1_ se conservan.

bloque <- function(archivo, hasta) {
  l <- readLines(archivo)
  fin <- grep(hasta, l)[1] - 1
  parse(text = l[1:fin])
}

# 02: índices por escena (corrige BOA_ADD_OFFSET desde el 25 de enero de 2022)
if (Sys.getenv('SKIP_02') != '1') eval(bloque('script/02_calcular_vis.R', '^#planet'), envir = new.env())

# 03: suavizado de Kalman y relleno diario
# clusterExport() del script 03 busca las funciones en el entorno global
eval(bloque('script/03_suavizar_vis.R', '^#planetscope'), envir = globalenv())

# 04: sumas acumuladas desde la siembra
eval(bloque('script/04_acumular_vis.R', '^#planetscope'), envir = new.env())

# 05: extracción en los puntos de muestreo (solo Sentinel-2)
l05 <- readLines('script/05_extraer_vis.R')
fin05 <- grep("write_rds\\('data/processed/rds/vi_extract.rds'\\)", l05) - 1
ini_write <- max(grep('^data \\|>', l05[1:fin05]))
env05 <- new.env()
eval(parse(text = l05[1:(ini_write - 1)]), envir = env05)

library(tidyverse)
claves <- c('sitio', 'temporada', 'fecha', 'muestra')
s2_nuevo <- bind_rows(env05$data) |> as_tibble()
anterior <- read_rds('data/processed/rds/vi_extract_s2off.rds')

stopifnot(setequal(grep('^S2_', names(anterior), value = TRUE), grep('^S2_', names(s2_nuevo), value = TRUE)))

vi_extract <- anterior |>
  select(-starts_with('S2_')) |>
  full_join(s2_nuevo, by = claves) |>
  select(all_of(names(anterior))) |>
  arrange(temporada, sitio, fecha, muestra)

write_rds(vi_extract, 'data/processed/rds/vi_extract.rds')

# 07: bloque de índices satelitales (vi.rds)
l07 <- readLines('script/07_procesar_datos.R')
eval(parse(text = l07[grep('^#indices satelitales', l07):length(l07)]), envir = new.env())
