library(tidyverse)

#1. Cargar predicciones out-of-fold del modelo fusion_XGBoost ----

oof_preds <- read_rds('data/processed/modelos/loso_oof_predictions.rds') |>
  filter(wflow_id == "fusion_XGBoost")

#2. Cargar fechas de fenología ----

feno <- read_rds('data/processed/rds/fechas_fenologia.rds')

sos_dates <- feno |>
  filter(fenologia == "SOWING") |>
  select(sitio, temporada, fecha_sos = fecha)

stage_dates <- feno |>
  select(sitio, temporada, fenologia, fecha_stage = fecha)

#3. Calcular Días Después de Siembra (DAS) y residuales ----

analysis <- oof_preds |>
  left_join(sos_dates, by = c("sitio", "temporada")) |>
  mutate(
    DAS      = as.integer(fecha - fecha_sos),
    residual = .pred - biomasa
  )

#4. Asignar etapa fenológica vigente en cada fecha de observación ----
# Para cada obs., se busca la transición fenológica más reciente ocurrida
# en fecha_stage <= fecha de observación.

current_stage <- analysis |>
  select(sitio, temporada, fecha) |>
  distinct() |>
  left_join(stage_dates,
            by = c("sitio", "temporada"),
            relationship = "many-to-many") |>
  filter(fecha_stage <= fecha) |>
  slice_max(fecha_stage,
            by = c("sitio", "temporada", "fecha"),
            n = 1, with_ties = FALSE) |>
  select(sitio, temporada, fecha, fenologia_actual = fenologia)

analysis <- analysis |>
  left_join(current_stage, by = c("sitio", "temporada", "fecha"))

# Verificación
message("Rango de DAS: ", min(analysis$DAS, na.rm = TRUE),
        " – ", max(analysis$DAS, na.rm = TRUE))
message("Etapas fenológicas presentes: ",
        paste(sort(unique(analysis$fenologia_actual)), collapse = ", "))

#5. DAS medio por transición fenológica (para líneas de referencia verticales) ----

stage_das_mean <- feno |>
  left_join(sos_dates, by = c("sitio", "temporada")) |>
  mutate(DAS_stage = as.integer(fecha - fecha_sos)) |>
  filter(fenologia != "SOWING") |>
  group_by(fenologia) |>
  summarize(mean_DAS = mean(DAS_stage, na.rm = TRUE), .groups = "drop") |>
  arrange(mean_DAS)

#6. Gráfico residuales vs DAS con anotaciones fenológicas ----

ggplot(analysis, aes(DAS, residual, color = fenologia_actual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_vline(data  = stage_das_mean,
             aes(xintercept = mean_DAS),
             linetype = "dotted", color = "gray60", linewidth = 0.6,
             inherit.aes = FALSE) +
  geom_text(data = stage_das_mean,
            aes(x = mean_DAS, y = Inf, label = fenologia),
            angle = 90, vjust = -0.3, hjust = 1.1, size = 2.4,
            color = "gray40", inherit.aes = FALSE) +
  geom_point(alpha = 0.55, size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x,
              aes(group = 1),
              color = "black", linewidth = 1, se = TRUE, alpha = 0.15) +
  scale_color_brewer(palette = "Set1", name = "Etapa fenológica",
                     na.value = "gray70") +
  labs(
    x     = "Días después de siembra (DAS)",
    y     = "Residual: predicho − observado (t/ha)",
    title = "Error del modelo LOSO por fenología (fusion_XGBoost)"
  ) +
  theme_bw() +
  theme(legend.position      = "bottom",
        legend.title.align   = 0.5,
        strip.background     = element_rect(fill = "white"))

ggsave('output/figs/residuales_vs_DAS.png',
       width = 8, height = 5, dpi = 300, scale = 1.2)
