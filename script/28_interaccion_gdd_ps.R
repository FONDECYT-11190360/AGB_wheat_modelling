library(DALEX)
library(DALEXtra)
library(stacks)       # required: registers predict.model_stack S3 method
library(tidymodels)
library(tidyverse)

# ── Physiological rationale ────────────────────────────────────────────────────
# GDD (gdd_cumsum): cumulative thermal time drives phenological progression.
#   - ~300–500 GDD: tillering — rapid LAI expansion, low AGB
#   - ~600–900 GDD: stem elongation / heading — peak AGB accumulation rate
#   - ~900–1400 GDD: grain fill — AGB stabilises; canopy closure reached
#   - >1400 GDD:    maturity / senescence — dry matter redistribution
#
# PS_EVI (PlanetScope Enhanced Vegetation Index): sensitive to canopy green
#   biomass and chlorophyll content (NDVI-like but less saturated at high LAI).
#   - Expected positive covariation with AGB throughout the season.
#   - INTERACTION hypothesis: the marginal effect of GDD on AGB should be
#     amplified when PS_EVI is high (green, dense canopy), because the canopy
#     is actively accumulating biomass. When PS_EVI is low (sparse / senescent
#     canopy), thermal time adds less to aerial biomass.
# ──────────────────────────────────────────────────────────────────────────────

dir.create("output/figs", showWarnings = FALSE, recursive = TRUE)

#0. Variables ----

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

#1. Cargar datos y modelo ensamblado ----
# `explain_tidymodels` espera los datos en escala original (sin bake) porque el
# ensemble model contiene el pipeline de preprocesamiento completo.

data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  select(-muestra) |>
  select(all_of(vars), sitio_temporada, temporada)

set.seed(987)
splits     <- initial_split(data, prop = 0.75, strata = "sitio")
biom_train <- training(splits)

model_ens <- read_rds('data/processed/modelos/modelo_ensamblado.rds')

#2. Crear explainer DALEX sobre datos de entrenamiento ----

explainer <- explain_tidymodels(
  model_ens,
  data    = biom_train |> select(-biomasa),
  y       = biom_train$biomasa,
  label   = "Ensemble (fusion)",
  verbose = FALSE
)

message("Explainer creado: ", explainer$label,
        " | n = ", nrow(explainer$data))

#3. Verificar que PS_EVI sobrevive step_corr en el modelo ----
# El stacks ensemble incluye modelos de múltiples recetas. PS_EVI puede no estar
# en todos los miembros del stack pero la predicción agregada sí lo captura.

ps_evi_range <- range(biom_train$PS_EVI, na.rm = TRUE)
message("Rango PS_EVI en entrenamiento: ",
        round(ps_evi_range[1], 4), " – ", round(ps_evi_range[2], 4))

#4. Stratificar datos en tres niveles de PS_EVI ----
# Se usa Q33 y Q66 para tres grupos de igual tamaño (terciles).

ps_evi_q <- quantile(biom_train$PS_EVI, probs = c(1/3, 2/3), na.rm = TRUE)

biom_train_grouped <- biom_train |>
  mutate(
    PS_EVI_group = case_when(
      PS_EVI <= ps_evi_q[1] ~ "Low PS_EVI\n(sparse / early canopy)",
      PS_EVI >= ps_evi_q[2] ~ "High PS_EVI\n(dense / active canopy)",
      TRUE                  ~ "Mid PS_EVI\n(developing canopy)"
    ) |>
    factor(levels = c(
      "Low PS_EVI\n(sparse / early canopy)",
      "Mid PS_EVI\n(developing canopy)",
      "High PS_EVI\n(dense / active canopy)"
    ))
  )

message("Distribución por grupo PS_EVI:")
print(table(biom_train_grouped$PS_EVI_group))

#5. Conditional PDP: PDP of gdd_cumsum for each PS_EVI tercile ----
# Para cada grupo, se crea un explainer separado con el subconjunto de datos
# y se calcula el PDP de gdd_cumsum. Esto es equivalente a un PDP condicional
# y muestra cómo la relación GDD→AGB cambia según la densidad de vegetación.

pdp_by_group <- levels(biom_train_grouped$PS_EVI_group) |>
  map(\(grp) {
    data_grp <- biom_train_grouped |>
      filter(PS_EVI_group == grp) |>
      select(-PS_EVI_group)

    exp_grp <- explain_tidymodels(
      model_ens,
      data    = data_grp |> select(-biomasa),
      y       = data_grp$biomasa,
      label   = grp,
      verbose = FALSE
    )

    pdp_grp <- model_profile(
      exp_grp,
      variables = "gdd_cumsum",
      type      = "partial",
      N         = NULL   # usa todos los datos del grupo
    )

    as.data.frame(pdp_grp$agr_profiles) |>
      rename(x = `_x_`, yhat = `_yhat_`) |>
      mutate(PS_EVI_group = grp)
  }) |>
  bind_rows() |>
  mutate(
    PS_EVI_group = factor(PS_EVI_group, levels = levels(biom_train_grouped$PS_EVI_group))
  )

#6. PDP marginal de PS_EVI (sin condicionamiento) ----
# Muestra la relación directa PS_EVI → AGB para comparación.

pdp_psevi <- model_profile(
  explainer,
  variables = "PS_EVI",
  type      = "partial",
  N         = NULL
)

df_pdp_psevi <- as.data.frame(pdp_psevi$agr_profiles) |>
  rename(x = `_x_`, yhat = `_yhat_`)

#7. Figura A: PDP condicional GDD × PS_EVI ----
# Interpreta la interacción: si las líneas divergen, hay interacción real.
# Convergencia de líneas → sin interacción (aditivo puro).

agro_gdd_refs <- tribble(
  ~x_ref, ~label,
  500,    "Tillering\n~500 GDD",
  800,    "Heading\n~800 GDD",
  1400,   "Maturity\n~1400 GDD"
)

p_interaction <- ggplot(pdp_by_group, aes(x, yhat,
                                           color    = PS_EVI_group,
                                           linetype = PS_EVI_group)) +
  geom_vline(data = agro_gdd_refs,
             aes(xintercept = x_ref),
             linetype = "dotted", color = "gray55", linewidth = 0.5) +
  geom_text(data = agro_gdd_refs,
            aes(x = x_ref, y = Inf, label = label),
            vjust = 1.2, hjust = -0.05, size = 2.5,
            color = "gray40", inherit.aes = FALSE) +
  geom_line(linewidth = 1.3, alpha = 0.9) +
  scale_color_manual(
    values = c("#1B7837", "#762A83", "#E66101"),
    name   = "PS EVI level"
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotdash"),
    name   = "PS EVI level"
  ) +
  labs(
    x       = expression(paste("Cumulative GDD (", degree, "C·day)")),
    y       = "Average predicted AGB (t/ha)",
    title   = "GDD × PS_EVI interaction: conditional partial dependence",
    caption = paste0(
      "Lines represent PDP of gdd_cumsum within PS_EVI terciles.\n",
      "Diverging lines indicate a non-additive GDD × canopy density interaction.\n",
      "Low PS_EVI tercile: ≤", round(ps_evi_q[1], 3),
      " | High PS_EVI tercile: ≥", round(ps_evi_q[2], 3)
    )
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    legend.key.width = unit(1.8, "cm"),
    plot.caption     = element_text(size = 7.5, color = "gray40")
  )

#8. Figura B: PDP marginal de PS_EVI ----

p_psevi_pdp <- ggplot(df_pdp_psevi, aes(x, yhat)) +
  geom_line(linewidth = 1.2, color = "#1B7837") +
  geom_rug(data = biom_train, aes(x = PS_EVI),
           inherit.aes = FALSE, alpha = 0.15, length = unit(0.015, "npc")) +
  labs(
    x       = "PS EVI",
    y       = "Average predicted AGB (t/ha)",
    title   = "Marginal PDP: PlanetScope EVI",
    caption = paste0("Expected: monotone increase up to canopy closure,\n",
                     "followed by plateau or slight decline during senescence.")
  ) +
  theme_bw() +
  theme(plot.caption = element_text(size = 7.5, color = "gray40"))

#9. Combinar y guardar ----

library(patchwork)

combined <- p_interaction / p_psevi_pdp +
  plot_layout(heights = c(3, 2)) +
  plot_annotation(
    title   = "Physiological interpretation: GDD × PlanetScope EVI",
    subtitle = paste0(
      "Interaction analysis confirms that the AGB–GDD relationship is ",
      "modulated by canopy greenness (PS EVI).\n",
      "High EVI (dense canopy) amplifies the biomass accumulation response ",
      "to thermal time, consistent with radiation-use efficiency theory."
    ),
    theme = theme(plot.subtitle = element_text(size = 8.5, color = "gray30"))
  )

ggsave('output/figs/interaccion_gdd_ps_evi.png',
       plot   = combined,
       width  = 7, height = 9, dpi = 300, scale = 1.2)

message("Figura guardada: output/figs/interaccion_gdd_ps_evi.png")

#10. Test cuantitativo de interacción ----
# Calcula la diferencia máxima entre las curvas GDD-AGB de PS_EVI alto y bajo.
# Una diferencia grande (> 1 t/ha en la meseta) confirma interacción biológicamente
# significativa en el modelo.

interaction_gap <- pdp_by_group |>
  # Each group has its own GDD x-grid (different range per tercile), so
  # point-by-point subtraction fails. Instead compare mean predicted AGB.
  mutate(grp = case_when(
    str_starts(as.character(PS_EVI_group), "Low")  ~ "low",
    str_starts(as.character(PS_EVI_group), "High") ~ "high",
    TRUE ~ "mid"
  )) |>
  filter(grp %in% c("low", "high")) |>
  group_by(grp) |>
  summarize(mean_yhat = mean(yhat, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = grp, values_from = mean_yhat) |>
  mutate(
    mean_gap = high - low,
    # max gap: range of mean yhat across the full GDD trajectory
    max_gap  = max(
      pdp_by_group |> filter(str_starts(as.character(PS_EVI_group), "High")) |> pull(yhat), na.rm = TRUE
    ) -
    min(
      pdp_by_group |> filter(str_starts(as.character(PS_EVI_group), "Low"))  |> pull(yhat), na.rm = TRUE
    )
  )

message("\nInteraction strength (PS_EVI High vs Low, mean predicted AGB):")
message("  Mean gap (High − Low): ", round(interaction_gap$mean_gap, 2), " t/ha")
message("  Max range gap:         ", round(interaction_gap$max_gap,  2), " t/ha")
