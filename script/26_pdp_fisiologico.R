library(DALEX)
library(DALEXtra)
library(stacks)
library(tidymodels)
library(tidyverse)

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
# Misma semilla y split que script 14 para coherencia con el explainer

data <- read_rds('data/processed/rds/dataset.rds') |>
  select(-muestra, -temporada, fecha) |>
  select(all_of(vars))

set.seed(987)
splits     <- initial_split(data)
biom_train <- training(splits)

model_ens <- read_rds('data/processed/modelos/modelo_ensamblado.rds')

#2. Crear explainer DALEX ----

explainer <- explain_tidymodels(
  model_ens,
  data    = biom_train,
  y       = biom_train$biomasa,
  label   = "Ensamblado",
  verbose = FALSE
)

#3. Calcular perfiles de dependencia parcial (top 4 predictores) ----
# Orden según importancia de variables (script 14.1): gdd_cumsum, S1_VH, S1_VV, S1_VH_VV
#
# Consistencia fisiológica esperada:
#   gdd_cumsum : tendencia monotónica creciente hasta ~800 GDD (encañado/heading),
#                seguida de meseta o ligera caída en madurez (~1400 GDD)
#   S1_VH      : incremento con biomasa aérea (dispersión volumétrica del dosel cerrado)
#   S1_VV      : respuesta a estructura del cultivo; menos sensible a volumen foliar que VH
#   S1_VH_VV   : ratio que discrimina cultivo denso de suelo desnudo (ratio alto = dosel cerrado)
#                caída del ratio durante senescencia indica pérdida de estructura del dosel

top4_vars <- c("gdd_cumsum", "S1_VH", "S1_VH_VV", "S1_VV")

profiles <- model_profile(explainer, variables = top4_vars, type = "partial")

#4. Extraer perfiles agregados ----

df_pdp <- as.data.frame(profiles$agr_profiles) |>
  rename(variable = `_vname_`, x = `_x_`, yhat = `_yhat_`)

#5. Referencias agronómicas ----

agro_refs <- tribble(
  ~variable,    ~x_ref,  ~label,
  "gdd_cumsum",  800,    "Heading ≈ 800 GDD",
  "gdd_cumsum",  1400,   "Maturity ≈ 1400 GDD",
  "S1_VH",       0.05,  "Dense canopy threshold",
  "S1_VH_VV",    0.25,  "Senescence signal"
) |>
  filter(variable %in% top4_vars)

var_labels <- c(
  gdd_cumsum = "Sigma~GDD",
  S1_VH      = "S1[VH]",
  S1_VV      = "S1[VV]",
  S1_VH_VV   = "S1[VH/VV]"
)

#6. Gráfico PDP con anotaciones agronómicas ----

ggplot(df_pdp, aes(x, yhat)) +
  geom_line(linewidth = 1.2, color = "darkblue") +
  geom_vline(data        = agro_refs,
             aes(xintercept = x_ref, linetype = label),
             color       = "firebrick", alpha = 0.75,
             inherit.aes = FALSE) +
  geom_label(data        = agro_refs,
             aes(x = x_ref, y = Inf, label = label),
             vjust       = 1.2, size = 2.5,
             color       = "firebrick", fill = "white",
             alpha       = 0.85, inherit.aes = FALSE,
             label.padding = unit(0.18, "lines")) +
  scale_linetype_manual(
    values = c("longdash", "dotdash", "dashed", "dotted"),
    name   = "Agronomic reference"
  ) +
  facet_wrap(~ variable, scales = "free_x", ncol = 2,
             labeller = as_labeller(var_labels, label_parsed)) +
  labs(
    y = "Average predicted AGB (t/ha)",
    x = "Predictor value"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm")
  )

ggsave('output/figs/pdp_top4_interpretacion_agronomica.png',
       width = 7, height = 6, dpi = 300, scale = 1.2)
