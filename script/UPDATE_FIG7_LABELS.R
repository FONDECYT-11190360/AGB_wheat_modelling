# Update Figure 7 labels to use ∑ notation instead of _cumsum
# Run this after script 31 to regenerate the figure with proper labels

library(tidyverse)
library(patchwork)

# Function to format variable names with ∑ notation
format_variable_label <- function(v) {
  # Replace _cumsum with ∑ prefix
  label <- str_replace(v, "_cumsum$", "")

  # Add ∑ symbol for cumulative variables
  if (str_detect(v, "_cumsum$")) {
    # Handle specific cases
    label <- case_when(
      v == "gdd_cumsum" ~ "∑GDD",
      v == "pp_cumsum" ~ "∑PP",
      v == "PS_CI_red_cumsum" ~ "∑PS_CI[red]",
      v == "PS_EVI_cumsum" ~ "∑PS_EVI",
      v == "PS_GRVI_cumsum" ~ "∑PS_GRVI",
      v == "S2_B1_cumsum" ~ "∑S2_B1",
      v == "S2_SWIR12_MCARI_cumsum" ~ "∑S2_MCARI[SW12]",
      v == "S2_SWIR12_TCARI_cumsum" ~ "∑S2_TCARI[SW12]",
      v == "S2_NDRE3_cumsum" ~ "∑S2_NDRE3",
      v == "S2_CI_red_8A_cumsum" ~ "∑S2_CI[red]*",
      v == "S2_NDRE3_8A_cumsum" ~ "∑S2_NDRE3*",
      TRUE ~ label
    )
  } else {
    # Non-cumulative variables - keep original formatting
    label <- case_when(
      v == "sm_mm" ~ "SM",
      str_detect(v, "_8A") ~ str_replace(v, "_", "_") %>% paste0("*"),
      TRUE ~ v
    )
  }

  return(label)
}

# Read the VIP summary data
vip_resumen <- read_csv('output/tables/rev2_vip_heldout_rf_all.csv')

# Define color palette
paleta_grupos <- setNames(cols4all::c4a('color_blind', 6), c('GDD', 'PP', 'SM', 'S1', 'S2', 'PS'))

# Function to assign group
etiqueta_grupo <- function(v) {
  case_when(
    v == 'gdd_cumsum' ~ 'GDD',
    v == 'pp_cumsum' ~ 'PP',
    v == 'sm_mm' ~ 'SM',
    str_starts(v, 'S1_') ~ 'S1',
    str_starts(v, 'S2_') ~ 'S2',
    str_starts(v, 'PS_') ~ 'PS'
  )
}

# Panel (a): Top 15 individual predictors with formatted labels
p_var <- vip_resumen |>
  filter(nivel == 'variable') |>
  slice_head(n = 15) |>
  mutate(
    grupo = etiqueta_grupo(variable),
    variable_label = map_chr(variable, format_variable_label)
  ) |>
  ggplot(aes(delta_media, fct_reorder(variable_label, delta_media), colour = grupo)) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey50') +
  geom_pointrange(aes(xmin = delta_min, xmax = delta_max), size = 0.25) +
  scale_colour_manual(values = paleta_grupos, guide = 'none') +
  labs(x = 'Increase in held-out RMSE (t/ha)', y = NULL, title = '(a) Individual predictors') +
  theme_bw() +
  theme(axis.text.y = element_text(size = 9))

# Panel (b): Predictor groups
p_grp <- vip_resumen |>
  filter(nivel == 'grupo') |>
  ggplot(aes(delta_media, fct_reorder(variable, delta_media), colour = variable)) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey50') +
  geom_pointrange(aes(xmin = delta_min, xmax = delta_max), size = 0.25) +
  scale_colour_manual(values = paleta_grupos, guide = 'none') +
  labs(x = 'Increase in held-out RMSE (t/ha)', y = NULL, title = '(b) Predictor groups') +
  theme_bw()

# Combine panels
patchwork::wrap_plots(p_var, p_grp, ncol = 2, widths = c(1.4, 1), guides = 'collect') &
  theme(legend.position = 'bottom', plot.title = element_text(size = 10))

# Save updated figure
ggsave('output/figs/loso_limpio_importancia_heldout.png',
       width = 6.5, height = 4, dpi = 300, scale = 1.2)

cat("Figure 7 updated with ∑ notation\n")
cat("Saved to: output/figs/loso_limpio_importancia_heldout.png\n")
