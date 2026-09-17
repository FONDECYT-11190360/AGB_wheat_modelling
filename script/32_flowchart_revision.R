# Script 32: diagrama metodológico del manuscrito revisado (Round 2, 2026-09-17)

library(tidyverse)

cajas <- tribble(
  ~id, ~x,   ~y,   ~w,  ~h,   ~tipo,     ~texto,
  1,   1.0,  5.0,  1.7, 0.9,  'dato',    'In-situ AGB\n153 samples, 4 site-seasons',
  2,   3.0,  5.0,  1.7, 0.9,  'dato',    'Weather and soil moisture\n∑GDD, ∑PP, SM',
  3,   5.0,  5.0,  1.7, 0.9,  'dato',    'Sentinel-1\nVV, VH, VH/VV',
  4,   7.0,  5.0,  1.7, 0.9,  'dato',    'Sentinel-2 and PlanetScope\nbands, VIs, cumulative VIs',
  5,   4.0,  3.6,  7.7, 0.8,  'proceso', 'Daily interpolation, extraction at sampling points, candidate predictor set (39 variables)',
  6,   4.0,  2.3,  7.7, 0.9,  'proceso', 'Predictor sets: DAS and ∑GDD baselines, weather, single sensors, sensor + ∑GDD, sensor + weather, all\nPreprocessing inside each training fold: KNN imputation, normalization, correlation filter (|r| ≥ 0.9)',
  7,   2.2,  0.9,  3.9, 1.0,  'modelo',  'Leave-one-site-season-out CV (4 folds)\nLM baselines, RF and XGBoost with fixed hyperparameters\nR², RMSE, MAE per held-out site-season',
  8,   6.1,  0.9,  3.3, 1.0,  'modelo',  'Explainability (RF, all predictors)\nheld-out permutation importance\nfor variables and sensor groups; ALE profiles'
)

flechas <- tribble(
  ~x, ~y, ~xend, ~yend,
  1.0, 4.55, 1.0, 4.0,
  3.0, 4.55, 3.0, 4.0,
  5.0, 4.55, 5.0, 4.0,
  7.0, 4.55, 7.0, 4.0,
  4.0, 3.2, 4.0, 2.75,
  2.2, 1.85, 2.2, 1.4,
  6.1, 1.85, 6.1, 1.4,
  4.15, 0.9, 4.45, 0.9
)

ggplot() +
  geom_segment(data = flechas, aes(x, y, xend = xend, yend = yend),
               arrow = arrow(length = unit(2, 'mm'), type = 'closed'), linewidth = 0.4) +
  geom_tile(data = cajas, aes(x, y, width = w, height = h, fill = tipo),
            colour = 'grey20', linewidth = 0.3) +
  geom_text(data = cajas, aes(x, y, label = texto), size = 2.6, lineheight = 0.9) +
  scale_fill_manual(values = c(dato = '#E8F1FA', proceso = '#F5F5F5', modelo = '#FDF0E1'), guide = 'none') +
  coord_cartesian(xlim = c(0, 8), ylim = c(0.3, 5.5), expand = FALSE) +
  theme_void()

ggsave('output/figs/flowchart_diagram_rev2.png', width = 7, height = 4.2, dpi = 300, bg = 'white', device = ragg::agg_png)
