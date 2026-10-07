# Script 34: resumen gráfico (graphical abstract) para European Journal of Agronomy
# Requisito Elsevier: 531 x 1328 px (alto x ancho) o proporcionalmente más, legible a 5 x 13 cm.
# Se diseña al tamaño final (13 x 5.3 cm) para que los textos sean legibles impresos.

library(tidyverse)
library(patchwork)

pt <- function(x) x / ggplot2::.pt   # tamaño de texto en puntos para geom_text

col_gdd  <- '#D55E00'
col_sat  <- '#0072B2'
col_comb <- '#009E73'
col_base <- 'grey45'

titulo <- element_text(face = 'bold', size = 7, hjust = 0.5, margin = margin(b = 1))

tema_vacio <- theme_void(base_size = 6) +
  theme(plot.title = titulo, plot.margin = margin(2, 2, 2, 2))

#1. Panel de datos ----

predictores <- tibble(
  y     = c(5.0, 3.95, 2.9, 1.85, 0.8),
  label = c('∑GDD (thermal time)', 'Sentinel-1', 'Sentinel-2',
            'PlanetScope', 'Rain, soil moisture'),
  fill  = c(col_gdd, col_sat, col_sat, col_sat, col_base)
)

p_datos <- ggplot() +
  annotate('text', x = 5, y = 9.3, label = '153 biomass samples',
           size = pt(6), fontface = 'bold') +
  annotate('text', x = 5, y = 8.2, label = '4 wheat fields, Chile', size = pt(6)) +
  annotate('text', x = 5, y = 7.1, label = '3 seasons, 2020 to 2023', size = pt(6)) +
  annotate('text', x = 5, y = 6.05, label = 'Predictors', size = pt(6), fontface = 'italic') +
  geom_tile(data = predictores, aes(x = 5, y = y, fill = fill),
            width = 9.8, height = 0.88, colour = NA) +
  geom_text(data = predictores, aes(x = 5, y = y, label = label),
            colour = 'white', size = pt(5.8), fontface = 'bold') +
  scale_fill_identity() +
  coord_cartesian(xlim = c(0, 10), ylim = c(0.3, 10), expand = FALSE) +
  labs(title = 'Data') +
  tema_vacio

#2. Panel de validación LOSSO ----

campos <- c('HN', 'HS', 'LC', 'VB')   # Hidango North, Hidango South, La Cancha, Villa Baviera

folds <- expand_grid(fold = 1:4, campo = 1:4) |>
  mutate(rol = if_else(fold == campo, 'Test', 'Train'),
         x = campo * 2.3 - 0.75,
         y = 8.0 - (fold - 1) * 1.35)

p_losso <- ggplot(folds) +
  geom_tile(aes(x = x, y = y, fill = rol), width = 2.05, height = 1.1, colour = 'white') +
  annotate('text', x = (1:4) * 2.3 - 0.75, y = 9.3, label = campos,
           size = pt(6), fontface = 'bold') +
  annotate('text', x = 5, y = 2.25, label = 'Train on 3 fields,\ntest on the 4th',
           size = pt(6), lineheight = 0.9) +
  annotate('text', x = 5, y = 0.8, label = 'LM, RF, XGBoost', size = pt(6), fontface = 'bold') +
  scale_fill_manual(values = c(Test = col_gdd, Train = 'grey80'), guide = 'none') +
  coord_cartesian(xlim = c(0, 10), ylim = c(0.3, 10), expand = FALSE) +
  labs(title = 'LOSSO validation') +
  tema_vacio

#3. Panel de resultados ----

seleccion <- c('DAS | LM'     = 'Days after sowing',
               'GDD | LM'     = '∑GDD, linear',
               'S1 | RF'      = 'Sentinel-1',
               'S2 | RF'      = 'Sentinel-2',
               'PS | RF'      = 'PlanetScope',
               'S1+GDD | RF'  = 'Sentinel-1 + ∑GDD',
               'All | RF'     = 'All predictors')

res <- read_csv('output/tables/rev2_loso_limpio_resumen.csv', show_col_types = FALSE) |>
  filter(subconjunto == 'all', id %in% names(seleccion)) |>
  mutate(etiqueta = factor(seleccion[id], levels = rev(seleccion)),
         grupo = case_when(conjunto %in% c('DAS', 'GDD') ~ 'Baseline',
                           conjunto %in% c('S1', 'S2', 'PS') ~ 'Single sensor',
                           .default = 'Sensor + thermal time'))

base_gdd <- res$rmse_mean[res$id == 'GDD | LM']

p_res <- ggplot(res, aes(rmse_mean, etiqueta, colour = grupo)) +
  geom_vline(xintercept = base_gdd, linetype = 'dashed', colour = col_gdd, linewidth = 0.35) +
  geom_linerange(aes(xmin = rmse_mean - rmse_sd, xmax = rmse_mean + rmse_sd), linewidth = 0.45) +
  geom_point(size = 1.1) +
  scale_colour_manual(values = c(Baseline = col_gdd, 'Single sensor' = col_sat,
                                 'Sensor + thermal time' = col_comb), guide = 'none') +
  scale_x_continuous(breaks = seq(4, 14, 2)) +
  labs(x = 'RMSE in held out fields (t/ha)', y = NULL, title = 'Error in unseen fields') +
  theme_bw(base_size = 6) +
  theme(plot.title = titulo,
        axis.text = element_text(size = 6, colour = 'black'),
        axis.title.x = element_text(size = 6),
        axis.ticks.length = unit(1, 'pt'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(2, 4, 1, 2))

#4. Mensaje principal y composición ----

p_msg <- ggplot() +
  annotate('rect', xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = 'grey20') +
  annotate('text', x = 0.5, y = 0.5, size = pt(6.5), fontface = 'bold', colour = 'white',
           label = 'Thermal time beat every single sensor model; satellite gains were modest') +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

flecha <- ggplot() +
  annotate('text', x = 0.5, y = 0.5, label = '▶', size = pt(9), colour = 'grey55') +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_void()

ga <- (p_datos + flecha + p_losso + flecha + p_res +
         plot_layout(widths = c(3, 0.22, 3, 0.22, 4.6))) /
  p_msg +
  plot_layout(heights = c(1, 0.12))

dir.create('output/figs/graphical_abstract', showWarnings = FALSE)
archivo <- 'output/figs/graphical_abstract/graphical_abstract'
ggsave(paste0(archivo, '.pdf'), ga, width = 13, height = 5.3, units = 'cm', device = cairo_pdf)
ggsave(paste0(archivo, '.tiff'), ga, width = 13, height = 5.3, units = 'cm', dpi = 600,
       bg = 'white', compression = 'lzw')
ggsave(paste0(archivo, '.png'), ga, width = 13, height = 5.3, units = 'cm', dpi = 300, bg = 'white')
