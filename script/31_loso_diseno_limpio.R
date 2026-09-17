# Script 31: análisis LOSO limpio para el manuscrito revisado (Round 2, 2026-09-17)
# Diseño:
#   - 4 folds = 4 sitio-temporadas (leave-one-site-season-out)
#   - hiperparámetros fijos (mismos valores del script 24), sin selección sobre el fold retenido
#   - las variables se eliminan ANTES de imputar, normalizar y filtrar por correlación,
#     de modo que cada conjunto de predictores es idéntico en todos los folds
#   - se excluyen los predictores red edge de PlanetScope (ausentes en 2 de 4 sitio-temporadas)
#   - líneas base: regresión lineal sobre días desde la siembra (DAS) y sobre ∑GDD
#   - R² tradicional (1 - SSE/SST), RMSE y MAE por fold; media ± SD entre folds
#   - importancia por permutación sobre el sitio retenido (variables y grupos de sensores)
#   - efectos locales acumulados (ALE) del modelo con todos los predictores

# Con xgboost 3.2 en R 4.6 aparecieron errores de alineación de memoria al ajustar; los resultados
# se generaron con xgboost 1.7.11.1 (misma versión que los modelos guardados, ver script 30).
if (nzchar(Sys.getenv('XGB17_LIB'))) .libPaths(c(Sys.getenv('XGB17_LIB'), .libPaths()))

library(tidymodels)
library(tidyverse)
library(glue)
library(DALEX)
library(DALEXtra)

tidymodels_prefer()
dir.create('output/tables', showWarnings = FALSE)

metricas <- function(df, truth, estimate) {
  df |>
    summarize(rsq      = rsq_trad_vec({{ truth }}, {{ estimate }}),
              r2_cor   = rsq_vec({{ truth }}, {{ estimate }}),
              rmse     = rmse_vec({{ truth }}, {{ estimate }}),
              mae      = mae_vec({{ truth }}, {{ estimate }}),
              n        = n(),
              .groups  = 'drop')
}

#1. Datos y conjuntos de predictores ----

vars <- c("pp_cumsum", "sm_mm","gdd_cumsum","S2_B1", "S2_B6", "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI2", "S2_SWIR11_MCARI", "S2_SWIR11_TCARI","S2_SWIR12_MCARI", "S2_CVI", "S2_NDRE3", "S2_NDRE_NDVI", "S2_WI1", "S2_TCARI_OSAVI_8A", "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A", "S1_VV", "S1_VH", "S1_VH_VV", "PS_B3", "PS_B8", "PS_SR", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI", "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum", "S2_B1_cumsum","S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum", "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum")

siembra <- read_rds('data/processed/rds/fechas_fenologia.rds') |>
  filter(fenologia == 'SOWING') |>
  select(sitio, temporada, fecha_siembra = fecha)

data <- read_rds('data/processed/rds/dataset.rds') |>
  mutate(sitio_temporada = paste0(sitio, "_", temporada)) |>
  left_join(siembra, by = c('sitio', 'temporada')) |>
  mutate(das = as.numeric(fecha - fecha_siembra)) |>
  select(sitio_temporada, fecha, das, all_of(vars), biomasa)

filas <- data |>
  mutate(.row = row_number()) |>
  select(.row, sitio_temporada, fecha, das, biomasa)

grupos <- list(
  GDD  = 'gdd_cumsum',
  PP   = 'pp_cumsum',
  SM   = 'sm_mm',
  S1   = grep('^S1_', vars, value = TRUE),
  S2   = grep('^S2_', vars, value = TRUE),
  PS   = grep('^PS_', vars, value = TRUE)
)
W <- c(grupos$GDD, grupos$PP, grupos$SM)

conjuntos <- tribble(
  ~conjunto,      ~variables,
  'DAS',          'das',
  'GDD',          grupos$GDD,
  'W',            W,
  'S1',           grupos$S1,
  'S2',           grupos$S2,
  'PS',           grupos$PS,
  'S1+GDD',       c(grupos$S1, grupos$GDD),
  'S2+GDD',       c(grupos$S2, grupos$GDD),
  'PS+GDD',       c(grupos$PS, grupos$GDD),
  'S1+W',         c(grupos$S1, W),
  'S2+W',         c(grupos$S2, W),
  'PS+W',         c(grupos$PS, W),
  'S2+W+S1',      c(grupos$S2, W, grupos$S1),
  'All',          c(W, grupos$S1, grupos$S2, grupos$PS)
)

receta <- function(variables) {
  recipe(biomasa ~ ., data = select(data, all_of(variables), biomasa)) |>
    step_impute_knn(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_corr(all_numeric_predictors(), threshold = 0.9) |>
    step_zv(all_predictors())
}

loso_folds <- group_vfold_cv(data, group = 'sitio_temporada')

#2. Modelos con hiperparámetros fijos ----

rf_spec <- function(p) {
  rand_forest(trees = 1000, mtry = min(7, p), min_n = 5) |>
    set_engine('ranger', seed = 1603, importance = 'none') |>
    set_mode('regression')
}

xgb_spec <- function(p) {
  boost_tree(trees = 500, tree_depth = 4, learn_rate = 0.05, min_n = 5,
             sample_size = 0.8, mtry = min(7, p)) |>
    set_engine('xgboost', counts = TRUE, nthread = 1) |>
    set_mode('regression')
}

lm_spec <- function(p) linear_reg() |> set_engine('lm')

disenio <- conjuntos |>
  mutate(p = map_int(variables, length)) |>
  expand_grid(modelo = c('LM', 'RF', 'XGBoost')) |>
  filter((modelo == 'LM') == (conjunto %in% c('DAS', 'GDD')) | conjunto == 'W')

archivo_res <- 'data/processed/modelos/rev2_loso_limpio_resultados.rds'

if (file.exists(archivo_res)) {
  res <- read_rds(archivo_res)
} else {
  res <- disenio |>
    mutate(pred = pmap(list(conjunto, variables, modelo, p), \(cj, v, m, p) {
      message(glue('{cj} {m}'))
      dat <- select(data, all_of(v), biomasa, sitio_temporada)
      folds <- group_vfold_cv(dat, group = 'sitio_temporada')
      rec <- recipe(biomasa ~ ., data = dat) |>
        update_role(sitio_temporada, new_role = 'ID') |>
        step_impute_knn(all_numeric_predictors()) |>
        step_normalize(all_numeric_predictors()) |>
        step_corr(all_numeric_predictors(), threshold = 0.9) |>
        step_zv(all_predictors())
      spec <- switch(m, LM = lm_spec(p), RF = rf_spec(p), XGBoost = xgb_spec(p))
      set.seed(1603)
      workflow(rec, spec) |>
        fit_resamples(folds, control = control_resamples(save_pred = TRUE)) |>
        collect_predictions() |>
        select(.row, .pred)
    }))
  write_rds(res, archivo_res)
}

oof <- res |>
  select(conjunto, modelo, pred) |>
  unnest(pred) |>
  left_join(filas, by = '.row') |>
  mutate(id = paste(conjunto, modelo, sep = ' | '))

#3. Métricas por fold, resumen y diferencias pareadas contra la línea base ∑GDD ----

por_fold <- bind_rows(
  oof |> group_by(id, conjunto, modelo, sitio_temporada) |> metricas(biomasa, .pred) |>
    mutate(subconjunto = 'all'),
  oof |> filter(biomasa > 0) |> group_by(id, conjunto, modelo, sitio_temporada) |>
    metricas(biomasa, .pred) |> mutate(subconjunto = 'AGB>0'))

write_csv(por_fold, 'output/tables/rev2_loso_limpio_por_fold.csv')

base <- por_fold |>
  filter(conjunto == 'GDD', modelo == 'LM') |>
  select(subconjunto, sitio_temporada, rmse_base = rmse, rsq_base = rsq)

resumen <- por_fold |>
  left_join(base, by = c('subconjunto', 'sitio_temporada')) |>
  mutate(d_rmse_pct = 100 * (rmse - rmse_base) / rmse_base) |>
  group_by(subconjunto, id, conjunto, modelo) |>
  summarize(across(c(rsq, r2_cor, rmse, mae), list(mean = mean, sd = sd)),
            d_rmse_pct_mean = mean(d_rmse_pct),
            folds_mejor_que_gdd = sum(rmse < rmse_base),
            .groups = 'drop') |>
  arrange(subconjunto, rmse_mean)

write_csv(resumen, 'output/tables/rev2_loso_limpio_resumen.csv')

# Ablación secuencial a partir de W (RF y XGBoost)
ablacion <- resumen |>
  filter(subconjunto == 'all', conjunto %in% c('W', 'S2+W', 'S2+W+S1', 'All'),
         modelo %in% c('RF', 'XGBoost')) |>
  mutate(conjunto = factor(conjunto, levels = c('W', 'S2+W', 'S2+W+S1', 'All'))) |>
  arrange(modelo, conjunto) |>
  group_by(modelo) |>
  mutate(rmse_antes = dplyr::lag(rmse_mean),
         cambio_pct = 100 * (rmse_mean - rmse_antes) / rmse_antes) |>
  ungroup() |>
  select(modelo, conjunto, rmse_antes, rmse_mean, rmse_sd, cambio_pct)

write_csv(ablacion, 'output/tables/rev2_loso_limpio_ablacion.csv')

#4. Figura: desempeño por conjunto de predictores ----

orden <- conjuntos$conjunto

resumen |>
  filter(subconjunto == 'all') |>
  select(conjunto, modelo, rmse_mean, rmse_sd, rsq_mean, rsq_sd) |>
  pivot_longer(-c(conjunto, modelo), names_to = c('.metric', '.value'), names_sep = '_') |>
  mutate(.metric = factor(.metric, levels = c('rmse', 'rsq'), labels = c('RMSE (t/ha)', 'R²')),
         conjunto = factor(conjunto, levels = rev(orden))) |>
  ggplot(aes(mean, conjunto, colour = modelo)) +
  geom_vline(data = \(d) filter(d, conjunto == 'GDD'), aes(xintercept = mean),
             linetype = 'dashed', colour = 'grey50') +
  geom_pointrange(aes(xmin = mean - sd, xmax = mean + sd),
                  position = position_dodge(width = 0.6), size = 0.25) +
  facet_wrap(~.metric, scales = 'free_x') +
  scale_colour_manual(name = 'Model', values = cols4all::c4a('color_blind')) +
  labs(x = 'Mean ± SD across the four LOSO folds', y = 'Predictor set') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'), legend.position = 'bottom')

ggsave('output/figs/loso_limpio_desempeno_conjuntos.png',
       width = 6, height = 4.5, dpi = 300, scale = 1.2)

#5. Figura: observado vs predicho fuera del fold y trayectoria temporal ----

modelos_fig <- c('GDD | LM', 'S1+GDD | RF', 'All | RF')

etq <- por_fold |>
  filter(subconjunto == 'all', id %in% modelos_fig) |>
  group_by(id) |>
  summarize(lab = sprintf('RMSE = %.2f t/ha\nR² = %.2f', mean(rmse), mean(rsq)))

etiquetas_sitio <- c('hidango_2021-2022' = 'Hidango 2021-2022',
                     'hidango_2022-2023' = 'Hidango 2022-2023',
                     'la_cancha_2022-2023' = 'La Cancha 2022-2023',
                     'villa_baviera_2020-2021' = 'Villa Baviera 2020-2021')

oof |>
  filter(id %in% modelos_fig) |>
  mutate(id = factor(id, levels = modelos_fig)) |>
  ggplot(aes(biomasa, .pred, colour = sitio_temporada)) +
  geom_abline(linetype = 'dashed') +
  geom_point(alpha = 0.7, size = 1.2) +
  geom_text(data = mutate(etq, id = factor(id, levels = modelos_fig)),
            aes(x = 0, y = 45, label = lab), inherit.aes = FALSE,
            hjust = 0, vjust = 1, size = 2.8) +
  facet_wrap(~id) +
  coord_equal(xlim = c(0, 45), ylim = c(-2, 45)) +
  scale_colour_manual(name = NULL, values = cols4all::c4a('color_blind'), labels = etiquetas_sitio) +
  labs(x = 'Observed AGB (t/ha)', y = 'Out-of-fold predicted AGB (t/ha)') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'), legend.position = 'bottom')

ggsave('output/figs/loso_limpio_obs_vs_pred.png',
       width = 6.5, height = 3.4, dpi = 300, scale = 1.3)

oof |>
  filter(id %in% modelos_fig) |>
  group_by(id, sitio_temporada, das) |>
  summarize(obs = mean(biomasa), pred = mean(.pred), .groups = 'drop') |>
  mutate(id = factor(id, levels = modelos_fig)) |>
  ggplot(aes(das)) +
  geom_point(aes(y = obs), colour = 'black', size = 1.2) +
  geom_line(aes(y = pred, colour = id)) +
  facet_wrap(~sitio_temporada, labeller = as_labeller(etiquetas_sitio)) +
  scale_colour_manual(name = 'Out-of-fold prediction', values = cols4all::c4a('color_blind')) +
  labs(x = 'Days after sowing', y = 'Mean AGB per sampling date (t/ha)') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'), legend.position = 'bottom')

ggsave('output/figs/loso_limpio_trayectoria_das.png',
       width = 6, height = 4.5, dpi = 300, scale = 1.2)

#6. Importancia por permutación sobre el sitio retenido (RF, todos los predictores) ----

todas <- conjuntos$variables[[which(conjuntos$conjunto == 'All')]]

archivo_vip <- 'data/processed/modelos/rev2_vip_heldout_rf_all.rds'

if (file.exists(archivo_vip)) {
  vip_folds <- read_rds(archivo_vip)
} else {
  set.seed(1603)
  vip_folds <- map_dfr(unique(data$sitio_temporada), \(st) {
    message(glue('DALEX {st}'))
    train <- filter(data, sitio_temporada != st) |> select(all_of(todas), biomasa)
    test  <- filter(data, sitio_temporada == st) |> select(all_of(todas), biomasa)
    fit <- workflow(receta(todas), rf_spec(length(todas))) |> fit(train)
    expl <- explain_tidymodels(fit, data = select(test, -biomasa), y = test$biomasa,
                               label = st, verbose = FALSE)
    bind_rows(
      model_parts(expl, loss_function = loss_root_mean_square, B = 50) |>
        as_tibble() |> mutate(nivel = 'variable'),
      model_parts(expl, loss_function = loss_root_mean_square, B = 50,
                  variable_groups = grupos) |>
        as_tibble() |> mutate(nivel = 'grupo')) |>
      mutate(sitio_temporada = st)
  })
  write_rds(vip_folds, archivo_vip)
}

vip_resumen <- vip_folds |>
  group_by(nivel, sitio_temporada, permutation) |>
  mutate(full = dropout_loss[variable == '_full_model_']) |>
  ungroup() |>
  filter(!variable %in% c('_full_model_', '_baseline_')) |>
  mutate(delta = dropout_loss - full) |>
  group_by(nivel, sitio_temporada, variable) |>
  summarize(delta = mean(delta), full = mean(full), .groups = 'drop') |>
  group_by(nivel, variable) |>
  summarize(delta_media = mean(delta), delta_sd = sd(delta),
            delta_min = min(delta), delta_max = max(delta),
            folds_positivo = sum(delta > 0), rmse_modelo = mean(full), .groups = 'drop') |>
  arrange(nivel, desc(delta_media))

write_csv(vip_resumen, 'output/tables/rev2_vip_heldout_rf_all.csv')

paleta_grupos <- setNames(cols4all::c4a('color_blind', 6), c('GDD', 'PP', 'SM', 'S1', 'S2', 'PS'))

etiqueta_grupo <- function(v) {
  case_when(v == 'gdd_cumsum' ~ 'GDD', v == 'pp_cumsum' ~ 'PP', v == 'sm_mm' ~ 'SM',
            str_starts(v, 'S1_') ~ 'S1', str_starts(v, 'S2_') ~ 'S2', str_starts(v, 'PS_') ~ 'PS')
}

p_var <- vip_resumen |>
  filter(nivel == 'variable') |>
  slice_head(n = 15) |>
  mutate(grupo = etiqueta_grupo(variable)) |>
  ggplot(aes(delta_media, fct_reorder(variable, delta_media), colour = grupo)) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey50') +
  geom_pointrange(aes(xmin = delta_min, xmax = delta_max), size = 0.25) +
  scale_colour_manual(name = NULL, values = paleta_grupos, drop = TRUE) +
  labs(x = 'Increase in held-out RMSE (t/ha)', y = NULL, title = '(a) Individual predictors') +
  theme_bw()

p_grp <- vip_resumen |>
  filter(nivel == 'grupo') |>
  ggplot(aes(delta_media, fct_reorder(variable, delta_media), colour = variable)) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey50') +
  geom_pointrange(aes(xmin = delta_min, xmax = delta_max), size = 0.25) +
  scale_colour_manual(values = paleta_grupos, guide = 'none') +
  labs(x = 'Increase in held-out RMSE (t/ha)', y = NULL, title = '(b) Predictor groups') +
  theme_bw()

patchwork::wrap_plots(p_var, p_grp, ncol = 2, widths = c(1.4, 1), guides = 'collect') &
  theme(legend.position = 'bottom', plot.title = element_text(size = 10))

ggsave('output/figs/loso_limpio_importancia_heldout.png',
       width = 6.5, height = 4, dpi = 300, scale = 1.2)

#7. Efectos locales acumulados (ALE) del RF con todos los predictores ajustado a todos los datos ----

fit_all <- workflow(receta(todas), rf_spec(length(todas))) |>
  fit(select(data, all_of(todas), biomasa))

expl_all <- explain_tidymodels(fit_all, data = select(data, all_of(todas)), y = data$biomasa,
                               label = 'RF all predictors', verbose = FALSE)

top6 <- vip_resumen |> filter(nivel == 'variable') |> slice_head(n = 6) |> pull(variable)

ale <- model_profile(expl_all, variables = top6, N = NULL, type = 'accumulated')

write_rds(ale, 'data/processed/modelos/rev2_ale_rf_all.rds')

as_tibble(ale$agr_profiles) |>
  mutate(`_vname_` = factor(`_vname_`, levels = top6)) |>
  ggplot(aes(`_x_`, `_yhat_`)) +
  geom_line() +
  geom_rug(data = pivot_longer(select(data, all_of(top6)), everything(),
                               names_to = '_vname_', values_to = '_x_') |>
             mutate(`_vname_` = factor(`_vname_`, levels = top6)),
           aes(`_x_`), inherit.aes = FALSE, alpha = 0.3, sides = 'b') +
  facet_wrap(~`_vname_`, scales = 'free_x') +
  labs(x = 'Predictor value', y = 'Accumulated local effect on AGB (t/ha)') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/figs/loso_limpio_ale_rf_all.png',
       width = 6, height = 4, dpi = 300, scale = 1.2)
