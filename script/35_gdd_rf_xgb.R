# Script 35: ∑GDD solo con RF y XGBoost (control del confundido algoritmo/sensor, IJRS 2026-10-07)
# La línea base ∑GDD del script 31 usa regresión lineal, mientras S1+GDD usa RF. Este script ajusta
# ∑GDD solo con RF y XGBoost, con la misma receta, folds e hiperparámetros fijos del script 31, para
# separar el aporte de Sentinel-1 del aporte de la no linealidad del algoritmo.
# Correr con XGB17_LIB apuntando a una librería que contenga solo xgboost 1.7.11.1.

# Datos, grupos y conjuntos: se reutiliza la sección 1 del script 31 (hasta la definición de folds)
lineas_31 <- readLines('script/31_loso_diseno_limpio.R')
fin_sec1 <- grep('^loso_folds <-', lineas_31)
eval(parse(text = lineas_31[1:fin_sec1]))

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

ajustar <- function(cj, v, m) {
  message(glue('{cj} {m}'))
  p <- length(v)
  dat <- select(data, all_of(v), biomasa, sitio_temporada)
  folds <- group_vfold_cv(dat, group = 'sitio_temporada')
  rec <- recipe(biomasa ~ ., data = dat) |>
    update_role(sitio_temporada, new_role = 'ID') |>
    step_impute_knn(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_corr(all_numeric_predictors(), -any_of('gdd_cumsum'), threshold = 0.9) |>
    step_zv(all_predictors())
  spec <- switch(m, LM = lm_spec(p), RF = rf_spec(p), XGBoost = xgb_spec(p))
  set.seed(1603)
  workflow(rec, spec) |>
    fit_resamples(folds, control = control_resamples(save_pred = TRUE)) |>
    collect_predictions() |>
    select(.row, .pred)
}

# Nuevos ajustes, más dos ajustes de control que deben reproducir el caché del script 31
disenio <- tribble(
  ~conjunto, ~variables,                    ~modelo,
  'GDD',     grupos$GDD,                    'RF',
  'GDD',     grupos$GDD,                    'XGBoost',
  'GDD',     grupos$GDD,                    'LM',
  'S1+GDD',  c(grupos$S1, grupos$GDD),      'RF'
)

res <- disenio |>
  mutate(pred = pmap(list(conjunto, variables, modelo), ajustar))

oof <- res |>
  select(conjunto, modelo, pred) |>
  unnest(pred) |>
  left_join(filas, by = '.row') |>
  mutate(id = paste(conjunto, modelo, sep = ' | '))

por_fold <- bind_rows(
  oof |> group_by(id, conjunto, modelo, sitio_temporada) |> metricas(biomasa, .pred) |>
    mutate(subconjunto = 'all'),
  oof |> filter(biomasa > 0) |> group_by(id, conjunto, modelo, sitio_temporada) |>
    metricas(biomasa, .pred) |> mutate(subconjunto = 'AGB>0'))

# Control: los ajustes repetidos deben coincidir con el caché del script 31
cache <- read_csv('output/tables/rev2_loso_limpio_por_fold.csv', show_col_types = FALSE)
control <- por_fold |>
  filter(id %in% c('GDD | LM', 'S1+GDD | RF')) |>
  left_join(select(cache, id, sitio_temporada, subconjunto, rmse_cache = rmse),
            by = c('id', 'sitio_temporada', 'subconjunto'))
stopifnot(isTRUE(all.equal(control$rmse, control$rmse_cache, tolerance = 1e-8)))

base <- por_fold |>
  filter(id == 'GDD | LM') |>
  select(subconjunto, sitio_temporada, rmse_base = rmse)

resumen <- por_fold |>
  left_join(base, by = c('subconjunto', 'sitio_temporada')) |>
  mutate(d_rmse_pct = 100 * (rmse - rmse_base) / rmse_base) |>
  group_by(subconjunto, id, conjunto, modelo) |>
  summarize(across(c(rsq, rmse, mae), list(mean = mean, sd = sd)),
            d_rmse_pct_mean = mean(d_rmse_pct),
            folds_mejor_que_gdd = sum(rmse < rmse_base),
            .groups = 'drop') |>
  arrange(subconjunto, rmse_mean)

# Comparación directa con el mismo algoritmo: S1+GDD frente a GDD, ambos con RF y con XGBoost
s1_mismo_alg <- cache |>
  filter(id %in% c('S1+GDD | RF', 'S1+GDD | XGBoost')) |>
  select(id, modelo, sitio_temporada, subconjunto, rmse_s1 = rmse) |>
  left_join(por_fold |> filter(conjunto == 'GDD') |>
              select(modelo, sitio_temporada, subconjunto, rmse_gdd = rmse),
            by = c('modelo', 'sitio_temporada', 'subconjunto')) |>
  mutate(d_rmse_pct = 100 * (rmse_s1 - rmse_gdd) / rmse_gdd)

write_csv(por_fold, 'output/tables/rev2_gdd_rf_xgb_por_fold.csv')
write_csv(resumen, 'output/tables/rev2_gdd_rf_xgb_resumen.csv')
write_csv(s1_mismo_alg, 'output/tables/rev2_s1gdd_vs_gdd_mismo_algoritmo.csv')

print(resumen, width = Inf)
print(s1_mismo_alg |> filter(subconjunto == 'all'), width = Inf)
print(s1_mismo_alg |> group_by(subconjunto, modelo) |>
        summarize(d_rmse_pct_mean = mean(d_rmse_pct), folds_mejor = sum(rmse_s1 < rmse_gdd),
                  .groups = 'drop'))
