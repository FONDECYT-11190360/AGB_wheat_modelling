library(tidyverse)
library(terra)
library(sf)
library(fs)

# Cargar metricas por fold (LOSO) de los modelos de predicción
files <- dir_ls('data/processed/modelos', regexp = 'loso_metrics_per_fold_prediccion(.*)lead')

metrics_per_fold <- files |>
  map_df(\(file){
    read_rds(file) |>
      mutate(lead = str_extract(file,'_[0-9]{1}_') |> str_remove_all('_'))
  })

# Para cada modelo y lead, seleccionar la configuración de hiperparámetros con
# mayor R^2 promedio entre los folds LOSO (igual criterio que en la etapa 1),
# y promediar R^2, RMSE y MAE de esa configuración entre los folds.
best_cfg <- metrics_per_fold |>
  filter(.metric == 'rsq') |>
  group_by(lead, wflow_id, .config) |>
  summarize(mean_rsq = mean(.estimate), .groups = 'drop') |>
  group_by(lead, wflow_id) |>
  slice_max(mean_rsq, n = 1, with_ties = FALSE) |>
  select(lead, wflow_id, .config)

df_metrics <- metrics_per_fold |>
  inner_join(best_cfg, by = c('lead','wflow_id','.config')) |>
  group_by(lead, wflow_id, .metric) |>
  summarize(mean = mean(.estimate), std_err = sd(.estimate)/sqrt(n()), .groups = 'drop') |>
  rename(model = wflow_id)

# Visualizar metricas (rec1, modelos LOSO-estables; GLMnet excluido por
# predicciones numéricamente inestables -RMSE/MAE 3-10x mayores que el resto-)
df_metrics |>
  filter(str_detect(model,'rec1'),
         !str_detect(model,'GLM')) |>
  mutate(lead = as.numeric(lead),
         model = str_remove(model,'rec1_'),
         .metric = toupper(.metric),
         .metric = case_when(
           .metric == 'RSQ' ~ 'R^2',
           .default = .metric
         ),
         .metric = factor(.metric,levels = c('R^2','MAE','RMSE')),
  ) |>
  ggplot(aes(lead,mean,color = model,shape=model,group=.metric)) +
  geom_line(stat = "smooth", alpha = 0.3,lwd=1.5,colour = 'darkblue') +
  geom_point() +
  scale_color_manual('Model',values = cols4all::c4a('poly.alphabet2'),
                     labels = c('bagMLP','KNN','RF','XGBoost')) +
  scale_shape_manual('Model',values = 6:9,
                     labels = c('bagMLP','KNN','RF','XGBoost')) +
  facet_wrap(.~.metric,scales = 'free_y',labeller = label_parsed) +
  labs(x='Prediction lead time (months)') +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        axis.title.y = element_blank())
ggsave('output/figs/metrics_prediction_lead_times.png',width=6,height = 3,dpi=300,scale=1.3)
