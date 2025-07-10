library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)

#0. Variables a seleccionar del análisis de correlacion

vars <- c("sitio", "fecha", "pp_cumsum", "sm_mm","gdd_cumsum","S2_B1", "S2_B6", "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI2", "S2_SWIR11_MCARI", "S2_SWIR11_TCARI","S2_SWIR12_MCARI", "S2_CVI", "S2_NDRE3", "S2_NDRE_NDVI", "S2_WI1", "S2_TCARI_OSAVI_8A", "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A", "S1_VV", "S1_VH", "S1_VH_VV", "PS_B3", "PS_B8", "PS_SR", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI", "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum", "S2_B1_cumsum","S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum", "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum", "PS_B5", "PS_TCARI_OSAVI", "PS_MCARI_OSAVI", "PS_NDRE_NDVI", "biomasa")

#1. Leer los datos ----

data <- read_rds('data/processed/rds/dataset.rds') |> 
  select(-muestra,-temporada,fecha) |> 
  select(all_of(vars))  
  #filter(sitio != 'villa_baviera')

#2. Definir subgrupos de datos para el modelado ----
set.seed(987)
splits <- initial_split(data)

#splits <- group_initial_split(data,group = fecha)

biom_train <- training(splits)
biom_test <- testing(splits)

#3. Especificación de los modelos a utilizar ----

## XGBoost
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),                    ## first three: model complexity
  sample_size = tune(), mtry = tune(),        ## randomness
  learn_rate = tune()                         ## step size
) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

### LightGBM

lgbm_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),                    ## first three: model complexity
  sample_size = tune(), mtry = tune(),        ## randomness
  learn_rate = tune()                         ## step size
) |> 
  set_engine("lightgbm") |> 
  set_mode("regression")

## Random Forest

rf_spec <-rand_forest(
  trees = 1000,
  mtry = tune(),
  min_n = tune()) |> 
  set_engine("ranger",importance = 'impurity') |> 
  set_mode("regression")

## Support Vector Machine

svm_spec <-
  svm_rbf(cost = tune(), 
          rbf_sigma = tune())  |> 
  set_mode("regression")  |> 
  set_engine("kernlab")

## 
glmnet_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine("glmnet") |> 
  set_mode("regression") 

# neural network

bagMLP_spec <- bag_mlp(
  hidden_units = tune(), # Tune the number of hidden units
  penalty = tune(), # Tune the regularization penalty
  epochs = tune()  # Tune the dropout rate
)  |> 
  set_engine("nnet") |> 
  set_mode("regression")

# K- Nearest neighbour

knn_spec <- nearest_neighbor(
  mode = "regression",
  neighbors = tune(),         # k
  weight_func = tune(),       # "rectangular" or "triangular" (uniform vs. distance)
  dist_power = tune()         # 1 = Manhattan, 2 = Euclidean
) %>%
  set_engine("kknn")

#4. Preprocesamiento

model_rec_todo <- recipe(biomasa~.,data = biom_train ) |>
  #update_role(sitio_temporada,new_role = 'dont_use') |> 
  update_role(sitio, new_role = 'dont_use') |> 
  update_role(fecha, new_role = 'dont_use') |> 
  step_impute_knn(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

model_rec_s2 <- model_rec_todo |> 
  step_rm(pp_cumsum, sm_mm,starts_with(c('S1','PS')))

model_rec_s2_clima <- model_rec_todo |>
  step_rm(starts_with(c('S1','PS')))

model_rec_ps <- model_rec_todo |> 
  step_rm(pp_cumsum, sm_mm,starts_with(c('S1','S2')))

model_rec_ps_clima <- model_rec_todo |> 
  step_rm(starts_with(c('S1','S2')))

model_rec_s1 <- model_rec_todo |> 
  step_rm(pp_cumsum, sm_mm,starts_with(c('S2','PS')))

model_rec_s1_clima <- model_rec_todo |> 
  step_rm(starts_with(c('S1','PS')))

model_rec_clima <- model_rec_todo |> 
  step_rm(starts_with(c('S1','S2','PS')))

# filt_obj <- prep(model_rec_todo,training = biom_train)
# filt_te <- bake(filt_obj,biom_test)

#4. Resampling y tunning
library(stacks)
ctrl <- control_stack_grid()

set.seed(453)
vb_folds <- vfold_cv(biom_train,v=5)

library(bonsai)
library(baguette)
biom_res <- 
  workflow_set(
    preproc = list(
      rec1 = model_rec_todo,
      rec2 = model_rec_s1,
      rec3 = model_rec_s1_clima,
      rec4 = model_rec_clima,
      rec5 = model_rec_s2,
      rec6 = model_rec_s2_clima,
      rec7 = model_rec_ps,
      rec8 = model_rec_ps_clima
                   ), 
    models = list(
      RF = rf_spec,
      #SVM = svm_spec,
      XGBoost = xgb_spec,
      #lgbm = lgbm_spec
      GLM = glmnet_spec,
      bagMLP = bagMLP_spec,
      KNN = knn_spec
    )
  ) |>  
  workflow_map(
    verbose = TRUE,
    seed = 1603,
    resamples = vb_folds,
    grid = 10,
    metrics = metric_set(rsq,rmse, mae),
    control = ctrl
  )

autoplot(biom_res,select_best = TRUE)
df_rank <- rank_results(biom_res, select_best = TRUE) 

df_rank |> 
  separate(wflow_id,sep = '_',into = c('rec','model')) |> 
  ggplot(aes(rank,mean,colour = rec,shape =model)) + 
  geom_point() +
  geom_errorbar(aes(ymax = mean +std_err,ymin = mean - std_err)) +
  scale_color_manual(name = 'Recipe',values = cols4all::c4a('spectral'),
                     labels = c('rec1 (all)',
                                'rec2 (S1)',
                                'rec3 (S1+W)',
                                'rec4 (W)',
                                'rec5 (S2)',
                                'rec6 (S2+W)',
                                'rec7 (PS)',
                                'rec8 (PS+W)'
                                )) +
  labs(y='value') +
  facet_grid(.metric~.,scales ='free') +
  theme_bw() +
  theme(strip.background = element_rect(fill= 'white'))
ggsave('output/figs/ranking_estimacion_modelos_rec.png',
       width=6,height =4,dpi =300,scale = 1.5)

#5. Rankear modelos ----

rankings <- 
  rank_results(biom_res, select_best = TRUE) |> 
  mutate(method = map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[1])) 

tidymodels_prefer()
df_rank <- rankings |>  
  dplyr::select(rank, mean, model, wflow_id, .metric,std_err) |> 
  filter(.metric == c('rsq','rmse')[1]) |> 
  rename(Model = wflow_id) |> 
  mutate(Model = str_remove(Model,'rec_')) 

#6. Ultimo entrenamiento de los modelos ----

models_name <- df_rank$Model

models_lfit <- models_name |> 
  map(\(model){
    biom_res |>  
    extract_workflow(model) |>  
    finalize_workflow(
      biom_res |>  
        extract_workflow_set_result(model) |>  
        select_best(metric = "rsq")
    ) |>  
    last_fit(split = splits, metrics = metric_set(rsq,rmse,mae)) 
  })

# 7. Metricas de los modelos ----

df_metrics <- seq_along(models_name) |> 
  map_df(\(i){
    tibble(collect_metrics(models_lfit[[i]]),model = models_name[i])
  })

# df_metrics <- df_metrics |> 
#   mutate(
#     .metric = toupper(.metric),
#     x = -1,
#     y = case_when(
#       .metric == 'RSQ' ~ -2.5,
#       .metric == 'MAE' ~ -2.7,
#       .default = -2.9),
#     unit = case_when(
#       .metric == 'RSQ' ~ '',
#       .default = '~~MPa'),
#     
#     .metric = case_when(.metric == 'RSQ' ~ 'R^2',
#                         .default = .metric)
#   )

write_rds(df_metrics,'data/processed/modelos/metrics.rds')

#8. Extraer y guardar modelos ----

seq_along(models_name) |> 
  walk(\(i){
    mname <- models_name[i]
    models_lfit[[i]] |> 
      extract_fit_parsnip() |> 
      write_rds(glue('data/processed/modelos/{mname}.rds'))
  })

#9. ensamblar modelos ----

library(stacks)

model_ensemble <- stacks() |> 
  add_candidates(biom_res) |> 
  blend_predictions() |> 
  fit_members()

write_rds(model_ensemble,'data/processed/modelos/modelo_ensamblado.rds')

autoplot(model_ensemble,select_best = TRUE)

bind_cols(biom_test, predict(model_ensemble, biom_test))

# Explicación del modelo
# 
library(DALEX)
library(DALEXtra)
explainer_rf <- 
  explain_tidymodels(
    model_ensemble, 
    data = biom_train, 
    y = biom_train$biomasa,
    label = "Ensamblado",
    verbose = FALSE
  )

vip_rf <- model_parts(explainer_rf, loss_function = loss_root_mean_square)
plot(vip_rf)

labels <- c("Sigma~GDD","S1~VH","S1~VV","S1~VH/VV","SM",
            "Sigma~PS~GRVI","S2~WI1","S2~NDRE3","S2~CVI",
            "Sigma~S2~SWIR12~TCARI","S2~B6","Sigma~PP",
            "PS~CVI","Sigma~PS~CI~red","Sigma~S2~B1",          
            "S2~SWIR11~TCARI","S2~NDRE~NDVI","S2~TCARI",
            "Sigma~S2~CI[red]*8*A","Sigma~S2~SWIR12~MCARI")

vip_rf |> 
  group_by(variable) |> 
  summarize(mean_dl = mean(dropout_loss),
            sd_dl = sd(dropout_loss)) |> 
  arrange(desc(mean_dl)) |>
  slice(-1) |> 
  slice_head(n=20) |> 
  mutate(variable = labels) |> 
  ggplot(aes(mean_dl,fct_reorder(variable,mean_dl))) +
  coord_cartesian(xlim = c(1, 6.5)) +# Set y-axis to start at 5 and end at 15
  geom_col(fill = 'darkblue',alpha = .7) +
  geom_point() +
  geom_errorbar(aes(xmin=mean_dl-sd_dl,xmax=mean_dl+sd_dl)) +
  #scale_x_continuous(breaks = seq(9,12,0.25),limits = c(9,12)) +
  scale_y_discrete(labels =label_parse()) +
  labs(x='Root mean square error (RMSE) loss after permutations') +
  theme_bw() +
  theme(axis.title.y = element_blank())
ggsave('output/figs/variables_importance.png',
       width=6,height =3,dpi =300,scale = 1.5)

explainer_rf2 <- 
  explain_tidymodels(
    mod1, 
    data = biom_train, 
    y = biom_train$biomasa,
    label = "Mod1",
    verbose = FALSE
  )

vip_rf2 <- model_parts(explainer_rf2, loss_function = loss_root_mean_square)
plot(vip_rf)
