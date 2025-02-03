library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)

#1. Leer los datos ----

data <- read_rds('data/processed/rds/data_prediccion_lead_1_mes.rds') |> 
  select(-lead_month)

#2. Definir subgrupos de datos para el modelado ----
set.seed(987)
splits <- initial_split(data,strata = 'sitio')

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

## Multilayer perceptron

glmnet_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine("glmnet") |> 
  set_mode("regression") 

#4. Preprocesamiento

model_rec_todo <- recipe(cosecha~.,data = biom_train ) |>
  update_role(sitio, new_role = 'dont_use') |> 
  step_impute_knn(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) 
  # step_corr(all_numeric_predictors()) |> 
  # step_dummy(all_nominal_predictors(), one_hot = TRUE)

# model_rec_s2 <- model_rec_todo |> 
#   step_rm(pp_cumsum, sm_mm,starts_with('S1|PS'))
# 
# model_rec_s2_clima <- model_rec_todo |>
#   step_rm(starts_with('S1|PS'))
# 
# model_rec_ps <- model_rec_todo |> 
#   step_rm(pp_cumsum, sm_mm,starts_with('S1|S2'))
# 
# model_rec_ps_clima <- model_rec_todo |> 
#   step_rm(starts_with('S1|S2'))
# 
# model_rec_s1 <- model_rec_todo |> 
#   step_rm(pp_cumsum, sm_mm,starts_with('S2|PS'))
# 
# model_rec_s1_clima <- model_rec_todo |> 
#   step_rm(starts_with('S1|PS'))
# 
# model_rec_clima <- model_rec_todo |> 
#   step_rm(starts_with('S1|S2|PS'))

# filt_obj <- prep(model_rec_todo,training = biom_train)
# filt_te <- bake(filt_obj,biom_test)

#4. Resampling y tunning

ctrl <- control_stack_grid()

set.seed(453)
vb_folds <- vfold_cv(biom_train,strata = 'sitio')

library(bonsai)
library(doMC)
registerDoMC(cores = parallel::detectCores()-1)

biom_res <- 
  workflow_set(
    preproc = list(rec1 = model_rec_todo
                   # rec2 = model_rec_s1,
                   # rec3 = model_rec_s1_clima,
                   # rec4 = model_rec_clima,
                   # rec5 = model_rec_s2,
                   # rec6 = model_rec_s2_clima,
                   # rec7 = model_rec_ps,
                   # rec8 = model_rec_ps_clima,
                   #rec9 = model_rec_clima
                   ), 
    models = list(
      RF = rf_spec,
      SVM = svm_spec,
      XGBoost = xgb_spec,
      lgbm = lgbm_spec,
      glm = glmnet_spec
      #MLP = mlp_spec
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

#5. Rankear modelos ----

rankings <- 
  rank_results(biom_res[1,], select_best = TRUE) |> 
  mutate(method = map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[1])) 

tidymodels_prefer()
df_rank <- rankings |>  
  dplyr::select(rank, mean, model, wflow_id, .metric,std_err) |> 
  filter(.metric == c('rsq','rmse')[1]) |> 
  rename(Model = wflow_id) |> 
  mutate(Model = str_remove(Model,'rec_')) 
