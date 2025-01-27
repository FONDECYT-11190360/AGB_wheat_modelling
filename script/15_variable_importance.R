library(tidymodels)
library(tidyverse)
library(vip)

get_rf_imp <- function(x) {
  x |> 
    extract_fit_parsnip() |> 
    vip::vi(scale = TRUE)
}

data <- read_rds('data/processed/rds/dataset_full_index.rds') |> 
  select(-muestra,-temporada,fecha)

pot_train <- training(splits) 
pot_test  <- testing(splits) 

set.seed(987)
splits <- initial_split(data,strata = 'sitio')

#splits <- group_initial_split(data,group = fecha)

biom_train <- training(splits)
biom_test <- testing(splits)

vb_folds <- vfold_cv(pot_train,v=5)

model_wf <- read_rds('data/processed/modelos/random_forest.rds')

ctrl_imp <- control_grid(extract = get_rf_imp)

model_fit_resample <-
  model_wf |> 
  fit_resamples(vb_folds,control = ctrl_imp)


df_var_imp <- model_fit_resample |> 
  select(id, .extracts) |> 
  unnest(.extracts) |> 
  unnest(.extracts) |> 
  group_by(Variable) |> 
  summarise(Mean = mean(Importance),
            Variance = sd(Importance)) 


df_var_imp |>
  mutate(Mean = (Mean-min(Mean))/(max(Mean)-min(Mean))) |> 
  slice_max(Mean,n=15) |> 
  ggplot(aes(reorder(Variable,Mean),Mean)) +
  geom_col(position = position_dodge()) +
  scale_fill_viridis_d('Model',option = 'E',alpha=.7) +
  scale_y_continuous(expand = c(0,0.0),limits = c(0,1.05)) +
  labs(y = 'Importance') +
  coord_flip() +
  #facet_grid(.~split) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = 'white'))
