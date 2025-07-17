library(tidyverse)

# 1.- Cargar datos ####
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

#2. Preprocesamiento ####
#

model_rec_todo <- recipe(biomasa~.,data = biom_train ) |>
  #update_role(sitio_temporada,new_role = 'dont_use') |> 
  update_role(sitio, new_role = 'dont_use') |> 
  update_role(fecha, new_role = 'dont_use') |> 
  step_impute_knn(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

model_rec_s1_clima <- model_rec_todo |> 
  step_rm(starts_with(c('S2','PS')))

model_rec_s2_clima <- model_rec_todo |>
  step_rm(starts_with(c('S1','PS')))


# 2. Cargar modelos ####
# 
files <- dir_ls('data/processed/modelos/',regexp = '\\b(rec3|rec6)_(XGBoost|RF)|(modelo_ensamblado)') 
files <- files[c(1:3,5)]
models <- map(files,read_rds)
mod_name <- files |> basename() |> str_remove('.rds')

recipes <- list(model_rec_todo,model_rec_s1_clima,model_rec_s1_clima,model_rec_s2_clima)
multi_metric <- metric_set(rsq, rmse, mae)

df_models <- seq_along(models)[2:4] |>
  map_df(\(i){
    bind_cols(biomasa = biom_test[,c('sitio','biomasa')],
              predict(models[[i]],recipes[[i]] |> prep() |> bake(new_data = biom_test))) |> 
      mutate(model = mod_name[i])
  
})

# modelo ensamblado
df_models_final <- bind_rows(df_models,
          bind_cols(biomasa = biom_test[,c('sitio','biomasa')],
                    predict(models[[1]],biom_test)) |> 
  mutate(model = mod_name[1]))

#metricas de los modelos

df_metric <- seq_along(mod_name) |> 
  map_df(\(i){
    df_models_final |> 
      filter(model == mod_name[i]) |> 
      multi_metric(truth = biomasa,estimate = .pred) |> 
      mutate(model = mod_name[i])
  })

panel_labels <- c('Ensemble | rec1','RF | rec3 (S1+W)','XGBoost | rec3 (S1+W)','XGBoost | rec6 (S2+W)')
names(panel_labels) <- mod_name

ggplot(df_models_final,aes(biomasa,.pred)) +
  geom_point(aes(colour = sitio)) +
  scale_color_manual(name = 'Site',
                     values = cols4all::c4a('poly.dark24'),
                     labels = c('Hidango','La Cancha','Villa Baviera')) +
  scale_alpha_manual(c(1,0.5)) +
  geom_abline(slope=1,lwd=1.5,colour='blue',alpha = .4) +
  geom_text(data = filter(df_metric,.metric =='rsq'),
            aes(x=30,y=5,
                label = paste("R^2==",round(.estimate,2))),parse = TRUE)+
  geom_text(data = filter(df_metric,.metric =='rmse'),
            aes(x=30,y=3.8,
                label = paste0("RMSE=",round(.estimate,2),' [ton/ha]')),parse = FALSE)+
  geom_text(data = filter(df_metric,.metric =='mae'),
            aes(x=30,y=2.6,
                label = paste0("MAE=",round(.estimate,2),' [ton/ha]')),parse = FALSE)+
  
  facet_grid(.~model,labeller = labeller(model = panel_labels)) +
  labs(y='Estimation of aboveground biomass (ton/ha)',x ='Aboveground biomass (ton/ha)') +
  theme_bw() +
  theme(strip.background = element_rect(fill  ='white'))
ggsave('output/figs/estimation_vs_observed_AGB_between_models.png',
       scale =1.3,width = 9,height=4)    

  
