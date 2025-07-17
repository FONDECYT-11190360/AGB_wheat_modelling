library(tidymodels)
library(tidyverse)
library(fs)

#1. Cargar datos

vars <- c("sitio", "fecha", "pp_cumsum", "sm_mm","gdd_cumsum","S2_B1", "S2_B6", "S2_MCARI", "S2_TCARI", "S2_MCARI_OSAVI2", "S2_SWIR11_MCARI", "S2_SWIR11_TCARI","S2_SWIR12_MCARI", "S2_CVI", "S2_NDRE3", "S2_NDRE_NDVI", "S2_WI1", "S2_TCARI_OSAVI_8A", "S2_CI_red_8A", "S2_NDRE3_8A", "S2_SIPI_8A", "S2_WI1_8A", "S1_VV", "S1_VH", "S1_VH_VV", "PS_B3", "PS_B8", "PS_SR", "PS_CVI", "PS_EVI", "PS_GNDVI", "PS_SIPI", "PS_CI_red_cumsum", "PS_EVI_cumsum", "PS_GRVI_cumsum", "S2_B1_cumsum","S2_SWIR12_MCARI_cumsum", "S2_SWIR12_TCARI_cumsum", "S2_NDRE3_cumsum", "S2_CI_red_8A_cumsum", "S2_NDRE3_8A_cumsum", "PS_B5", "PS_TCARI_OSAVI", "PS_MCARI_OSAVI", "PS_NDRE_NDVI", "biomasa")

#1. Leer los datos ----

data <- read_rds('data/processed/rds/dataset.rds') |> 
  select(-muestra,-temporada,fecha) |> 
  select(all_of(vars))  
#filter(sitio != 'villa_baviera')

#2. Definir subgrupos de datos para el modelado ----
set.seed(987)
splits <- initial_split(data)

biom_train <- training(splits)
biom_test <- testing(splits)

#3. Cargar modelos ####
#
files <- dir_ls('data/processed/modelos/',regexp = '\\b(rec3|rec6)_(XGBoost|RF)') 
files <- files[c(1:2,4)]
models <- map(files,read_rds)

#4. Recetas 
#
model_rec_todo <- recipe(biomasa~.,data = biom_train ) |>
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
  step_rm(starts_with(c('S2','PS')))

model_rec_clima <- model_rec_todo |> 
  step_rm(starts_with(c('S1','S2','PS')))

#5. Explicación del modelo ####
# 
library(DALEX)
library(DALEXtra)

models_name <- files |> basename()
recipes <- list(model_rec_s1_clima,model_rec_s1_clima,model_rec_s2_clima)

seq_along(models_name) |> 
  map(\(i){
    
    explainer_rf <- 
      explain_tidymodels(
        models[[i]], 
        data = recipes[[i]] |> prep()  |> bake(new_data = NULL), 
        y = biom_train$biomasa,
        label = models_name[[i]],
        verbose = FALSE
      )
    
    vip <- model_parts(explainer_rf, loss_function = loss_root_mean_square)
    vip |> 
      group_by(variable) |> 
      summarize(mean_dl = mean(dropout_loss),
                sd_dl = sd(dropout_loss)) |> 
      arrange(desc(mean_dl)) |>
      slice(-1) |> 
      slice_head(n=20) |> 
      ggplot(aes(mean_dl,fct_reorder(variable,mean_dl))) +
      coord_cartesian(xlim = c(1, 6.5)) +# Set y-axis to start at 5 and end at 15
      geom_col(fill = 'darkblue',alpha = .7) +
      geom_point() +
      geom_errorbar(aes(xmin=mean_dl-sd_dl,xmax=mean_dl+sd_dl)) +
      #scale_x_continuous(breaks = seq(9,12,0.25),limits = c(9,12)) +
      #scale_y_discrete(labels =label_parse()) +
      labs(x='Root mean square error (RMSE) loss after permutations') +
      theme_bw() +
      theme(axis.title.y = element_blank())
    ggsave(glue('output/figs/variables_importance_estimacion_{models_name[i]}.png'),
           width=6,height =3,dpi =300,scale = 1.5)
  })

# Modelo ensamblado ####

model_ens <- read_rds(dir_ls('data/processed/modelos/',regexp = 'ensamblado'))

explainer_rf <- 
  explain_tidymodels(
    model_ens, 
    data = biom_train, 
    y = biom_train$biomasa,
    label = models_name[[i]],
    verbose = FALSE
  )

vip <- model_parts(explainer_rf, loss_function = loss_root_mean_square)

labels <- c("Sigma~GDD","S1[VH]","S1[VV]","S1[VH/VV]","SM",
            "Sigma~PP","Sigma~S2~SWIR[12]~TCARI",
            "Sigma~S2~NDRE[3]~8*A","S2~SWIR11~TCARI",
            "S2~MCARI","S2~SWIR[12]~MCARI","S2~NDRE[3]~8*A",
            "S2~CI[red]~8*A","Sigma~S2~SWIR[12]~MCARI",
            "S2~SIPI~8*A","S2~TCARI","Sigma~S2[B1]",
            "S2[B1]","PS~EVI","Sigma~PS~GRVI")

vip |> 
  group_by(variable) |> 
  summarize(mean_dl = mean(dropout_loss),
            sd_dl = sd(dropout_loss)) |> 
  arrange(desc(mean_dl)) |>
  slice(-1) |> 
  slice_head(n=20) |> 
  mutate(variable = labels) |> 
  ggplot(aes(mean_dl,fct_reorder(variable,mean_dl))) +
  coord_cartesian(xlim = c(3, 6.5)) +# Set y-axis to start at 5 and end at 15
  geom_col(fill = 'darkblue',alpha = .7) +
  geom_point() +
  geom_errorbar(aes(xmin=mean_dl-sd_dl,xmax=mean_dl+sd_dl)) +
  #scale_x_continuous(breaks = seq(9,12,0.25),limits = c(9,12)) +
  scale_y_discrete(labels =label_parse()) +
  labs(x='Root mean square error (RMSE) loss after permutations') +
  theme_bw() +
  theme(axis.title.y = element_blank())
ggsave(glue('output/figs/variables_importance_estimacion_modelo_ensamblado.png'),
       width=6,height =3,dpi =300,scale = 1.2)


