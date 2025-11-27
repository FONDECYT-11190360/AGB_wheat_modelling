library(DALEX)
library(DALEXtra)
library(tidyverse)

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

model_ens <- read_rds(dir_ls('data/processed/modelos/',regexp = 'ensamblado'))

explainer_rf <- 
  explain_tidymodels(
    model_ens, 
    data = biom_train, 
    y = biom_train$biomasa,
    label = "Ensamblado",
    verbose = FALSE
  )

library(stacks)
vip <- model_parts(explainer_rf, loss_function = loss_root_mean_square)

labels <- c("Sigma~GDD","S1[VH]","S1[VV]","S1[VH/VV]","SM",
            "Sigma~PP","Sigma~S2~SWIR[12]~TCARI",
            "Sigma~S2~NDRE[3]~8*A","S2~SWIR11~TCARI",
            "S2~MCARI","S2~SWIR[12]~MCARI","S2~NDRE[3]~8*A",
            "S2~CI[red]~8*A","Sigma~S2~SWIR[12]~MCARI",
            "S2~SIPI~8*A","S2~TCARI","Sigma~S2[B1]",
            "S2[B1]","PS~EVI","Sigma~PS~GRVI")

model_profile_rf1 <- model_profile(explainer_rf, type = "partial", variables = c("gdd_cumsum", "S1_VV", "S1_VH", "S1_VH_VV","pp_cumsum","sm_mm"))

#Customizando el grafico

new_labels <- c(
  "gdd_cumsum" = "Sigma~GDD",
  "S1_VV" = "S1[VV]",
  "S1_VH" = "S1[VH]",
  "S1_VH_VV" = "S1[VH/VV]",
  "pp_cumsum" = "Sigma~PP",
  "sm_mm" = "SM")

plot(model_profile_rf1) + 
  facet_wrap(~`_vname_`,ncol=3,scales = 'free',labeller = as_labeller(new_labels,label_parsed)) + 
  #ggtitle(title = " ") +
  theme_bw() +
  #theme(title = element_blank()) +
  labs(title = NULL,subtitle = NULL,y='average prediction') +
  theme(strip.background = element_rect(fill = 'white'))
ggsave('output/figs/partial_dependence_plots_estimation.png',scale=1.5,width=5,height=3)  
