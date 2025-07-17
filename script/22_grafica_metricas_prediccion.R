library(tidyverse)
library(terra)
library(sf)

# Cargar metricas modelos de predicción
# 
files <- dir_ls('data/processed/modelos',regexp = 'metrics(.*)lead')

df_metrics <- files |> 
  map_df(\(file){
    read_rds(file) |> 
      mutate(lead = str_extract(file,'_[0-9]{1}_') |> str_remove_all('_'))
    
  })

#Visualizar metricas
df_metrics |> 
  filter(str_detect(model,'rec1')) |> 
  mutate(lead = as.numeric(lead),
         model = str_remove(model,'rec1_'),
         .metric = toupper(.metric),
         .metric = case_when(
           .metric == 'RSQ' ~ 'R^2',
           .default = .metric
         ),
         .metric = factor(.metric,levels = c('R^2','MAE','RMSE')),
  ) |> 
  ggplot(aes(lead,.estimate,color = model,shape=model,group=.metric)) +
  #geom_line() +
  geom_line(stat = "smooth", alpha = 0.3,lwd=1.5,colour = 'darkblue') +
  geom_point() + 
  #geom_smooth(se = FALSE,alpha =.5) +
  scale_color_manual('Model',values = cols4all::c4a('poly.alphabet2'),
                     labels = c('bagMLP','GLMnet','KNN','RF','XGBoost')) +
  scale_shape_manual('Model',values = 6:10,
                     labels = c('bagMLP','GLMnet','KNN','RF','XGBoost')) +
  facet_wrap(.~.metric,scales = 'free_y',labeller = label_parsed) +
  labs(x='Prediction lead time (months)') +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        axis.title.y = element_blank()) 
ggsave('output/figs/metrics_prediction_lead_times.png',width=6,height = 3,dpi=300,scale=1.3)
