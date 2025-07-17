library(tidyverse)
library(tidymodels)


biom_res <- read_rds('data/processed/modelos/workflow_estimacion.rds')

autoplot(biom_res,select_best = TRUE)
df_rank <- rank_results(biom_res, select_best = TRUE) 


df_rank |> 
  separate(wflow_id,sep = '_',into = c('rec','model')) |>
  mutate(#lead = as.numeric(lead),
         #model = str_remove(model,'rec1_'),
         .metric = toupper(.metric),
         .metric = case_when(
           .metric == 'RSQ' ~ 'R^2',
           .default = .metric
         ),
         .metric = factor(.metric,levels = c('R^2','MAE','RMSE')),
  ) |> 
  ggplot(aes(rank,mean,colour = model,shape =rec)) + 
  geom_point() +
  geom_errorbar(aes(ymax = mean +std_err,ymin = mean - std_err)) +
  scale_color_manual(name = 'Model',
                     values = cols4all::c4a('color_blind'),
                     labels =c('bagMLP',
                               'GLMnet',
                               'KNN',
                               'RF',
                               'XGBoost')) +
  scale_shape_manual(name = 'Recipe',
                     labels = c('rec1 (all)',
                                'rec2 (S1)',
                                'rec3 (S1+W)',
                                'rec4 (W)',
                                'rec5 (S2)',
                                'rec6 (S2+W)',
                                'rec7 (PS)',
                                'rec8 (PS+W)'
                     ),values=10:17) +
  labs(y='value') +
  facet_grid(.metric~.,scales ='free',labeller = label_parsed) +
  theme_bw() +
  theme(strip.background = element_rect(fill= 'white'),
        axis.title.y = element_blank())
ggsave('output/figs/ranking_estimacion_modelos_rec.png',
       width=6,height =4,dpi =300,scale = 1.2)
