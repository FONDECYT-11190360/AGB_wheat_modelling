library(tidyverse)

#datos de producción
data_bio <- read_rds('data/data_processed/data_biomasa_estructuras.rds')
data_bio |> glimpse()

data_bio |> 
  pivot_longer(-(site:sample)) |> 
  separate(sample,sep=5,into = c('pre','sample')) |> 
  ggplot(aes(date_sample,value,colour=sample)) +
    geom_point() +
    facet_wrap(site~season+name,scales = 'free')+
    theme_bw()

#datos de clima
data_clima <- read_rds('data/data_processed/data_clima.rds')
data_clima |> glimpse()

data_clima |> 
  pivot_longer(-(site:date)) |> 
  ggplot(aes(date,value)) +
  geom_point() +
  facet_wrap(site+season~name,scales = 'free')+
  theme_bw()

#datos de fenologia (grados día)
data_feno <- read_rds('data/data_processed/data_fenologia.rds')
data_feno |> glimpse()

data_feno |> 
  ggplot(aes(date_sentinel,gdd)) +
  geom_point() +
  facet_wrap(site~season,scales = 'free')+
  theme_bw()

#datos de fenología (clorofila)
data_gcc <- read_rds('data/data_processed/data_gcc.rds')
data_gcc |> glimpse()

data_gcc |>
  ggplot(aes(date,gcc)) +
  geom_point() +
  facet_wrap(site~season,scales = 'free')+
  theme_bw()

#datos de fenología (lai)
data_lai <- read_rds('data/data_processed/data_lai.rds')
data_lai |> glimpse()

data_lai |> 
  #pivot_longer(-(site:sample)) |> 
  separate(sample,sep=5,into = c('pre','sample')) |> 
  ggplot(aes(lai_cept,lai_manual,colour=sample)) +
  geom_point() +
  geom_smooth(method ='lm',se = FALSE) +
  facet_wrap(site~season,scales = 'free')+
  theme_bw()

data_lai |> 
  ggplot(aes(lai_cept,lai_manual)) +
  geom_point() +
  geom_smooth(method= 'lm') +
  theme_bw()

data_lai |> 
  drop_na() |> 
  summarize(r = cor(lai_cept,lai_manual))

#datos de suelo
data_soil <- read_rds('data/data_processed/data_soil.rds')
data_soil |> glimpse()

data_soil |> 
  filter(sm <1 & temp>=0) |> 
  pivot_longer(-(site:depth)) |>
  mutate(depth = as.character(depth)) |> 
  ggplot(aes(date,value,colour=depth)) +
  geom_point() +
  facet_wrap(site~season+name,scales = 'free',ncol=2)+
  theme_bw()

#datos satelitales (panetscope)
data_visps <- read_rds('data/data_processed/data_vis_planetscope.rds')
data_visps |> glimpse()

data_visps |> 
  #filter(value > 0) |> 
  ggplot(aes(dates,value,colour=sample)) +
  geom_point() +
  facet_wrap(site~season+vi,scales = 'free',ncol=3) +
  theme_bw()

#datos satelitales (sentinel 2)
data_viss2 <- read_rds('data/data_processed/data_vis_sentinel2.rds')
data_viss2 |> glimpse()

data_viss2 |> 
  mutate(value = ifelse(index != 'kNDVI', value/10000,value)) |> 
  group_by(sitio,season,index) |> 
  reframe(min = min(value,na.rm=T),
          max = max(value,na.rm=T),
          mean = mean(value,na.rm=T),
          median = median(value,na.rm=T),
          q1 = quantile(value,probs=.25,na.rm=T),
          q3 = quantile(value,probs=.75,na.rm=T)) |> 
  View()

#datos satelitales (sentinel 2 backup)
data_viss22 <- read_rds('data/data_processed/data_vis_sentinel2_bkp.rds')
data_viss22 |> glimpse()

data_bio |> distinct(site)

data_bio |> 
    group_by(site,season,date_sample) |> 
    summarize(biomass = mean(biomass,na.rm = TRUE)) |> 
    mutate(date_sample = ymd(date_sample))  |> 
    ggplot(aes(date_sample,biomass,colour = site,group = site)) +
    geom_point() +
    geom_line()

# visualización
data_clima |> distinct(site)

data_clima |> 
    ggplot(aes(date,temp_c)) +
    geom_point() +
    geom_line() +
    facet_wrap(site~.,scales = 'free_x')

data_gcc |> distinct(sitio)

data_gcc |> 
    mutate(fecha = ymd(fecha))  |> 
    ggplot(aes(fecha,gcc,colour=sitio)) +
    geom_point() + 
    geom_line() +
    facet_grid(.~sitio,scales = 'free')

data_lai |> distinct(site)

data_lai |> 
    mutate(date_s2 = ymd(date_sentinel))  |> 
    ggplot(aes(date_s2,lai_cept,colour=site)) +
    geom_point() + 
    geom_line() +
    facet_grid(.~site,scales = 'free')

data_soil |> ungroup() |> distinct(site)

data_soil |> 
    filter(sm <1) |> 
    ggplot(aes(date,sm)) +
    geom_point() +
    facet_grid(.~site)

data_visps |> distinct(sitio)
data_viss2 |> distinct(sitio)

data_viss2 |> 
ggplot(aes(date_sentinel,value)) +
    geom_point() +
    facet_grid(site~.,scales = 'free')

