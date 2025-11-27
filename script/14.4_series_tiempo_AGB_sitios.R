library(terra)
library(tidyverse)
library(fs)
library(sf)

dirs <- dir_ls('data/processed/raster/predicciones')

agb_rasters <- dirs |> 
  map(\(dir){
    rast(dir_ls(dir,regexp = 'tif'))
    
  })

#extraer fechas

dates <- dirs |> 
  map(\(dir){
    dir_ls(dir) |> str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
  })

names(agb_rasters)

# [1] "data/processed/raster/predicciones/hidango_2021-2022"      
# [2] "data/processed/raster/predicciones/hidango_2022-2023"      
# [3] "data/processed/raster/predicciones/la_cancha_2022-2023"    
# [4] "data/processed/raster/predicciones/villa_baviera_2020-2021"


layers <- st_layers('data/processed/sitios.gpkg')

#Hidango 2021-2022

pol <- read_sf('data/processed/sitios.gpkg', layer = "a_hidango_2021-2022")

agb_hid1 <- mask(agb_rasters[[1]],st_transform(pol,32719))
df_agb_hid1 <- global(agb_hid1,'mean',na.rm = TRUE)

df_hidango1 <- tibble(dates = dates[[1]] |> ymd(),AGB = df_agb_hid1$mean, sitio = 'Hidango S1')
#Hidango 2022-2023

pol <- read_sf('data/processed/sitios.gpkg', layer = "a_hidango_2022-2023")

agb_hid2 <- mask(agb_rasters[[2]],st_transform(pol,32719))
df_agb_hid2 <- global(agb_hid2,'mean',na.rm = TRUE)

df_hidango2 <- tibble(dates = dates[[2]] |> ymd(),AGB = df_agb_hid2$mean, sitio = 'Hidango S2')

#la_cancha 2022-2023"

pol <- read_sf('data/processed/sitios.gpkg', layer = "a_la_cancha_2022-2023")

agb_lc <- mask(agb_rasters[[3]],st_transform(pol,32719))
df_agb_lc <- global(agb_lc,'mean',na.rm = TRUE)

df_la_cancha <- tibble(dates = dates[[3]] |> ymd(),AGB = df_agb_lc$mean, sitio = 'La Cancha')

#villa_baviera 2020-2021

pol <- read_sf('data/processed/sitios.gpkg', layer = "a_villa_baviera_2020-2021")

agb_vb <- mask(agb_rasters[[4]],st_transform(pol,32719))
df_agb_vb <- global(agb_vb,'mean',na.rm = TRUE)

df_villa_baviera <- tibble(dates = dates[[4]] |> ymd(),AGB = df_agb_vb$mean, sitio = 'Villa Baviera')

# Crear data.frame

df_hidango1 <- df_hidango1 |> 
  mutate(dates = dates + years(1))

df_villa_baviera <- df_villa_baviera |> 
  mutate(dates = dates + years(2))

  
data <- bind_rows(df_hidango1,df_hidango2,df_la_cancha,df_villa_baviera)
#data$dates <- data$dates |> str_replace('[0-9]{4}','2000')

data |> 
  ggplot(aes(dates,AGB,colour = sitio)) +
  geom_line() +
  scale_x_date(date_breaks = "1 months",date_labels = '%m-%b') +
  scale_color_manual(values=c('darkred','red','#8ef064','#17a9cd')) +
  labs(y='AGB (ton/h)') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())
ggsave('output/figs/time_series_AGB_mean_sites.png')
