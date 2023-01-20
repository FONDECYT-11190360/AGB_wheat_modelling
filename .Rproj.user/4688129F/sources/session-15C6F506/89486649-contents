library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# 1. biomasa y estructuras
#####

#leer los datos .csv
data <- read_csv2('data/data_raw/data_biomasa_estructuras.csv')

data |> 
  pivot_longer(biomass:hi) |> 
  ggplot(aes(name,value,fill=season)) +
  geom_boxplot() +
  facet_grid(.~site)+
  theme_bw()
  
# guardar los datos como .rds
data |> 
  write_rds('data/data_processed/data_biomasa_estructuras.rds')

# data fenología

data_fen <- read_csv2('data/data_raw/data_fenologia.csv')

data_fen |> 
  mutate(date_sentinel = dmy(date_sentinel)) |> 
  write_rds('data/data_processed/data_fenologia.rds')

data_fen |> 
  mutate(date_sentinel = dmy(date_sentinel)) |> 
  ggplot(aes(date_sentinel,gdd,color = site)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks = "3 days") +
  facet_grid(.~season,scales = 'free') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# 2. datos de suelo
##### 

data_soil_lc <- read_csv2('data/data_raw/data_soil_la_cancha.csv')

## La cancha

data_sm <- data_soil_lc |>
  select(c(1:4)) |> 
  pivot_longer(2:4,values_to = 'sm') |> 
  mutate(date = floor_date(dmy_hm(date),'1 hour'),
         depth = as.numeric(substr(name,4,5)),
         site = 'la_cancha',
         season = '2021_2022') |>
  group_by(site,season,date,depth) |> 
  summarise(sm =mean(sm,na.rm = TRUE)) |>
  relocate(season,.before = 'date') |> 
  relocate(site,.before = 'season') |> 
  relocate(depth,.before = 'sm') 
  
data_temp <- data_soil_lc |>
  select(c(1,5:7)) |> 
  pivot_longer(2:4,values_to = 'temp') |> 
  mutate(date = floor_date(dmy_hm(date),'1 hour'),
         depth = as.numeric(substr(name,3,4)),
         site = 'la_cancha',
         season = '2021_2022') |>
  group_by(site,season,date,depth) |> 
  summarise(temp =mean(temp,na.rm = TRUE)) |> 
  relocate(season,.before = 'date') |> 
  relocate(site,.before = 'season') |> 
  relocate(depth,.before = 'temp') 

data_soil_la_cancha <- left_join(data_sm,data_temp, by =c('site','season','date','depth'))

## Hidango

data_soil_hid <- read_csv2('data/data_raw/data_soil_hidango.csv')

data_sm <- data_soil_hid |>
  select(c(1:6)) |> 
  pivot_longer(4:6,values_to = 'sm') |> 
  mutate(date = floor_date(dmy_hm(date),'1 hour'),
         depth = as.numeric(substr(name,4,5))) |>
  group_by(site,season,date,depth) |> 
  summarise(sm =mean(sm,na.rm = TRUE)) |>
  relocate(season,.before = 'date') |> 
  relocate(site,.before = 'season') |> 
  relocate(depth,.before = 'sm') 

data_temp <- data_soil_hid |>
  select(c(1:3,7:9)) |> 
  pivot_longer(4:6,values_to = 'temp') |> 
  mutate(date = floor_date(dmy_hm(date),'1 hour'),
         depth = as.numeric(substr(name,3,4))) |>
  group_by(site,season,date,depth) |> 
  summarise(temp =mean(temp,na.rm = TRUE)) |> 
  relocate(season,.before = 'date') |> 
  relocate(site,.before = 'season') |> 
  relocate(depth,.before = 'temp') 

data_soil_hidango <- left_join(data_sm,data_temp, by =c('site','season','date','depth'))

data_soil_unida <- bind_rows(data_soil_la_cancha,data_soil_hidango)

write_rds(data_soil_unida,'data/data_processed/data_soil.rds')

#3.- Datos clima

## Datos clima Hidango
library(agrometR)

estaciones_agromet |> filter(nombre_ema == 'Hidango')

data <- get_agro_data(260,date_start = "2021-01-01",date_end = "2022-12-31",verbose = TRUE)
write_rds(data,'data/data_processed/data_clima_hidango.rds')

data |> 
  group_by(station_id,dia = floor_date(fecha_hora,'1 day')) |> 
  summarize(prec_dia = sum(precipitacion_horaria,na.rm = TRUE)) |> 
  ggplot(aes(dia,prec_dia)) +
  geom_col() +
  scale_x_datetime(date_breaks = "5 day") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))

data |> 
  group_by(station_id,year = floor_date(fecha_hora,'1 year')) |> 
  summarize(prec_dia = sum(precipitacion_horaria,na.rm = TRUE)) 
  
data |> 
  mutate(year = year(fecha_hora)) |> 
  group_by(station_id,year,mes = floor_date(fecha_hora,'1 month')) |> 
  summarize(prec = sum(precipitacion_horaria,na.rm = TRUE)) |> 
  ungroup() |> 
  ggplot(aes(mes,prec,fill=as.factor(year))) +
  geom_col() +
  theme_bw()

## Datos clima Ariztia

data_clima_ar <- read_csv2('data/data_raw/data_clima_ariztia.csv')

data_clima_ar |> glimpse()

data_clima_ar <- data_clima_ar |> 
  select(date,mm_precipitation,air_temperature_c) |> 
  setNames(c('date','precipitacion_mm','temp_c')) |> 
  mutate(date = force_tz(dmy_hm(date),tzone = 'America/Santiago')) |>   
  group_by(fecha_hora = floor_date(date,'1 hour')) |> 
  summarize(precipitacion_mm = sum(precipitacion_mm,na.rm = TRUE),
            temp_c = mean(temp_c,na.rm = TRUE))

data_clima_ar <- data_clima_ar |> 
  mutate(site = 'ariztia') |> 
  relocate(site,.before = 'fecha_hora')

data |> 
  select(c(2,4:3)) |> 
  mutate(site = 'hidango') |> 
  relocate(site,.before = 'fecha_hora') |> 
  rename(precipitacion_mm =precipitacion_horaria,
         temp_c = temp_promedio_aire) |> 
  bind_rows(data_clima_ar) |> 
  write_rds('data/data_processed/data_clima.rds')
  
  
