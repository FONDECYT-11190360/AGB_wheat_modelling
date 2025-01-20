library(readr)
library(sf)
library(sen2r)
library(lubridate)
library(glue)
library(purrr)
library(tidyverse)
library(XML)

sit_tile <- list(
  'hidango_1' = '18HYH',
  'hidango_2' = '18HYH',
  'la_cancha' = '19HBC',
  'villa_baviera' = '19HBV'
)

date_range <- list(
  'hidango_1' = as.Date(c('2021-05-01', '2022-01-31')),
  'hidango_2' = as.Date(c('2022-05-01', '2023-01-31')),
  'la_cancha' = as.Date(c('2022-05-01', '2022-12-31')),
  'villa_baviera' = as.Date(c('2020-12-01', '2021-01-18'))
)

dir_out <- '/mnt/data_raw/ESA/Sentinel2/SAFE/'

f <- function(i,sit_tile,date_range,dir_out){
  ls <- s2_list(time_interval = date_range[[i]],tile = sit_tile[i])
  s2_download(ls,outdir = glue('{dir_out}{names(sit_tile[i])}/'))
}

test_ls <- s2_list(
  time_interval = date_range[['hidango_1']],
  tile = sit_tile[['hidango_1']]
)

map(1:length(sit_tile), function(i) {
  safely(function() {
    f(i, sit_tile = sit_tile, date_range = date_range, dir_out = dir_out)
  })()
})

f(3,sit_tile = sit_tile, date_range = date_range,dir_out = dir_out)
