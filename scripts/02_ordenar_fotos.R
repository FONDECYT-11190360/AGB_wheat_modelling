#install.packages('exifr')
library(exifr)

folder <- '/media/francisco/TOSHIBA EXT/Hemera/trigo/hidango/'

files <- list.files(folder,recursive = TRUE,full.names = TRUE)

#leyendo metadat de los archivo de fotos
data <- read_exif(files)

library(dplyr)
library(lubridate)
library(ggplot2)

data |> 
  mutate(date = ymd_hms(FileModifyDate)) |>
  group_by(day = floor_date(date,'1 day')) |> 
  summarize(n_fotos = n()) |> 
  ggplot(aes(n_fotos)) + geom_histogram()

# seleccionar fotos
data_filtrada <- data |> 
  mutate(
    date = ymd_hms(FileModifyDate,tz = 'America/Santiago'),
    date2 = as.Date(ymd_hms(FileModifyDate)),
    hour = hour(date),
    mins = minute(date)) |>  
  filter(hour %in% c(13,14,15,16) & (mins == 0 | mins==59)) |> 
  select(date,date2,SourceFile,FileModifyDate)

library(purrr)
library(terra)

dir_out <- '/media/francisco/TOSHIBA EXT/Hemera/trigo/hidango/fotos_prom_selec/'

data_filtrada$date2 |>
  unique() |> 
  map(function(d){
    files <- data_filtrada |> 
      filter(date2 == d) |> 
      pull(SourceFile)
    im <- rast(files)
    if (length(files) > 1){
      l <- map(1:3,\(x) seq(x,nlyr(im),3))
      r <- map(l,function(ind){
        app(im[[ind]],mean)
      }) 
      out <- rast(r)
    } else out <- im
    RGB(out) <- 1:3
    writeRaster(out, paste0(dir_out,format(d,'%Y%m%d'),'.tif'),
                gdal=c("COMPRESS=LZW"),datatype = 'INT1U')
    rm(out)
    gc()
  })

  group_by(day = floor_date(date,'1 day')) |> 
  summarize(n_fotos = n()) |> filter(n_fotos == 1)
  ggplot(aes(n_fotos)) + geom_histogram()





data |> 
  mutate(date = ymd_hms(FileModifyDate),
         sec = second(date)) |>
  filter(sec != 0) |> 
  select(Directory,FileName)

data |> 
  #group_by(ISO) |> 
  #count()
  filter(ISO == 6400) |> 
  select(Directory,FileName)

  
group_by(day = floor_date(date,'1 day')) |> 
  summarize(n_fotos = n()) |> 
  ggplot(aes(n_fotos)) + geom_histogram()

  filter(FileModifyDate <= ymd_hms("2021:12:01 07:00:00"))  |> 
  pull(FileModifyDate)