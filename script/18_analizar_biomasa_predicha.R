library(fs)
library(terra)
library(tidyverse)

dirs <- dir_ls('data/processed/raster/predicciones',recurse = TRUE,
                type = 'directory')


out <- map_df(dirs,\(folder){
  files <- dir_ls(folder)

  bio <- rast(files)
  df <- global(bio,'mean',na.rm = TRUE)
  df_cov <- global(bio,\(x) sd(x,na.rm = TRUE)/mean(x,na.rm=TRUE))
  
  data <- tibble(
    temporada = str_extract(folder,'[0-9]{4}-[0-9]{4}'),
    sitio = str_remove(basename(folder),'_[0-9]{4}-[0-9]{4}'),
    date = basename(files) |> 
      str_extract('[0-9]{4}-[0-9]{2}-[0-9]{2}') |> 
      ymd(),
    biomasa = df$mean,
    cov = 100*df_cov$global,
                 )
  data
})


out |> 
  #filter(date > "2021-06-01" & cov < 100) |> 
  #filter(cov >0 & cov < 100) |> 
  ggplot(aes(date,biomasa)) + 
  geom_point(size=.2) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  facet_wrap(sitio~temporada,scales ="free_x") +
  theme_bw()
