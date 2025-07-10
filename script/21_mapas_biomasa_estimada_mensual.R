library(sf)
library(tidymodels)
library(terra)
library(fs)

#change locale to English
Sys.setlocale("LC_TIME", "C")

sitios <- dir_ls('data/processed/shp',regexp = 'shp$')
pol_hids1 <- read_sf(sitios[2]) |> st_transform(32719)
pol_hids2 <- read_sf(sitios[3]) |> st_transform(32719)
pol_lc <- read_sf(sitios[4]) |> st_transform(32719)
pol_vb <- read_sf(sitios[5]) |> st_transform(32719)

dirs <- dir_ls('data/processed/raster/predicciones')

biomasa <- map(dirs,\(d){
  files <- dir_ls(d,regexp = 'tif$')
  dates <- str_extract(files,'[0-9]{4}-[0-9]{2}-[0-9]{2}') |> ymd()
  r <-rast(files)
  names(r) <- dates
  r
})

hid_s1 <- biomasa[[1]]
hid_s1 <- mask(hid_s1,pol_hids1)
ids <- names(hid_s1) |> ymd() |> month()
mth_name <- names(hid_s1) |> ymd() |> months() |> unique()
hid_s1_mes <- tapp(hid_s1,ids,'mean',na.rm = TRUE)
names(hid_s1_mes) <- mth_name
plot(hid_s1_mes)

hid_s2 <- biomasa[[2]]
hid_s2 <- mask(hid_s2,pol_hids2)
ids <- names(hid_s2) |> ymd() |> month()
mth_name <- names(hid_s2) |> ymd() |> months() |> unique()
hid_s2_mes <- tapp(hid_s2,ids,'mean',na.rm = TRUE)
names(hid_s2_mes) <- mth_name
plot(hid_s2_mes)

lc <- biomasa[[3]]
lc <- mask(lc,pol_lc)
ids <- names(lc) |> ymd() |> month()
mth_name <- names(lc) |> ymd() |> months() |> unique()
lc_mes <- tapp(lc,ids,'mean',na.rm = TRUE)
names(lc_mes) <- mth_name
plot(lc_mes)

vb <- biomasa[[4]]
vb <- mask(vb,pol_vb)
ids <- names(vb) |> ymd() |> month()
mth_name <- names(vb) |> ymd() |> months() |> unique()
vb_mes <- tapp(vb,ids,'mean',na.rm = TRUE)
names(vb_mes) <- mth_name
plot(vb_mes)


library(tmap)
library(cols4all)

map1 <- tm_shape(hid_s1_mes) +
  tm_raster(col.free = FALSE,
            col.scale = tm_scale_intervals(
              style = 'jenks',
              values = c4a('spectral')
            ),
            col.legend = tm_legend(
              title = 'AGB (ton/ha)',
              orientation = "landscape")
            ) +
  tm_shape(pol_hids1) +
  tm_borders() +
  tm_facets(nrow = 1,sync = TRUE) +
  tm_layout(legend.position = tm_pos_out("center", "bottom", pos.v = "center"),
            panel.label.size   = 1,
            panel.label.bg = FALSE,
            component.autoscale = FALSE)

map2 <- tm_shape(hid_s2_mes) +
  tm_raster(col.free = FALSE,
            col.scale = tm_scale_intervals(
              style = 'jenks',
              values = c4a('spectral')
            ),
            col.legend = tm_legend(
              title = 'AGB (ton/ha)',
              orientation = "landscape")
  ) +
  tm_shape(pol_hids2) +
  tm_borders() +
  tm_facets(nrow = 1,sync = TRUE) +
  tm_layout(legend.position = tm_pos_out("center", "bottom", pos.v = "center"),
            panel.label.size   = 1,
            panel.label.bg = FALSE,
            component.autoscale = FALSE)

map3 <- tm_shape(vb_mes) +
  tm_raster(col.free = FALSE,
            col.scale = tm_scale_intervals(
              style = 'jenks',
              values = c4a('spectral')
            ),
            col.legend = tm_legend(
              title = 'AGB (ton/ha)',
              orientation = "landscape")
  ) +
  tm_shape(pol_vb) +
  tm_borders() +
  tm_facets(nrow = 1,sync = TRUE) +
  tm_layout(legend.position = tm_pos_out("center", "bottom", pos.v = "center"),
            panel.label.size   = 1,
            panel.label.bg = FALSE,
            component.autoscale = FALSE)

map4 <- tm_shape(lc_mes) +
  tm_raster(col.free = FALSE,
            col.scale = tm_scale_intervals(
              style = 'jenks',
              values = c4a('spectral')
            ),
            col.legend = tm_legend(
              title = 'AGB (ton/ha)',
              orientation = "landscape")
  ) +
  tm_shape(pol_lc) +
  tm_borders() +
  tm_facets(nrow = 1,sync = TRUE) +
  tm_layout(legend.position = tm_pos_out("center", "bottom", pos.v = "center"),
            panel.label.size   = 1,
            panel.label.bg = FALSE,
            component.autoscale = FALSE)

mapas_unidos <- tmap_arrange(map1,map2,map4,map3)
tmap_save(mapas_unidos,'output/figs/mapas_biomasa_mensual_estimada.png',
          scale =1.5,dpi = 300)
