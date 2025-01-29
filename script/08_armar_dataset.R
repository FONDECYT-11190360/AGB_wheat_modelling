library(tidyverse)
library(glue)
library(scales)

#cargar datos
item <- c('biomasa','clima','sm','vi')

data_list <- lapply(item, \(x) {read_rds(glue('data/processed/rds/{x}.rds'))})
names(data_list) <- item

# data_vi <- sos |>
#   bind_rows(data_list$biomasa) |> 
#   select(-biomasa) |> 
#   bind_rows(data_list$vi_filled |> select(sitio:muestra)) |> 
#   distinct(sitio,temporada,fecha,muestra) |> 
#   arrange(temporada,sitio,fecha) |> 
#   left_join(data_list$vi_filled |> select(sitio:muestra,
#                                           EVI_cumsum:SAVI_cumsum)) |> 
#   group_by(sitio,temporada,muestra) |> 
#   mutate(across(
#     EVI_cumsum:SAVI_cumsum, 
#     ~ na.approx(., x = as.numeric(fecha), na.rm = FALSE),
#     .names = "{.col}_filled"
#   ))

data_list$biomasa |> 
  arrange(temporada,sitio,fecha) |> 
  left_join(data_list$clima) |>
  left_join(data_list$sm |> select(sitio,temporada,fecha,sm_mm)) |> 
  left_join(data_list$vi) |>
  write_rds('data/processed/rds/dataset_full_index.rds')

#visualizar

data <- read_rds('data/processed/rds/dataset_full_index.rds')

var_orden <- names(data)[-(1:4)]

data |> 
  pivot_longer(
    cols = biomasa:SAVI_cumsum, 
    names_to = "variable",
    values_to = "valor"
  ) |> 
  mutate(variable = factor(variable,levels=rev(var_orden))) |> 
  ggplot(aes(fecha,variable,color=muestra)) +
  geom_point() +
  facet_grid(~temporada+sitio,scales='free')

plot <- data |> 
  pivot_longer(
    cols = biomasa:SAVI_cumsum, 
    names_to = "variable",
    values_to = "valor"
  ) |> 
  mutate(variable = factor(variable,levels=var_orden)) |>
  group_by(sitio,temporada,variable,muestra) |> 
  mutate(valor = scale(valor)) |> 
  group_by(sitio,temporada,fecha,variable) |>
  reframe(valor = mean(valor,na.rm=T))

col_pal <- c('black',hue_pal()(length(var_orden)-1))
names(col_pal) <- var_orden

sos <- read_rds('data/processed/rds/fechas_fenologia.rds') |> 
  filter(fenologia == 'SOWING')

var_names <- gsub('_MM','',gsub('_CUMSUM','_acum',toupper(var_orden)))
var_names[c(1:2,4)] <- c('biomass','pp_acum','sm')

# plot <- plot |> 
  # filter(variable %in% c('biomasa','pp_cumsum','gdd','cumsum','sm_mm','NDVI'))
  
plot |> 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_point(data = plot |> filter(variable != 'biomasa'),
             alpha = .9, size = 1.3) +
  geom_line(data = plot |> filter(variable != 'biomasa'),
            alpha = .5, linewidth = .7,linetype = 'dashed') +
  geom_point(data = plot |> filter(variable == 'biomasa'),
             size = 1.7, alpha = 0.9) +
  geom_line(data = plot |> filter(variable == 'biomasa'),
            linewidth = .7, alpha = .8) +
  geom_text(data = sos, aes(x = fecha, y = -Inf, label = "SOS"),
            inherit.aes = FALSE, color = "gray30", vjust = -0.5, hjust =-.2) +
  facet_wrap(~sitio + temporada, scales = 'free_x', ncol = 2) +
  scale_color_manual(
    values = col_pal,
    breaks = var_orden,
    labels = var_names
  ) +
  geom_vline(data = sos, aes(xintercept = as.numeric(fecha)),
             linetype = "dashed", color = "gray30", alpha = 0.8) +
  scale_x_date(date_breaks = '2 month',
               date_minor_breaks = '1 month',
               date_labels = "%b/%Y") +
  labs(y = 'scaled values', x = NULL, color = NULL) +
  theme_bw() +
  theme(legend.position = "none")

ggsave('output/figs/series/dataset_full_index.png', width = 15, height = 8)


