library(tidyverse)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

abrev <- function(string) {
  str_extract_all(string, "\\b[A-Za-z]") |>
    unlist() |>
    str_c(collapse = "")
}

fechas_feno <- read_rds('data/processed/rds/fechas_fenologia.rds') |> 
  mutate(fecha_nueva = as.Date(ifelse(fenologia == 'RIPENING', fecha+days(5),
                                      fecha + days(ceiling((as.numeric(lead(fecha))-as.numeric(fecha))/2))))) |> 
  rowwise() |> 
  mutate(label = abrev(fenologia)) |> 
  mutate(fecha = as.Date(paste0(ifelse(month(fecha)>=5,2021,2022),'-',
                                month(fecha),'-',day(fecha))),
         fecha_nueva = as.Date(paste0(ifelse(month(fecha_nueva)>=5,2021,2022),'-',
                                      month(fecha_nueva),'-',day(fecha_nueva))),
         sitio = abrev(str_to_title(gsub('_',' ',sitio))),
         sitio = case_when(sitio == 'H' & temporada == '2021-2022' ~ 'H1',
                           sitio == 'H' & temporada == '2022-2023' ~ 'H2',
                           .default = sitio),
         sitio = factor(sitio,levels = c('VB','LC','H1','H2'))) |> 
  mutate(label = case_when(fenologia == 'EAR EMERGENCE' ~ 'E',
                           fenologia == 'MILK DEVELOPMENT' ~ 'M',
                           fenologia == 'DOUGH DEVELOPMENT' ~ 'D',
                           .default = label))

data_vi <- read_rds('data/processed/rds/vi.rds') |> 
  group_by(sitio,temporada,fecha) |> 
  reframe(NDVI = mean(S2_NDVI,na.rm=T)) |> 
  rowwise() |>
  mutate(fecha_nueva = as.Date(paste0(ifelse(month(fecha)>=5,2021,2022),'-',month(fecha),'-',day(fecha))),
         sitio = abrev(str_to_title(gsub('_',' ',sitio))),
         sitio = case_when(sitio == 'H' & temporada == '2021-2022' ~ 'H1',
                           sitio == 'H' & temporada == '2022-2023' ~ 'H2',
                           .default = sitio),
         sitio = factor(sitio,levels = c('VB','LC','H1','H2'))) |> 
  ungroup()

data_biomasa <- read_rds('data/processed/rds/biomasa.rds') |> 
  rowwise() |> 
  mutate(fecha_nueva = as.Date(paste0(ifelse(month(fecha)>=5,2021,2022),'-',month(fecha),'-',day(fecha)))) |> 
  mutate(sitio = abrev(str_to_title(gsub('_',' ',sitio))),
         sitio = case_when(sitio == 'H' & temporada == '2021-2022' ~ 'H1',
                           sitio == 'H' & temporada == '2022-2023' ~ 'H2',
                           .default = sitio),
         sitio = factor(sitio,levels = c('VB','LC','H1','H2'))) |> 
  group_by(sitio,temporada,fecha,fecha_nueva) |> 
  reframe(biomasa = mean(biomasa,na.rm=T)) |> 
  left_join(data_vi |> select(-fecha),by=c('sitio','temporada','fecha_nueva')) |> 
  group_by(sitio,temporada) |> 
  mutate(sub = 0:(length(sitio)-1),
         sub = ifelse(sub == first(sub),'SOS',sub),
         sub = ifelse(sub == last(sub),'final',sub),
         label = sprintf("S[%s]", sub)) |> 
  ungroup()
  
coef <- max(data_biomasa$biomasa, na.rm = TRUE) / max(data_vi$NDVI, na.rm = TRUE)

data_vi |> 
  ggplot(aes(fecha_nueva)) +
  geom_hline(yintercept = 0, alpha = .4, col = 'black') +
  geom_vline(data = fechas_feno, aes(xintercept = fecha), linetype = 'dashed', alpha = .4,color = 'navyblue') +
  geom_smooth(aes(y = NDVI * coef, color = 'NDVI'), method = "loess", span = 0.1, 
              linewidth = 1, se = F, alpha = .6) +
  geom_line(data = data_biomasa, aes(y = biomasa, color='AGB'), linewidth = 1, alpha = .7) +
  geom_point(data = data_biomasa, aes(y = biomasa), size = 2, color = "darkorange", alpha = .7, shape = 21, fill = 'white') +
  geom_text(data = fechas_feno, aes(x = fecha_nueva, y = 42, label = label, hjust = .5), size = 3) +
  geom_label(data = data_biomasa,aes(x = fecha_nueva,y = biomasa,label = label),
            size = 3.5,alpha = .5,vjust = -.3,parse = TRUE, label.size = 0) +
  scale_y_continuous(name = "AGB",
                     sec.axis = sec_axis(~ . / coef, name = "NDVI"),
                     breaks = seq(0, 40, by = 10), 
                     limits = c(-5, 42), 
                     minor_breaks = NULL) +
  scale_x_date(date_breaks='2 months',minor_breaks='1 month',date_labels='%b') +
  scale_color_manual(values = c("NDVI" = "darkolivegreen3", "AGB" = "darkorange"),name = NULL) +
  labs(x = NULL, y = NULL, fill = NULL) +
  facet_wrap(~sitio + temporada, ncol = 2) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, 0, -10),
        strip.background = element_rect(fill='white'))

ggsave('output/figs/fenology_and_biomass.png', height = 6, width = 8)
  