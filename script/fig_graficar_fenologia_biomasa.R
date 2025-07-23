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
         sitio = str_to_title(gsub('_',' ',sitio))) |> 
  mutate(label = case_when(fenologia == 'EAR EMERGENCE' ~ 'E',
                           fenologia == 'MILK DEVELOPMENT' ~ 'M',
                           fenologia == 'DOUGH DEVELOPMENT' ~ 'D',
                           .default = label)) |> 
  mutate(temporada = paste0(substr(temporada,1,4),' season'))

data_vi <- read_rds('data/processed/rds/vi.rds') |> 
  group_by(sitio,temporada,muestra,fecha) |> 
  reframe(NDVI = mean(S2_NDVI,na.rm=T)) |> 
  mutate(sitio = str_to_title(gsub('_',' ',sitio)),
         fecha = as.Date(paste0(ifelse(month(fecha)>=5,2021,2022),'-',month(fecha),'-',day(fecha)))) |> 
  mutate(temporada = paste0(substr(temporada,1,4),' season'))

data_biomasa <- read_rds('data/processed/rds/biomasa.rds') |> 
  mutate(sitio = str_to_title(gsub('_',' ',sitio)),
         fecha = as.Date(paste0(ifelse(month(fecha)>=5,2021,2022),'-',month(fecha),'-',day(fecha)))) |> 
  mutate(temporada = paste0(substr(temporada,1,4),' season'))

data_plot <- data_vi |> 
  left_join(data_biomasa) |> 
  mutate(temporada = paste0(substr(temporada,1,4),' season'))

data_label <- data_biomasa |>
  group_by(sitio,temporada,fecha) |> 
  reframe(biomasa = mean(biomasa,na.rm=T)) |> 
  group_by(sitio,temporada) |> 
  mutate(sub = 0:(length(sitio)-1),
         sub = ifelse(sub == first(sub),'SOS',sub),
         sub = ifelse(sub == last(sub),'final',sub),
         label = sprintf("S[%s]", sub)) |> 
  ungroup() |> 
  mutate(temporada = paste0(substr(temporada,1,4),' season'))

data_plot |> 
  ggplot(aes(fecha,biomasa,color = 'AGB')) +
  geom_vline(data = fechas_feno, aes(xintercept = fecha), linetype = 'dotted', alpha = .6, color = 'navyblue') +
  geom_line(aes(fecha,NDVI*45,color = 'NDVI'), stat = "smooth", method = 'gam',
            linewidth = .8, alpha = 0.6) +
  geom_line(data=data_label,linewidth = .8, alpha = 0.7) +
  geom_point(size = 1) +
  # geom_boxplot(aes(group=fecha)) +
  geom_label(data = data_label, aes(fecha,biomasa,label = label), col = 'black',
             size = 3.5,alpha = .5,vjust = -.3,hjust = 1,parse = TRUE, label.size = 0) +
  geom_text(data = fechas_feno, aes(x = fecha_nueva, y = 47, label = label, hjust = .5), size = 3, color = 'black') +
  scale_color_manual(values = c("NDVI" = "darkolivegreen3", "AGB" = "darkorange"),name = NULL) +
  scale_y_continuous(name = "AGB",
                     sec.axis = sec_axis(~ . / 45, 
                                         name = "NDVI",
                                         labels = function(x) sub("\\.0$", "", as.character(x)),
                                         breaks = seq(0,1,by = .25)),
                     limits = c(0,48),
                     breaks = seq(0, 45, by = 15),
                     minor_breaks = seq(0, 45, by = 5)) +
  scale_x_date(date_breaks='1 month',minor_breaks='1 month',date_labels='%b',
               limits = c(as.Date('2021-05-18'),as.Date('2022-01-20'))) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~sitio+temporada,ncol=1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/figs/fenology_and_biomass_new_v1.png', height = 8, width = 7, scale=1)

data_plot |> 
  ggplot(aes(fecha,biomasa,color = 'AGB')) +
  geom_vline(data = fechas_feno, aes(xintercept = fecha), linetype = 'dotted', alpha = .6, color = 'navyblue') +
  geom_line(aes(fecha,NDVI*45,color = 'NDVI'), stat = "smooth", method = 'gam',
            linewidth = .8, alpha = 0.6) +
  geom_line(data=data_label,linewidth = .8, alpha = 0.7) +
  geom_point(size = 1) +
  # geom_boxplot(aes(group=fecha)) +
  geom_label(data = data_label, aes(fecha,biomasa,label = label), col = 'black',
             size = 3.5,alpha = .5,vjust = -.3,hjust = 1,parse = TRUE, label.size = 0) +
  geom_text(data = fechas_feno, aes(x = fecha_nueva, y = 47, label = label, hjust = .5), size = 3, color = 'black') +
  scale_color_manual(values = c("NDVI" = "darkolivegreen3", "AGB" = "darkorange"),name = NULL) +
  scale_y_continuous(name = "AGB",
                     sec.axis = sec_axis(~ . / 45, 
                                         name = "NDVI",
                                         labels = function(x) sub("\\.0$", "", as.character(x)),
                                         breaks = seq(0,1,by = .25)),
                     limits = c(0,48),
                     breaks = seq(0, 45, by = 15),
                     minor_breaks = seq(0, 45, by = 5)) +
  scale_x_date(date_breaks='1 month',minor_breaks='1 month',date_labels='%b',
               limits = c(as.Date('2021-05-01'),as.Date('2022-01-20'))) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~sitio+temporada,ncol=2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'),
        legend.position = 'bottom')

ggsave('output/figs/fenology_and_biomass_new_v2.png', height = 7, width = 9,scale=1)



data_biomasa |> 
  group_by(sitio,temporada) |> 
  reframe(n = n())
  
  