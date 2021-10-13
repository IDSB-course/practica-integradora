library(tidyverse)
library(lubridate)
library(skimr)
library(rio)
library(naniar)
library(jsonlite)
library(ggthemes)
library(parsedate)
library(boot)
library(knitr)
library(janitor)
library(leaps)
library(Metrics)
library(sf)
library(ggmap)



# 0 read data
temp = tempfile()
download.file('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte/bicicletas-publicas/recorridos-realizados-2018.zip',
              temp)

ecobicis_df <- read_csv(unz(temp,'recorridos-realizados-2018.csv')) %>% 
  clean_names()

#1 bind data
usuarios_list <- map(2015:2018,~ read_csv(paste0(
  "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/usuarios-ecobici-",
  .x, ".csv"))) 


  
usuarios_df <- do.call('rbind', usuarios_list) 

rm(usuarios_list)
gc()

# EDA

glimpse(ecobicis_df)
sample_n(ecobicis_df, 10)

glimpse(usuarios_df)
sample_n(usuarios_df,10)


# Stats
summary(ecobicis_df)
summary(usuarios_df)


#2 univariate plotshttp://localhost:8787/graphics/plot_zoom_png?width=663&height=646

ggplot(ecobicis_df)+
  geom_bar(aes(x= as.factor(genero_usuario)))

# columna generada
ecobicis_df$tiempo <- as.numeric(ecobicis_df$fecha_destino_recorrido - ecobicis_df$fecha_origen_recorrido)


ggplot(ecobicis_df)+
  geom_histogram(aes(x = tiempo),bins = 60, col = 'white')+
  scale_x_log10()


#3 time series plots

#viajes por día

ecobicis_df %>% 
  group_by(day = as.Date(fecha_origen_recorrido)) %>% 
  summarise(operaciones = n()) %>% 
  ggplot()+
  geom_line(aes(x = day, y = operaciones), col = 'steelblue')+
  theme_bw()

#bivariate plots
# operaciones por mes

ecobicis_df %>%
  group_by(mes = month(fecha_origen_recorrido, label = T, abbr =F)) %>%
  summarise(operaciones = n()) %>% 
  ggplot()+
  geom_col(aes(x = mes, y = operaciones), alpha=.8)+
  ggtitle("Número de operaciones por mes")+
  xlab("Meses")+
  ylab("# de Operaciones")+
  theme_bw()








#4 duraciones de los recorridos por semana

ecobicis_df$dia_semana <-  ecobicis_df$fecha_origen_recorrido %>% wday(label = T, abbr = F)

ggplot(ecobicis_df %>% sample_n(2e4),
       aes(x = dia_semana, y =tiempo))+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(aes(col= dia_semana), alpha = .15)+
  coord_cartesian(ylim=c(0,75))+
  theme_bw()+
  ggtitle("Duración de viaje por dia de la semana")+
  xlab("Día")+
  ylab("Duración del recorrido (Minutos)")

#operaciones por día de la semana
  
ecobicis_df %>%
  group_by(dia_semana) %>%
  summarise(operaciones = n()) %>% 
  ggplot()+
  geom_col(aes(x = dia_semana, y = operaciones), alpha=.8)+
  ggtitle("Número de operaciones por día de la semana")+
  theme_bw()

#5 Outliers.
#histograma de outliers

ecobicis_df %>% 
  filter(tiempo > 1.5*IQR(tiempo, na.rm = T)+
           quantile(tiempo,.75, na.rm = T)) %>% 
  ggplot()+
  geom_histogram(aes(x =tiempo), col = " White", binwidth = 3)+
  theme_bw()+
  scale_x_continuous(breaks = seq(60,180,20))+
  ggtitle("Histograma de Outliers")+
  xlab("Duración del recorrido (Minutos)")+
  ylab("Número de operaciones")


#6 inference

# conf ints int por días de semana


operaciones_dia <- ecobicis_df %>%
  group_by(fecha = as.Date(fecha_origen_recorrido)) %>%
  summarise(registros = n()) %>% 
  mutate(dia_semana = wday(fecha,label = T,abbr = F))


operaciones_dia_summ <- operaciones_dia %>% 
  group_by(dia_semana) %>% 
  summarise(media = mean(registros),
            int_min = t.test(registros,conf.level = .95)$conf.int[1],
            int_max = t.test(registros,conf.level = .95)$conf.int[2])

ggplot(operaciones_dia_summ, aes(x = fct_rev(dia_semana))) +
  geom_segment( aes(xend=dia_semana, y=int_min, yend=int_max), color="grey") +
  geom_point( aes(y=int_min), color=rgb(0.7,0.2,0.1,0.5), size=5 ) +
  geom_point( aes( y=int_max), color=rgb(0.2,0.7,0.1,0.5), size=5 ) +
  geom_point(aes(y= media), size = 2, shape = 1 )+
  coord_flip()+
  theme_tufte() +
  theme(
    legend.position = "none",
  ) +
  xlab("Día de la semana") +
  ylab("Cantidad de Registros")+
  ggtitle("Intervalos de confianza de registros por día de semana")

#7 observemos todas las estaciones en un mapa!


estaciones_df <- ecobicis_df %>% 
  select(id_estacion_origen,lat_estacion_origen, long_estacion_origen) %>% 
  unique() %>% 
  filter(complete.cases(.))


bbox <- c(min(estaciones_df$long_estacion_origen) - .005,
          min(estaciones_df$lat_estacion_origen) - .005,
          max(estaciones_df$long_estacion_origen) + .005,
          max(estaciones_df$lat_estacion_origen) + .005)

ggmap(get_stamenmap(bbox, zoom = 13))+
  geom_point(data = estaciones_df, aes( x=long_estacion_origen, y =lat_estacion_origen  ))


#8 & 9
# Visual de usuarios_df piramides poblacionales

usuarios_plot <- usuarios_df %>% 
  filter(usuario_sexo %in% c("M","F")) %>% 
  mutate(rango_etario = cut(usuario_edad,breaks = seq(15,100,5))) %>% 
  filter(!is.na(rango_etario)) %>% 
  group_by(usuario_sexo, rango_etario) %>% 
  summarise(total = n())


usuarios_plot$total[usuarios_plot$usuario_sexo == "M"] <-
  -1 * usuarios_plot$total[usuarios_plot$usuario_sexo == "M"]

ggplot(usuarios_plot, aes(x = rango_etario, y = total, fill = usuario_sexo))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(-24e3,20e3,4e3),
                     labels =c(seq(24e3,0,-4e3), seq(4e3,20e3,4e3)) )+
  coord_flip()+
  theme_bw()+
  ggtitle("Piramide poblacional de usuarios de ECOBICI")+
  ylab("Número de usuarios")+
  xlab("Rango etario (años)")+
  labs(fill = "Sexo")






#10 join de usuarios

bicis_df_joined <-  left_join(ecobicis_df, usuarios_df,by = c("id_usuario" = "usuario_id")) %>% 
  filter(!is.na(usuario_edad),
         !usuario_edad > 100)



sl_data <- bicis_df_joined %>% 
  filter(id_estacion_origen == 177,
         id_estacion_destino == 5,
         !is.na(tiempo)) %>% 
  mutate(inicio_time = hour(fecha_origen_recorrido) * 60 + minute(fecha_origen_recorrido)) %>% 
  select(genero_usuario, dia_semana, usuario_edad, hora_alta, inicio_time, tiempo) %>% 
  mutate(hora_alta = as.numeric(hora_alta))



# 11 train test split
set.seed(0)
keep <- sample(1:nrow(sl_data),size = floor(.8*nrow(sl_data)),replace = F)
train_df <- sl_data[keep,]
test_df <- sl_data[-keep,]


# bss

bss <- regsubsets(tiempo~., train_df , method = 'exhaustive')

summary(bss) 
summary(bss)$adjr2 %>% which.max()
summary(bss)$adjr2[summary(bss)$adjr2 %>% which.max()]
summary(bss)$bic %>% which.min()
summary(bss)$bic[summary(bss)$bic %>% which.min()]

#train model

lin_model <- lm(tiempo~., data = train_df)
summary(lin_model)

#test performance

preds <- predict(lin_model, test_df)



rmse(test_df$tiempo, preds)



