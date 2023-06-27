#Iniciamos instalando las librerias adecuadas

library(spdep)
library(rgeos)
library(rgdal)
library(spatialreg)
library(tmap)
library(sf)
library(ggplot2)
library(plotKML)
library(RColorBrewer)
library(cartogram)
library(stringr)
library(spatmap)
library(maptools)
library(dplyr)


#Ahora vamos a importar nuestros Datasets

empresas_rama<-readOGR(dsn="C:/Users/PC/Desktop/Trabajo final EE/Datasets/Distribución de las empresas por rama Industrial por UPZ/Empresas_Rama.shp")
t_lab_fuerte <- readOGR("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Tasa de Informalidad Laboral Fuerte por UPZ/Tasa_Laboral_Fuerte_UPZ-polygon.shp")
mov_shapefile <- readOGR("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Encuesta de Movilidad/ZONAS/ZONAS/UTAM.shp")
mov_hogares_csv <- read.csv("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Encuesta de Movilidad/Archivos CSV/HogaresEODH2019.csv", sep = ";")
mov_personas_csv <- read.csv("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Encuesta de Movilidad/Archivos CSV/PersonasEODH2019.csv", sep = ";")
mov_duracion_csv <- read.csv("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Encuesta de Movilidad/Archivos CSV/Aux_DuraciónEODH2019.csv", sep = ";")

#vamos a unir las bases de datos de la encuesta de movilidad

mov_hogares_csv <- rename(mov_hogares_csv, id_hogar = Id_Hogar)

mov_csv_1 <- merge(mov_hogares_csv, mov_personas_csv, by = "id_hogar")
mov_csv <- merge(mov_csv_1, mov_duracion_csv, by="id_hogar")
mov_csv <- subset(mov_csv, !duplicated(mov_csv$id_hogar))

# Agrupamos tanto dificultad medios transporte como dificultad fisica, dado que
# no nos interesa si el tipo de dificultad sino solamente si el usuario ha tenido al menos una dificultad

mov_csv <- mutate(mov_csv, dificultad_fisica = 0 + p8_id_dificultad_fisica_1 + p8_id_dificultad_fisica_2 + p8_id_dificultad_fisica_3 + p8_id_dificultad_fisica_4 + p8_id_dificultad_fisica_5 + p8_id_dificultad_fisica_6 + p8_id_dificultad_fisica_7)
mov_csv <- mutate(mov_csv, dificultad_medio = 0 + p9_id_dificultad_medios_transporte_1 + p9_id_dificultad_medios_transporte_2 + p9_id_dificultad_medios_transporte_3 + p9_id_dificultad_medios_transporte_4 + p9_id_dificultad_medios_transporte_5 + p9_id_dificultad_medios_transporte_6 + p9_id_dificultad_medios_transporte_7 + p9_id_dificultad_medios_transporte_8 + p9_id_dificultad_medios_transporte_9 + p9_id_dificultad_medios_transporte_10 + p9_id_dificultad_medios_transporte_11 + p9_id_dificultad_medios_transporte_12 + p9_id_dificultad_medios_transporte_13) 

#Los usuarios que no tienen niguna dificultad están señalados como NA, debemos rellenar este hueco

mov_csv$dificultad_fisica <- ifelse(is.na(mov_csv$dificultad_fisica), 0, mov_csv$dificultad_fisica)
mov_csv$dificultad_medio <- ifelse(is.na(mov_csv$dificultad_medio), 0, mov_csv$dificultad_medio)
#ahora vamos a extraer las columnas de nuestro interes

mov_csv <- mov_csv[, c("modo_principal",
                       "Utam",
                       "id_hogar",
                       "Latitud",
                       "Longitud",
                       "duracion",
                       "dificultad_fisica",
                       "dificultad_medio",
                       "id_rango_ingresos")]

#Hacemos el joint con el .shp a partir de la variable UTAM (Unidad Territorial de Análisis de Transporte)

mov_csv <- rename(mov_csv, UTAM = Utam)
mov_shapefile@data <- merge(mov_shapefile@data, mov_csv, by = "UTAM")

#Ahora nece

mov_csv %>% 
  group_by(modo_principal) %>%
  summarise(total = n())

mov_csv %>% 
  group_by(UTAM) 
