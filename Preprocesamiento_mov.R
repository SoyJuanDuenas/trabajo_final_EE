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

#ahora vamos a extraer las columnas de nuestro interes

mov_csv <- mov_csv[, c("modo_principal",
                       "Utam",
                       "id_hogar",
                       "duracion",
                       "id_rango_ingresos")]

mov_shapefile@data <-mov_shapefile@data[, c("MUNCodigo",
                                            "MUNNombre",
                                            "UTAM",
                                            "UTAMNombre")]

#ahora debemos cambiar el sujeto de análisis de hogares a UTAM haciendo diferentes agregaciones
#hacemos un promedio simple para hallar la duración promedio de viaje en cada UPZ

#Hacemos el joint con el .shp a partir de la variable UTAM (Unidad Territorial de Análisis de Transporte)

mov_csv <- rename(mov_csv, UTAM = Utam)
mov_shapefile@data <- merge(mov_shapefile@data, mov_csv, by = "UTAM")


#Eliminamos datos no pertenecientes a Bogotá, estandarizando UTAM para posterior merge con UPZ

mov_shapefile@data <- subset(mov_shapefile@data, MUNNombre == "BOGOTA")

#Creamos una nueva columna en donde mantenga el número de la UTAM y eliminamos UPR

mov_shapefile@data$codigo <- substr(mov_shapefile@data$UTAM, start = 5, stop = nchar(mov_shapefile@data$UTAM))
mov_shapefile@data <- subset(mov_shapefile@data, codigo != "")

#Hacemos lo mismo para empresas_rama con el fin de tener la llave del joint en un mismo termino

empresas_rama@data$codigo <- substr(empresas_rama@data$UPlCodigo, start = 4, stop = nchar(empresas_rama@data$UPlCodigo))

#Ahora vamos por medio de indices a generalizar datos desde hogares hasta UPZ

promedio_duración_mov <- aggregate(duracion ~ UTAM, data = mov_shapefile@data, FUN = mean)

#ahora indice de ingresos

ingresos_mov <- aggregate(id_rango_ingresos ~ UTAM, data = mov_shapefile@data, FUN = mean)
ingresos_mov$id_rango_ingresos = ingresos_mov$id_rango_ingresos*(1/10)

#Ahora el indice de transporte informal

num_observaciones_por_UPZ <- as.data.frame(table(mov_shapefile@data$UTAM))
frecuencia_a_pie_por_UPZ <- as.data.frame(table(subset(mov_shapefile@data, modo_principal == "A pie")$UTAM))
frecuencia_a_bici_por_UPZ <- as.data.frame(table(subset(mov_shapefile@data, modo_principal == "Bicicleta")$UTAM))

base_1 <- merge(num_observaciones_por_UPZ, frecuencia_a_bici_por_UPZ, by = "Var1")
I_trans_informal <- merge(base_1, frecuencia_a_pie_por_UPZ, by = "Var1")
colnames(I_trans_informal) <- c("UTAM", "n", "bici", "pie")
I_trans_informal$tasa = (I_trans_informal$bici + I_trans_informal$pie)/ I_trans_informal$n



