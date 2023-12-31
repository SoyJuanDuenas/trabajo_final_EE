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
library(writexl)
library(readxl)



#Ahora vamos a importar nuestros Datasets

empresas_rama<-readOGR(dsn="C:/Users/PC/Desktop/Trabajo final EE/Datasets/Distribución de las empresas por rama Industrial por UPZ/Empresas_Rama.shp")
t_lab_fuerte <- readOGR("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Tasa de Informalidad Laboral Fuerte por UPZ/Tasa_Laboral_Fuerte_UPZ-polygon.shp")
mov_shapefile <- readOGR("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Encuesta de Movilidad/ZONAS/ZONAS/UTAM.shp")
mov_hogares_csv <- read.csv("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Encuesta de Movilidad/Archivos CSV/HogaresEODH2019.csv", sep = ";")
mov_personas_csv <- read.csv("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Encuesta de Movilidad/Archivos CSV/PersonasEODH2019.csv", sep = ";")
mov_duracion_csv <- read.csv("C:/Users/PC/Desktop/Trabajo final EE/Datasets/Encuesta de Movilidad/Archivos CSV/Aux_DuraciónEODH2019.csv", sep = ";")

print(dim(mov_shapefile@data))

#vamos a unir las bases de datos de la encuesta de movilidad

mov_hogares_csv <- rename(mov_hogares_csv, id_hogar = Id_Hogar)

mov_csv_1 <- merge(mov_hogares_csv, mov_personas_csv, by = "id_hogar")
mov_csv <- merge(mov_csv_1, mov_duracion_csv, by="id_hogar")
mov_csv <- subset(mov_csv, !duplicated(mov_csv$id_hogar))

print(dim(mov_shapefile@data))

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

print(dim(mov_shapefile@data))

#Eliminamos datos no pertenecientes a Bogotá, estandarizando UTAM para posterior merge con UPZ

mov_shapefile@data <- subset(mov_shapefile@data, MUNNombre == "BOGOTA")

print(dim(mov_shapefile@data))

#Creamos una nueva columna en donde mantenga el número de la UTAM y eliminamos UPR

mov_shapefile@data$codigo <- substr(mov_shapefile@data$UTAM, start = 5, stop = nchar(mov_shapefile@data$UTAM))
mov_shapefile@data <- subset(mov_shapefile@data, codigo != "")

print(dim(mov_shapefile@data))

#Hacemos lo mismo para empresas_rama con el fin de tener la llave del joint en un mismo termino

empresas_rama@data$codigo <- substr(empresas_rama@data$UPlCodigo, start = 4, stop = nchar(empresas_rama@data$UPlCodigo))

#Ahora vamos por medio de indices a generalizar datos desde hogares hasta UPZ

promedio_duración_mov <- aggregate(duracion ~ UTAM, data = mov_shapefile@data, FUN = mean)

print(dim(promedio_duración_mov))

#ahora indice de ingresos

ingresos_mov <- aggregate(id_rango_ingresos ~ UTAM, data = mov_shapefile@data, FUN = mean)
ingresos_mov$id_rango_ingresos = ingresos_mov$id_rango_ingresos*(1/10)

print(dim(ingresos_mov))

#Ahora el indice de transporte informal

num_observaciones_por_UPZ <- as.data.frame(table(mov_shapefile@data$UTAM))
frecuencia_a_pie_por_UPZ <- as.data.frame(table(subset(mov_shapefile@data, modo_principal == "A pie")$UTAM))
frecuencia_a_bici_por_UPZ <- as.data.frame(table(subset(mov_shapefile@data, modo_principal == "Bicicleta")$UTAM))

base_1 <- merge(num_observaciones_por_UPZ, frecuencia_a_bici_por_UPZ, by = "Var1")
I_trans_informal <- merge(base_1, frecuencia_a_pie_por_UPZ, by = "Var1")
colnames(I_trans_informal) <- c("UTAM", "n", "bici", "pie")
I_trans_informal$tasa = (I_trans_informal$bici + I_trans_informal$pie)/ I_trans_informal$n

print(dim(I_trans_informal))

#Agregamos los datos


Df_1 = merge(I_trans_informal, ingresos_mov, by = "UTAM")
datos_mov_UPZ = merge(Df_1, promedio_duración_mov, by = "UTAM")
datos_mov_UPZ <-datos_mov_UPZ[, c("UTAM",
                                  "tasa",
                                  "id_rango_ingresos",
                                  "duracion")]
datos_mov_UPZ$UTAM <- as.character(datos_mov_UPZ$UTAM)

datos_mov_UPZ$codigo <- substr(datos_mov_UPZ$UTAM, start = 5, stop = nchar(datos_mov_UPZ$UTAM))

#Hacemos unas ediciones al .shp antes de hacer el merge

mov_shapefile@data <- subset(mov_shapefile@data, !duplicated(UTAM))
print(dim(mov_shapefile@data))
mov_shapefile@data <- mov_shapefile@data[, c("UTAM",
                                             "UTAMNombre")]

mov_shapefile@data <- merge(mov_shapefile@data, datos_mov_UPZ, by = "UTAM")
print(dim(mov_shapefile@data))

#IMPORTAMOS DATOS INCLUYENDO MULTIPROPOSITO

datos <- read_excel("C:/Users/PC/Desktop/datos movilidad.xlsx")
t_lab_fuerte@data <- merge(t_lab_fuerte@data, datos, by = "UPZZONA")

#Exportamos shp
t_lab_fuerte_p <- st_as_sf(t_lab_fuerte)
drv <- st_drivers("ESRI Shapefile")
driver <- drv$driver[1]
st_write(t_lab_fuerte_p, "C:/Users/PC/Desktop/", driver = "ESRI Shapefile")
