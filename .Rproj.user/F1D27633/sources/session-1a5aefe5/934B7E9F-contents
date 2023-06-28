

# Preliminares ------------------------------------------------------------

# Limipiar entorno de trabajo
rm(list=ls())

#setwd("C:/Users/jpber/OneDrive/Documents/Universidad/09 Matricula/Econometria espacial/Trabajo final/Datos/")

# dir_german 
setwd("C:/Users/germa/Desktop/UNAL/Unal_cursos/trabajo_final_econometria_espacial/Bases_datos/Datos_final")

funciones <- paste0(getwd(),"/Codigos")
cat(c('\14'))

# Librerias --------------------------------------------------------------

#Paquetes de proposito general
library(tidyverse)
library(stringr)
library(stargazer)

# Paquetes de graficacion
library(plotKML)
library(RColorBrewer)

# Paquetes de regresion
library(lmtest)
library(car)
library(nortest)
library(tseries)

# Paquetes espaciales
library(spdep)
library(rgeos)
library(rgdal)
library(spatialreg)
library(tmap)
library(sf)            # Para trabajar con datos vectoriales
library(cartogram)
library(spatmap)
library(maptools)
library(terra)         # Para trabajar con datos raster
library(classInt)
library(GWmodel)       # Modelo GWR
library(spgwr)

# Funciones espaciales ----------------------------------------------------

source(paste0(funciones,"/northarrow.R"))
source(paste0(funciones,"/scalebar.R"))
source(paste0(funciones,"/moranbi.test.R"))
source(paste0(funciones,"/moran.cluster.R"))
source(paste0(funciones,"/moran.bi.R"))
source(paste0(funciones,"/moran.cluster.R"))
source(paste0(funciones,"/localmoran.bi.R"))
source(paste0(funciones,"/moranbi.plot.R"))
source(paste0(funciones,"/quantile.e.R"))
source(paste0(funciones,"/correlogram.d.R"))
source(paste0(funciones,"/randomize_vector.R"))
source(paste0(funciones,"/spcorrelogram.bi.R"))
source(paste0(funciones,"/moranbi1.test.R"))
source(paste0(funciones,"/moran.bi1.R"))


# Carga del shapefile -----------------------------------------------------

bog_shp <- readOGR(dsn="datos_bogota2.gpkg")

# Cambio nombre de las variables
bog_shp@data <- bog_shp@data %>% 
  rename('porcentaje_informalidad'='datos_planos_bogotá_porcent_informalidad',
         'indice_transporte_informal'='datos_planos_bogotá_tasa_transp_inf',
         'indice_rango_ingresos'='datos_planos_bogotá_id_rango_ingresos',
         'media_duracion_viaje'='datos_planos_bogotá_media_duración_viaje',
         'porcentaje_pobreza_multidimensional'='datos_planos_bogotá_porcent_p_mult',
         'porcentaje_hacinamiento_mitigable'='datos_planos_bogotá_porcentaje_hacinamiento_mitigable',
         'porcentaje_pobreza_monetaria'='datos_planos_bogotá_porcent_p_mone',
         'porcentaje_vivienda_habitable_es_negocio'='datos_planos_bogotá_porcent_p_negocio_vivienda',
         'porcentaje_afiliacion_salud'='datos_planos_bogotá_porcent_afi_salud',
         'porcentaje_poblacion_joven'='datos_planos_bogotá_prct_joven',
         'porcentaje_poblacion_educacion_maximo_bachiller'='datos_planos_bogotá_pcrt_bachi',
         'porcentaje_edad_trabajar'='datos_planos_bogotá_pcrt_edad_trabajo',
         'porcentaje_desocupados'='datos_planos_bogotá_prct_desocupados',
         'gasto_educacion_mensual'='datos_planos_bogotá_gasto_edu_mens') 

# Mapas -------------------------------------------------------------------

# Mapa de cuartiles
tm_shape(bog_shp) +
  tm_fill("porcentaje_informalidad",n=4, style="quantile")  +
  tm_borders()

# Mapa de cuartiles
tm_shape(bog_shp) +
  tm_fill("porcentaje_pobreza_multidimensional",n=4, style="quantile",palette="Greens")  +
  tm_borders()

# Mapa de cuartiles
tm_shape(bog_shp) +
  tm_fill("porcentaje_pobreza_monetaria",n=4, style="quantile",palette="BuPu")  +
  tm_borders()

# Mapa de cuartiles
tm_shape(bog_shp) +
  tm_fill("porcentaje_poblacion_joven",n=4, style="quantile", palette = "Blues")  +
  tm_borders()

# Mapa de cuartiles
tm_shape(bog_shp) +
  tm_fill("porcentaje_poblacion_educacion_maximo_bachiller",n=4, style="quantile", palette = "Reds")  +
  tm_borders()

# Mapa de cuartiles
tm_shape(bog_shp) +
  tm_fill("porcentaje_afiliacion_salud",n=4, style="quantile", palette = "YlGn")  +
  tm_borders()

# Mapa de cuartiles
tm_shape(bog_shp) +
  tm_fill("gasto_educacion_mensual",n=4, style="quantile", palette = "OrRd")  +
  tm_borders()

tm_shape(bog_shp) +
  tm_fill("media_duracion_viaje",n=4, style="quantile", palette = "OrRd")  +
  tm_borders()

# Formula de los modelos --------------------------------------------------

formula <- porcentaje_informalidad ~  porcentaje_pobreza_multidimensional +
  porcentaje_poblacion_educacion_maximo_bachiller + porcentaje_afiliacion_salud

# Modelo OLS --------------------------------------------------------------

model_ols <- lm(formula, data=bog_shp@data); summary(model_ols)
stargazer(model_ols)

# Residuales del modelo
resid <- residuals(model_ols)

# AIC regresion lineal
AIC(model_ols)

# Normalidad de los residuales
jarque.bera.test(resid)
qqPlot(resid)

# Homoscedasticidad
bptest(model_ols) 

# Mapa residuales modelo OLS

bog_shp$lm_res <- residuals(model_ols)
pal2 <- colorRampPalette(c("red3", "wheat1", "blue3"))
spplot(bog_shp["lm_res"], col.regions = pal2(20))

# Prueba de autocorrelacion espacial en los residuales
list.queen <- poly2nb(bog_shp,row.names = bog_shp$NombreUP_1, queen=TRUE)
W          <- nb2listw(list.queen, style="W", zero.policy=TRUE)

moran.test(model_ols$residual, listw=W, randomisation = FALSE, alternative = "two.sided")
moran.plot(model_ols$residual, listw=W, labels=FALSE, quiet=TRUE,
           ylab="Residuales OLS espacialmente rezagados", xlab="Residuales OLS")

# Moran Bivariado ---------------------------------------------------------

# Porcentaje informalidad vs pobreza multidimensional
moran.bi(bog_shp$porcentaje_informalidad,bog_shp$porcentaje_pobreza_multidimensional,W,zero.policy =T) #t value
moranbi.test(bog_shp$porcentaje_informalidad,bog_shp$porcentaje_pobreza_multidimensional,W,999,graph=T,zero.policy =T,N=1000)
moranbi.plot(bog_shp$porcentaje_informalidad,bog_shp$porcentaje_pobreza_multidimensional, quiet =F,zero.policy =F,listw=W, 
             main="Bivariado de Moran - (W Queen)", 
             ylab="W*Porcentaje_Pobreza_Multidimensional", xlab="Porcentaje informalidad")

# Porcentaje informalidad vs porcenaje poblacion maximo bachiller
moran.bi(bog_shp$porcentaje_informalidad,bog_shp$porcentaje_poblacion_educacion_maximo_bachiller,W,zero.policy =T) #t value
moranbi.test(bog_shp$porcentaje_informalidad,bog_shp$porcentaje_poblacion_educacion_maximo_bachiller,W,999,graph=T,zero.policy =T,N=1000)
moranbi.plot(bog_shp$porcentaje_informalidad,bog_shp$porcentaje_poblacion_educacion_maximo_bachiller, quiet =F,zero.policy =F,listw=W, 
             main="Bivariado de Moran - (W Queen)", 
             ylab="W*porcentaje_poblacion_educacion_maximo_bachiller", xlab="Porcentaje informalidad")

# Modelos espaciales (SAR, SEM, SARAR, DURBIN) ----------------------------

# Test de independencia espacial

LMq1 <-lm.LMtests(model_ols, W, test="all") 
LMq1
t(sapply(LMq1, function(x) unlist (x[1:3])))
# Se tiene dos formas de leer los multiplicadores que permiten establecer el modelo. Se realiza el LMerr y LMlag, si ambos dan significativos 
# toca ir a los robustos, RLMerr y RLMlag. No rechaza la hipotesis nula de RLMerr, por lo cual no se debe usar este modelo, no es significativo
# Segun la prueba podemos utilizar un modelo SARMA o uno de SEM
# Se rechaza la hipotesis nula para el de error, no para el de LAG

# Modelo SAR
sar.upz<-lagsarlm(formula,data=bog_shp@data, W)
summary(sar.upz,Nagelkerke=T)
# De aca podemos ver el rho, el parametro junto a su p-valor. 

# Pseudo R2
sar.R2 = 1- var(sar.upz$residuals)/var(sar.upz$y)
sar.R2
AIC(sar.upz) # AIC OLS da menor

# Se deberian arreglar los problemas de autocorrelacion espacial. Se realiza el moran para el modelo SAR
moran.test(sar.upz$residual, listw=W, randomisation = FALSE, alternative = "two.sided") # Da significativo
# Se rechaza la hipotesis nula de no autocorrelacion
moran.plot(sar.upz$residual, listw=W, labels=FALSE, quiet=TRUE,
           ylab="Residuales SAR espacialmente rezagados", xlab="Residuales SAR")

# Modelo SEM

upz.sem<-errorsarlm(formula, data=bog_shp@data, listw=W, tol.solve=1.0e-30)
summary(upz.sem, Naguelkerke=T) # Lambda significativo, AIC menor que OLS y SAR
AIC(upz.sem)

sem.R2 = 1- var(upz.sem$residuals)/var(upz.sem$y)
sem.R2 # Pseudo R2 da mayor que SAR

moran.test(upz.sem$residual, listw=W, randomisation = TRUE, alternative = "two.sided")
# Tambien arregla el problema de autocorrelacion espacial
moran.plot(upz.sem$residual, listw=W, labels=FALSE, quiet=TRUE,
           ylab="Residuales SEM espacialmente rezagados", xlab="Residuales SEM")

# Uno ya deberia saber que modelo se ajusta mas a los datos, pero para el trabajo final si toca hacer una tabla con todos los 
# modelos.

# Modelo SARAR

upz.sarar<-sacsarlm(formula, data=bog_shp@data, listw=W,tol.solve=1.0e-30)
summary(upz.sarar, Naguelkerke=T) # Rho y Lambda significativos

AIC(upz.sarar) # AIC menor de todos

sarar.R2 = 1- var(upz.sarar$residuals)/var(upz.sarar$y)
sarar.R2 # El mayor pseudo R2 de todos

moran.test(upz.sarar$residual, listw=W, randomisation = TRUE, alternative = "two.sided")
moran.plot(upz.sarar$residual, listw=W, labels=FALSE, quiet=TRUE,
           ylab="Residuales SARAR espacialmente rezagados", xlab="Residuales SARAR") 
# Tambien arregla el problema de autocorrelacion espacial

# Modelo SDM 

upz.sdm<-lagsarlm(formula,data=bog_shp@data, W, type="mixed",tol.solve=1.0e-30)
# Toca ponerle el type = "mixed"
summary(upz.sdm)
AIC(upz.sdm)

sdm.R2 = 1- var(upz.sdm$residuals)/var(upz.sdm$y)
sdm.R2

moran.test(upz.sdm$residual, listw=W, randomisation = TRUE, alternative = "two.sided")
moran.plot(upz.sdm$residual, listw=W, labels=FALSE, quiet=TRUE,
           ylab="Residuales SDM espacialmente rezagados", xlab="Residuales SDM") 
# Tambien corrige problemas de autocorrelacion espacial

# Efecto total, directos e indirectos -------------------------------------------

# Pre simulación 
rho       <- sar.upz$rho                                        #Rho estimado del modelo SAR-SLM
beta_hat  <- coef(sar.upz)[-1]                                  #Parámetros estimados
A         <- invIrW(W, rho = rho)                                      #(I - rho*W)^{-1}
X         <- cbind(1, bog_shp$porcentaje_pobreza_multidimensional, bog_shp$porcentaje_poblacion_educacion_maximo_bachiller,
           bog_shp$porcentaje_afiliacion_salud)                #Matriz de variables observables
y_hat_pre <- A %*% crossprod(t(X), beta_hat)                   # y gorro (1)

# Matriz S
S = A * beta_hat["porcentaje_pobreza_multidimensional"]

# Efecto total
Total <- (crossprod(rep(1, nrow(S)), S) %*% rep (1, nrow(S))) / nrow(X)

# Efecto directo promedio (Average Impact of Density)
ADI <- sum(diag(S)) / nrow (X) # Es el efecto sobre las propias regiones si cada una aumenta en 1 su densidad.

# Efecto indirecto promedio (Average indirect impact of Density)
AID <- Total - ADI

# Simulación --------------------------------------------------------------
col_new <- bog_shp                                                                        # Copia del dataset
# Cambiar el valor de pobreza multidimensional de la UPZ con mayor porcentaje al porcentaje de la UPZ con menor porcentaje de pobreza
# multidimensional
col_new@data[col_new@data$UPZEM == 811, "porcentaje_pobreza_multidimensional"] <- min(col_new@data$porcentaje_pobreza_multidimensional)  

X_d <- cbind(1, col_new$porcentaje_pobreza_multidimensional, col_new$porcentaje_poblacion_educacion_maximo_bachiller,
             col_new$porcentaje_afiliacion_salud)                                         # Matriz de variables observables con cambio
y_hat_post <- A %*% crossprod(t(X_d), beta_hat)                                           # y gorro (2)

delta_y <- y_hat_post - y_hat_pre                              #Diferencia entre y gorro (1) y y gorro (2)
col_new$delta_y <- delta_y                                     #Se agrega variable al dataset

summary(delta_y)
sum(delta_y) # Efecto de reducir la informalidad en un 20%

#  Mapa simulacion -------------------------------------------------------------------

pal5 <- brewer.pal(6, "Spectral")
cats5 <- classIntervals(col_new$delta_y, n = 5, style = "jenks")
colors5 <- findColours(cats5, pal5)
plot(col_new, col = colors5)
legend("topleft", legend = round(cats5$brks, 2), fill = pal5, bty = "n")


# GWR ---------------------------------------------------------------------

# Estimación del ancho de banda. Se prueban distribuiones bicuadrada y gaussiana, fija y adaptativa.

## Gaussiano 
GWRbandwidth1 <- gwr.sel(formula, data = bog_shp, gweight = gwr.Gauss, adapt = T)
GWRbandwidth2 <- gwr.sel(formula, data = bog_shp, gweight = gwr.Gauss, adapt = F)

# Bi-cuadrada
GWRbandwidth3 <- gwr.sel(formula, data = bog_shp, gweight = gwr.bisquare, adapt = T)
GWRbandwidth4 <- gwr.sel(formula, data = bog_shp, gweight = gwr.bisquare, adapt = F)

# De acuerdo al resultado, el menor CV se logra con un kernel gaussiano fijo. Tomando el valor del ancho de banda
# gaussiano fijo se ubica en una relación de vecinos de 4, equivalentes al 4.25% de las unidades de análisis.

# Estimación del model GWR - dos formas (la segunda incluye información de los SE de los estimadores).

gwr.model = gwr(formula, data = bog_shp, adapt = GWRbandwidth1)
gwr.model

gwr.model2 = gwr(formula,
                 data = bog_shp,
                 adapt=GWRbandwidth1,
                 hatmatrix=TRUE,
                 se.fit=TRUE)  
gwr.model2 

# Pruebas de la GWR (Leung, Mei, and Zhang, 2000)

# Test de bondad del ajuste de los modelos
# Ho: Los modelos OLS y GWR no son muy diferentes, H1: Modelo GWR es mejor.

LMZ.F2GWR.test(gwr.model2)

# Test de significancia de la variación de los parámetros locales estimados
# Ho: Variación insignificativa y por tanto con parámetros constantes, Ha: Valores locales significativos

LMZ.F3GWR.test(gwr.model2) # Las variables X rechazan la hipótesis nula; no así el intercepto.

# Revisamos los elementos contenidos en cada salida del modelo y conocer la forma de extracción.

names(gwr.model$SDF)
names(gwr.model2$SDF)

# Mapas de los betas de Pobrea multidimensional

b1 <- gwr.model$SDF$porcentaje_pobreza_multidimensional
brks <- c(min(b1), mean(b1)-sd(b1), mean(b1), mean(b1)+sd(b1), max(b1))
cols <- c("steelblue4", "lightskyblue", "thistle1", "plum3")
x11()
plot(bog_shp, col=cols[findInterval(b1, brks)])
title(main="Coeficientes locales Pobreza mulitimendional sobre informalidad")
legend("topleft", legend=leglabs(round(brks, 2)), fill=cols, bty="n")

# Mapas de los betas Educación bachiller

b1 <- gwr.model$SDF$porcentaje_poblacion_educacion_maximo_bachiller
brks <- c(min(b1), mean(b1)-sd(b1), mean(b1), mean(b1)+sd(b1), max(b1))
cols <- c("steelblue4", "lightskyblue", "thistle1", "plum3")
x11()
plot(bog_shp, col=cols[findInterval(b1, brks)])
title(main="Coeficientes locales nivel de educación bachiller sobre informalidad")
legend("topleft", legend=leglabs(round(brks, 2)), fill=cols, bty="n")

# Mapas de los betas de salud

b1 <- gwr.model$SDF$porcentaje_afiliacion_salud
brks <- c(min(b1), mean(b1)-sd(b1), mean(b1), mean(b1)+sd(b1), max(b1))
cols <- c("steelblue4", "lightskyblue", "thistle1", "plum3")
x11()
plot(bog_shp, col=cols[findInterval(b1, brks)])
title(main="Coeficientes locales Afiliación a salud sobre informalidad")
legend("topleft", legend=leglabs(round(brks, 2)), fill=cols, bty="n")