## librerías utilizadas
library(rjson)
library(RJSONIO)
library(RCurl)
library(plyr)
library(stringr)
library(data.table)
library(caret)
library(dplyr)
## Funciones
##-------------------------------------
## get_directions
##-------------------------------------
## Calcula direcciones entre dos puntos
## esto está útil para graficarlos recorridos una vez que los tengamos ;)
get_directions <- function(origen, destino, mode = "driving"){
    base        <- "https://maps.googleapis.com/maps/api/directions/json?"
    origen      <- paste0("origin=",origen)
    destino     <- paste0("destination=",destino)
    mode        <- paste0("mode=",mode)
    key         <- "key=AIzaSyAkW2m1J6oq_UblEtwhzVB9EYmz7Ayc4k0"
    query       <- paste(base,origen,destino,mode,key,sep = "&")
    route       <- fromJSON(getURL(query))$routes[[1]]$legs
    steps       <- route[[1]]$steps
    list(
        "durations"  = ldply(steps,function(t)t <- t$duration$text)[,1],
        "distance"   = ldply(steps,function(t)t <- t$distance$text)[,1],
        "start_loc"  = ldply(steps,function(t)t <- t$start_location),
        "end_loc"    = ldply(steps,function(t)t <- t$end_location)
        )
}
##-------------------------------------
## get_distance
##-------------------------------------
## Calcula distancia entre dos puntos
## falta hacer para distancia entre n puntos
get_distance <- function(origen, destino){
    base        <- "https://maps.googleapis.com/maps/api/distancematrix/json?"
    origen      <- paste0("origins=",origen)
    destino     <- paste0("destinations=",destino)
    key         <- "key=AIzaSyAkW2m1J6oq_UblEtwhzVB9EYmz7Ayc4k0"
    query       <- paste(base,origen,destino,key,sep = "&")
    results     <- fromJSON(getURL(query))
    distance    <- results$rows[[1]]$elements[[1]]$distance$text
    duration    <- results$rows[[1]]$elements[[1]]$duration$text
    list("distance"=distance,"duration"=duration)
}
##-------------------------------------
## inf_dist
##-------------------------------------
## Determina los puntos que se encuentran a
## una distancia medida en norma infinito
## menor a cierto parámetro
inf_dist <- function(coords, array, dist){
    array.long.near <- array[
                             abs(array$long -
                                     coords[2]) <
                                 dist, ]
    array.lat.near  <- array.long.near[
                                       abs(array.long.near$lat -
                                               coords[1]) <
                                           dist, ]
    array.lat.near
}
##-------------------------------------
## get_min_resc
##-------------------------------------
## Determina los vehículos de rescate
## que se encontraban a una distancia mínima del
## punto del accidente.
get_min_resc  <- function(accident, rescuers){
    distances <- ldply(rescuers, function(t)t <- get_distance(t,accident)$distance)[,1]
    distances[which(distances)==min(distances)]
}
##################################################################
##################################################################
##################################################################
##################################################################

####################### lectura de datos #########################
## Leer datos referentes a recorridos de patrullas y accidentes
##################################################################
recorridos <- read.csv("../../angeles_verdes/data/recorridos.csv", sep = ",")
## Accidentes
accidentes <- read.csv("../../angeles_verdes/data/angels_clean.csv",
                       sep = ",",
                       encoding = "UTF-8")
setnames(accidentes, tolower(names(accidentes)))
accidentes$fechaservi <- as.Date(accidentes$fechaservi)

#################### Preparativos Análisis #######################
## Se dividirán los datos en prueba y entrenamiento
##################################################################
set.seed(123454321)
trainIndex <- createDataPartition(accidentes$servicio,
                                  p = .8,
                                  list = FALSE,
                                  times = 1)
acc_train <- accidentes[trainIndex,]

############################ Análisis ############################
#### Ingeniería de variables
### Los pasos a realizar son los siguientes
## Análisis y tratamiento de valores faltantes
## Análisis de varianza de predictores
## Análisis y detección de predictores sesgados
## Análisis y detección de datos aberrantes
## Análisis y detección de correlación en los predictores
#### Selección de variables
## Identificación de posible separación de clases
##################################################################
## Para llevara a cabo la ingeniería de variables
## obtendremos 10 muestras equivalentes al 30% de los
## datos. De esta manera, 
trainIndex <- createDataPartition(acc_train$servicio,
                                  p = .3,
                                  list = FALSE,
                                  times = 10)
acc_exp <- accidentes[trainIndex[,1],]
###------------------------------------------------------
## Análisis y tratamiento de datos faltantes
###------------------------------------------------------
## Análisis de varianza de predictores
###------------------------------------------------------
nzv <- nearZeroVar(acc_train, saveMetrics= TRUE)
## el número de extranjeros y opinión tienen varianza casi cero.
acc_exp <- select(acc_exp, -matches("opinion","totextranj"))
###------------------------------------------------------
## Análisis y tratamiento de datos correlacionados
###------------------------------------------------------
## Existen varios predictores que se encuentran trivialmente correlacionados.

