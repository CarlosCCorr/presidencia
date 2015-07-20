## librerías utilizadas
library(rjson)
library(RJSONIO)
library(RCurl)
library(plyr)
library(stringr)
library(data.table)
library(caret)
library(dplyr)
library(lubridate)
library(geosphere)
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
## near_accident_id
##-------------------------------------
## Determina los k puntos que se encuentran
## más cerca de un accidente dado un intervalo
## de tiempo.
near_accident_id<- function(accidente, recorridos, intervalo=2.1e4){
    ## Esto me da a los recorridos que estan en una vecindad
    ## de tamaño intervalo del accidente.
    recorridos <-
        recorridos[recorridos$tiempo <= accidente$fecha + intervalo &
                       recorridos$tiempo >= accidente$fecha - intervalo, ]
    ## Es necesario obtener la distancia carretera entre el accidente y los
    ## recorridos 
    coords_accidente <- accidente[,c(12,13)]
    coords_recorrido <- recorridos[,c(3,4)]
    ## para esto usamos la distancia geodésica.
    distancia_geo  <-
        apply(coords_recorrido[,c(2,1)],1,function(t)t <-
            distCosine(t, coords_accidente[,c(2,1)])/1e3)
    ## Obtenemos los más cercanos
    recorridos$distancia_geo <- distancia_geo
    recorridos <- data.table(recorridos)
    dist_accident_id <- recorridos[,min(distancia_geo), by = patrulla]
    setNames(dist_accident_id,
             c("patrulla","dist"))
}
##-------------------------------------
## near_accident
##-------------------------------------
## Determina los k puntos que se encuentran
## más cerca de un accidente dado un intervalo
## de tiempo.
near_accident <- function(accidente,
                          recorridos,
                          intervalo = 2.1e4,
                          k = 3){
    ## Esto me da a los recorridos que estan en una vecindad
    ## de tamaño intervalo del accidente.
    recorridos <-
        recorridos[recorridos$tiempo <= accidente$fecha + intervalo &
                       recorridos$tiempo >= accidente$fecha - intervalo, ]
    ## Es necesario obtener la distancia carretera entre el accidente y los
    ## recorridos 
    coords_accidente <- accidente[,c(12,13)]
    coords_recorrido <- recorridos[,c(3,4)]
    ## para esto usamos la distancia geodésica.
    distancia_geo  <-
        apply(coords_recorrido[,c(2,1)],1,function(t)t <-
            distCosine(t, coords_accidente[,c(2,1)])/1e3)
    ## Obtenemos los más cercanos
    recorridos$distancia_geo <- distancia_geo
    k_vecinos <- head(recorridos[order(distancia_geo),], k)
    ## continuamos con la distancia carretera
    distancia_carr <-
        apply(k_vecinos[,c(3,4)],
              1,
              function(t)t <-{
                  get_distance(
                      paste(as.character(t),
                            collapse = ","),
                      paste(
                          as.character(coords_accidente),
                          collapse = ",")
                      )$distance
              }
              )
    k_vecinos$distancia_carr <- distancia_carr
    k_vecinos
}
##-------------------------------------
## k_near_accident_id
##-------------------------------------
## Determina los k  puntos que se encuentran
## más cerca de cada uno de los puntos por id
## de tiempo.
k_near_accident_id <- function(accidentes,
                               recorridos,
                               intervalo = 2.1e4){
    dlply(accidentes,1,
          function(t)t <-
              near_accident_id(t,
                               recorridos,
                               intervalo
                               )
          ) 
}
##-------------------------------------
## k_near_accident
##-------------------------------------
## Determina los k  puntos que se encuentran
## más cerca de cada uno de los accidentes dado un intervaloo
## de tiempo.
k_near_accident <- function(accidentes,
                            recorridos,
                            intervalo = 2.1e4,
                            k = 3){
    dlply(accidentes,1,
          function(t)t <-
              near_accident(t,
                            recorridos,
                            intervalo,
                            k)) 
}
##################################################################
##################################################################
##############################PRUEBAS#############################
##################################################################
##################################################################

####################### lectura de datos #########################
## Leer datos referentes a recorridos de patrullas y accidentes
##################################################################
## Recorridos
recorridos <- read.csv("../../angeles_verdes/data/recorridos.csv",
                       stringsAsFactors = FALSE)
recorridos$tiempo <- as.POSIXct(recorridos$tiempo)

## Accidentes
accidentes <- read.csv("../../angeles_verdes/data/base_hora.csv",
                       encoding = "latin1",
                       stringsAsFactors = FALSE)
accidentes$fecha <- as.POSIXct(accidentes$fecha)
accidentes_acc   <- filter(accidentes, servicio == "Accidente")
in_time_acc <- accidentes_acc[accidentes_acc$fecha <= max(recorridos$tiempo) &
                                  accidentes_acc$fecha >= min(recorridos$tiempo),]

## Prueba distancia más corta
k_vecinos_accidentes <- k_near_accident(in_time_acc, recorridos)
k_vecinos_accidentes <- k_near_accident_id(in_time_acc, recorridos)











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

