## librerías utilizadas
library(rjson)
library(RJSONIO)
library(RCurl)
library(plyr)
library(stringr)
library(data.table)
## Crear query
origen <- "DF"
destino <- "Guadalajara"
mode <- "driving"
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
directions <- get_directions(origen,destino)
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
distance <- get_distance(origen,destino)
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
## Determina los vehículos de rescate
## que se encontraban a una distancia mínima del
## punto del accidente.
get_min_resc  <- function(accident, rescuers){
    distances <- ldply(rescuers, function(t)t <- get_distance(t,accident)$distance)[,1]
    distances[which(distances)==min(distances)]
}
############################ Pruebas ############################
## lectura de datos
## Recorridos de patrullas
recorridos <- read.csv("../angeles_verdes/data/recorridos.csv", sep = ",")
setkey(recorridos,Patrulla)
setnames(recorridos, tolower(names(recorridos)))
## Accidentes
accidentes <- fread("../angeles_verdes/data/angels_clean.csv", sep = ",")
setnames(accidentes, tolower(names(accidentes)))
############################ Análisis ############################
## Análisis exploratorio
test_coord <- c(30.637415,-112.541097)
