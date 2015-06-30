## Código que revisa que el formato de los datos sea el correcto.
library(lubridate)
library(plyr)
library(dplyr)
library(stringr)
##---------------------------------
## Funciones
##---------------------------------
####################################
#############FECHAS#################
####################################
##---------------------------------
## prob.date
##---------------------------------
prob.date <- function(col){
    ## Recorre e inspecciona todas las entradas de col
    ## y determina si estas cumplen con un patrón estandar de fecha
    ## IN
    ## col: la columna que se quiere inspeccionar
    ## OUT
    ## el porcentaje de entradas que cumplen con el patrón
    pattern <-
        '(((0{1}[1-9]{1}|[1,2]{1}[0-9]{1}|3{1}[0-1]{1})|(0{1}[1-9]{1}|1{1}[0-2])|(1{1}[0-9]{4}|20{1}(0{1}[0-9]{1}|1{1}[0-5]{1})))[[:punct:]]?){1,3}'
    col.match  <- ldply(str_split(col," "),function(t)t<- t[[1]][1])
    true_match <- na.omit(str_length(str_match(col,pattern)[,1]) == str_length(col.match[,1]))
    sum(true_match)/length(col)
}
##---------------------------------
## ident.date
##---------------------------------
ident.date <- function(df, thresh = .7){
    ## Aplica prob.date a todas las columnas de ident.date y determina si la
    ## proporción de entradas que cumplen con el patrón es mayor que thresh
    ## IN
    ## df: data.frame de la que se quieren verificar las columnas
    ## OUT
    ## variable booleana que identifica a las columnas que sobrepasan thresh.
    apply(df, 2,function(t) t <- prob.date(t)) > thresh
}
####################################
###########COORDENADAS##############
####################################
##---------------------------------
## prob.coord
##---------------------------------
prob.coord <- function(col){
    ## Recorre e inspecciona todas las entradas de col
    ## y determina si estas cumplen con un patrón estandar de coordenada
    ## IN
    ## col: la columna que se quiere inspeccionar
    ## OUT
    ## el porcentaje de entradas que cumplen con el patrón
    pattern <-
        "^-?[0-9]{2,3}.{1}[0-9]{3,}"
    col.match  <- ldply(str_split(col," "),function(t)t<- t[[1]][1])
    true_match <- na.omit(str_length(str_match(col,pattern)[,1]) == str_length(col.match[,1]))
    sum(true_match)/length(col)
}

##---------------------------------
## ident.coord
##---------------------------------
ident.coord <- function(df, thresh = .7){
    ## Aplica prob.coord a todas las columnas de ident.date y determina si la
    ## proporción de entradas que cumplen con el patrón es mayor que thresh
    ## IN
    ## df: data.frame de la que se quieren verificar las columnas
    ## OUT
    ## data.frame con las columnas que respresentan las coordenadas.
    res <- data.frame(matrix(NA,ncol = 2, nrow = nrow(df)))
    coords      <- df[,apply(df, 2,function(t) t <- prob.coord(t)) > thresh]
    res[,1] <- coords[, which(coords[1,]<0) ]
    res[,2] <- coords[, which(coords[1,]>0) ]
    names(res) <- c("long","lat")
    res
}
##---------------------------------
## correct.coord
##---------------------------------
correct.coord <- function(df){
    upper.left  <-  c(-121.615487, 32.812103)
    lower.right <-  c(-81.844979,  14.143912)
    coords      <-  ident.coord(df)
    if(prod(upper.left[1]< coords$long)>0 & prod(coords$long<lower.right[1])>0){
        print('longitud dentro del territorio')
    }else{
        print('longitud fuera del territorio')
    }
    if(prod(upper.left[2]>coords$lat)>0 & prod(coords$lat > lower.right[2])>0){
        print('latitud dentro del territorio')
    }else{
        print('latitud fuera del territorio')
    }
}
