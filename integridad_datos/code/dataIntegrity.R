## Código que revisa que el formato de los datos sea el correcto.
library(lubridate)
library(plyr)
library(dplyr)
library(stringr)
##---------------------------------
## Funciones
##---------------------------------

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
