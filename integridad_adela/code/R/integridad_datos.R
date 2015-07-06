## Código que revisa que el formato de los datos sea el correcto.
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
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
##---------------------------------
## correct.date
##---------------------------------
format.date <- function(col){
    ## Explora col  y determina si las fechas
    ## se encuentran en algún formato conocido.
    ## IN
    ## col: columna con fechas
    ## OUT
    ## cadena que indica si las fechas tienen un formato conocido o no.
    result <- ""
    suppressWarnings(formats <- list(
        ymd = sum(is.na(ymd(col))),
        mdy = sum(is.na(mdy(col))),
        dym = sum(is.na(dym(col))),
        dmy = sum(is.na(dmy(col))),
        myd = sum(is.na(myd(col))),
        ydm = sum(is.na(ydm(col)))
        ))
    values     <- laply(formats, function(t)t<- t[[1]])
    min.values <- min(values)
    format     <- names(formats)[which(values == min.values)]
    result     <- paste0("El formato más factible es: ", format,
                         ". Hay: ", min.values, " inconsistencias")
    result[1]
}
##---------------------------------
## check.date
##---------------------------------
check.date <- function(df, thresh = .7){
    dates   <- df[, ident.date(df,thresh)]
    cols    <- names(dates)
    formats <- apply(dates,2, format.date)
    results <- paste0(rep('Columna de fecha: ', length(cols)),
                      cols,
                      rep('. Resultado: ', length(cols)),
                      formats
                      )
    results
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
    prob <- c()
    pattern1 <-
        "^-?[0-9]{2,3}.{1}[0-9]{3,}"
    pattern2 <-
        "([0-9]{2,3}[[:punct:]]{1}){1}([0-9]{2}[[:punct:]]{1}){1}([0-9]{2}[[:punct:]]{2}){1}"
    col.match  <- ldply(str_split(col," "),function(t)t<- t[[1]][1])
    true_match1 <- na.omit(str_length(str_match(col,pattern1)[,1]) == str_length(col.match[,1]))
    true_match2 <- na.omit(str_length(str_match(col,pattern2)[,1]) == str_length(col.match[,1]))
    prob1 <- sum(true_match1)/length(col)
    prob2 <- sum(true_match2)/length(col)
    prob[1] <- prob1
    prob[2] <- prob2
    prob
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
    res <- list()
    res1        <- data.frame(matrix(NA,ncol = 2, nrow = nrow(df)))
    res2        <- data.frame(matrix(NA,ncol = 2, nrow = nrow(df)))
    coords1     <- df[,apply(df, 2,function(t) t <- prob.coord(t)[1] > thresh)]
    coords2     <- df[,apply(df, 2,function(t) t <- prob.coord(t)[2] > thresh)]
    if(ncol(coords1)>0){
      long <-
          apply(coords1,2,function(t)t<- sum(str_detect(t,"[0-9]{3}[[:punct:]]{1}")) > 0)
      res1[,1]     <- coords1[long]
      res1[,2]     <- coords1[1-long]
      names(res1)  <- c("long","lat")
      res[[1]] <- res1
  }
    if(ncol(coords2)>0){
        long <-
            apply(coords2,2,function(t)t<- sum(str_detect(t,"[0-9]{3}[[:punct:]]{1}")) > 0)
        res2[,1]     <- coords2[long]
        res2[,2]     <- coords2[1-long]
        names(res2)  <- c("long","lat")
        res[[2]]     <- res2
    }
    res
}
##---------------------------------
## correct.coord
##---------------------------------
correct.coord <- function(df){
    upper.left  <-  c(-121.615487, 32.812103)
    lower.right <-  c(-81.844979,  14.143912)
    coords      <-  ident.coord(df)[[1]]
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
####################################
######Entidades Federativas#########
####################################
##---------------------------------
## prob.entity
##---------------------------------
prob.entity <- function(col){
    ## Recorre e inspecciona todas las entradas de col
    ## y determina si estas cumplen con un patrón estandar de fecha
    ## IN
    ## col: la columna que se quiere inspeccionar
    ## OUT
    ## el porcentaje de entradas que cumplen con el patrón
    pattern <-
        '(([A-Z]{1}[[:alpha:]]{2,}|[A-Z])([[:punct:]]|[[:space:]])?){1,3}'
    true_match <-
        na.omit(str_length(str_match(col,pattern)[,1]) ==
                    str_length(col))
    sum(true_match)/length(col)    
}
##---------------------------------
## ident.entity
##---------------------------------
ident.entity <- function(df, thresh = .7){
    ## Aplica prob.date a todas las columnas de ident.date y determina si la
    ## proporción de entradas que cumplen con el patrón es mayor que thresh
    ## IN
    ## df: data.frame de la que se quieren verificar las columnas
    ## OUT
    ## variable booleana que identifica a las columnas que sobrepasan thresh.
    apply(df, 2,function(t) t <- prob.entity(t)) > thresh
}
########################################################
########################################################
## Pruebas #############################################
########################################################
########################################################
##### Prueba fecha
test <- data.frame(fecha      = c("20011012","2012-03-15", "2015-07-01"),
                   casi_fecha = c("20011512","12-23-23", "13-15-17"),
                   no_fecha   = c("hola", "una", "prueba"),
                   ruido      = c(14234, 123423515, 12341234))

check.date(test)
##### Prueba entidad federativa
