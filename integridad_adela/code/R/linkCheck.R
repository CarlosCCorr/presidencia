#!/usr/bin/Rscript
## Código que checa si las ligas de los servidores están activas.
## Librerías utilizadas
library(httr)
library(plyr)
## Lectura de datos (muestras de 15)
data   <- read.csv( "/home/lgarcia/proyectos/presidencia/data_analysis/serverUp/code/R/MAT.csv", stringsAsFactors = FALSE )
data   <- data[ data$Version == 1, ]
url    <- data$URL
## Prueba
test   <- ldply(url,
                function( t ){ u <-
                    http_status( GET( t ) ); print(paste0("url: ", t,", test result: ", u$message)); u$category } )
## result <- data.frame( url = data$URL, test_result = test[,1] )
## data$result <- test[,1]
## write.csv(data, "./MAT_RES.csv", row.names = FALSE)
## Imprimir resultados
## print(result)
