## En este script se lleva a cabo el análisis de la base de datos de ángeles verdes.
##----------------------------
## Funciones utilizadas
source("./directions.R")
##----------------------------
## Lectura de datos
## Recorridos
recorridos <- read.csv("../../angeles_verdes/data/recorridos.csv",
                       stringsAsFactors = FALSE)
recorridos$tiempo <- as.POSIXct(recorridos$tiempo)

## Accidentes
accidentes <- read.csv("../../angeles_verdes/data/base_hora_clean.csv",
##                       encoding = "latin1",
                       stringsAsFactors = FALSE)
accidentes$fecha <- as.POSIXct(accidentes$fecha)
