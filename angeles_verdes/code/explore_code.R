## Luis Manuel Román García
## Código para explorar la base de datos de ángeles verdes.
##---------------------------------------
## Librerías utilizadas
library(dplyr)
library(tidyr)
library(tm)
library(stringr)
## Lectura de datos
data <- read.csv("./data/angels_clean.csv")
## Procesamiento de datos
data$FechaServi <- as.Date(data$FechaServi)
