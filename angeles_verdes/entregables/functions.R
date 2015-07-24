## Funciones para llevar a cabo tareas básicas de manipulación
## con los datos de las carreteras.
## Librerías utilizadas
library(plyr)
library(foreign)
library(sp)
library(rgdal)
library(maptools)
library(shapefiles)
library(proj4)
library(geosphere)
library(mapproj)
## Lectura de shapefile
## shp <- readOGR("/home/lgarcia/proyectos/presidencia/data_analysis/angeles_verdes/data/data_entregable/CSTAV_Cobertura_Carretera-0/CSTAV_Cobertura_Carretera/","Cobertura_Carretera_CSTAV_2014")
### Obtención de coordenadas
## el shape tiene 245 rutas.
## las lineas que definen cada tramo de la
## carretara están dentro de lines
## las coordenadas de cada linea estan en Lines@coords
##
## ejemplo:
## Número de carreteras
## nrow(shp)
## Número de lineas que constituyen la carretera 2
## length(shp[2,1]@lines[[1]]@Lines)
## coordenadas de la primera línea de la carretera 2
## shp[2,1]@lines[[1]]@Lines[[1]]@coords
## datos de la proyección
## CRS arguments:
##    +proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000
## +y_0=0 +ellps=GRS80 +units=m +no_defs
##coords.tot <- list()
##carretera  <- list()
##for(i in 1:nrow(shp)){
##    ## Obtenemos total de lineas que constituyen
##    ## la carretera.
##    lines  <- length(shp[i,1]@lines[[1]]@Lines)
##    coords <- list()
##    print(paste0("carretera: ",i))
##    for(j in 1:lines){
##        coords[[j]]    <- shp[i,1]@lines[[1]]@Lines[[j]]@coords
##        print(paste0("línea: ",j))
##    }
##    coords.tot[[i]] <- ldply(coords,function(t)t<-t)
##    carretera[[i]]  <- rep(as.character(shp[i,1]@data$NOMVIAL[1]),
##                           nrow(coords.tot[[i]]))
##}
##coords.tot <- ldply(coords.tot, function(t)t<-t)
##carretera  <- unlist(carretera)
##coords.tot$carretera <- carretera
##names(coords.tot) <-c("lon","lat","carretera")
#### transformar cordenadas
##coords <- project(coords.tot[,1:2], proj= c("+proj=lcc", "+lat_1=17.5", "+lat_2=29.5", "+lat_0=12", "+lon_0=-102", "+x_0=2500000", "+y_0=0", "+ellps=GRS80" ,"+units=m", "+no_defs"), inverse = TRUE)
##coords.tot$lon <- unlist(coords[[1]])
##coords.tot$lat <- unlist(coords[[2]])
## Guardar datos
##write.csv(coords.tot, "/home/lgarcia/proyectos/presidencia/data_analysis/angeles_verdes/data/data_entregable/road_coords.csv", row.names = FALSE)
##########################################################################
#################################Funciones################################
##########################################################################
## Lectura de datos
coords <- read.csv("/home/lgarcia/proyectos/presidencia/data_analysis/angeles_verdes/data/data_entregable/road_coords.csv", stringsAsFactors = FALSE)
p <- c(-99.1710295,19.4052998)
##----------------------------
## asigna_camino
##----------------------------
## dado un punto, le asigna la carretera
## a la cual pertenece (la más cercana)
## p= punto en formato longitud, latitud
asigna_camino <- function(p){
    d_lon <- abs(coords$lon - as.numeric(p[1]))
    d_lat <- abs(coords$lat - as.numeric(p[2]))
    dist  <- data.frame(lon=d_lon,lat=d_lat)
    dist  <- apply(dist,1,max)
    near  <- head(coords[order(dist),],3)
    dist  <- distCosine(near[,1:2],p)
    data.frame(carretera = near[,3],
               dist = dist/1000,
               nearest_lon = near[,1],
               nearest_lat = near[,2])
}
##----------------------------
## curvatura
##----------------------------
## dado un punto, le asigna la carretera
## a la cual pertenece (la más cercana)
## p= punto en formato longitud, latitud
## θ = atan2(sin(Δlong)*cos(lat2), cos(lat1)*sin(lat2) − sin(lat1)*cos(lat2)*cos(Δlong)
curvatura <- function(p){
    road     <- asigna_camino(p)
    delt_lon <- road$nearest_lon[1] - road$nearest_lon[3]
    angle    <- atan2(sin(delt_lon)*cos(road$nearest_lat[3]),
                      cos(road$nearest_lat[1])*sin(road$nearest_lat[3])-
                          sin(road$nearest_lat[1])*cos(road$nearest_lat[3])*
                              cos(delt_lon)
                      )
    angle
}

curvature <- apply(coords[,1:2],1,curvatura)
