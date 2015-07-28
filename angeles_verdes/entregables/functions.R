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
library(FNN)
library(ggmap)
## Lectura de shapefile
## shp <- readOGR("/home/lgarcia/proyectos/presidencia/data_analysis/angeles_verdes/data/data_entregable/CSTAV_Cobertura_Carretera-0/CSTAV_Cobertura_Carretera/","Cobertura_Carretera_CSTAV_2014")
### Obtención de coordenadas
## el shape tiene 245 rutas.
## las lineas que definen cada tramo de la
## carretara están dentro de lines
## las coordenadas de cada linea estan en Lines@coords
#########
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
##########
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
## Datos oficiales accidentes
accidentes_tot<- read.csv("../../../angeles_verdes/data/angels_clean.csv",
                          stringsAsFactors = FALSE)
## Datos recorridos
recorridos <- read.csv("../../../angeles_verdes/data/recorridos.csv",
                       stringsAsFactors = FALSE)
recorridos$tiempo <- as.POSIXct(recorridos$tiempo)
## Datos de las carreteras
coords <- read.csv("/home/lgarcia/proyectos/presidencia/data_analysis/angeles_verdes/data/data_entregable/road_coords.csv", stringsAsFactors = FALSE)
## Datos de tráfico
aforo <- read.csv("/home/lgarcia/proyectos/presidencia/data_analysis/angeles_verdes/data/data_entregable/aforo.csv", stringsAsFactors = FALSE)
## Datos de accidentes 
accident <- read.csv("/home/lgarcia/proyectos/presidencia/data_analysis/angeles_verdes/data/base_hora_clean.csv", stringsAsFactors = FALSE)
## Datos limpios y agregados carreteras
tij_ens <- read.csv("/home/lgarcia/proyectos/presidencia/data_analysis/angeles_verdes/data/data_entregable/tij_ens.csv", stringsAsFactors = FALSE)
accident$fecha <- as.POSIXct(accident$fecha)
accident <- accident[accident$fecha <= max(recorridos$tiempo) &
                         accident$fecha >= min(recorridos$tiempo),]
## Tijuana Ensenada (punto 4)
##tij_ens <- dplyr::filter(coords, carretera == "Ensenada-Tijuana")
##----------------------------
## asigna_camino (punto 1)
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
    dist  <- apply(near[,1:2], 1,function(t)t <-
        distCosine(as.numeric(t),as.numeric(p)))
    data.frame(carretera = near[,3],
               dist = dist/1000,
               nearest_lon = near[,1],
               nearest_lat = near[,2])
}
##----------------------------
## curvatura (punto 2)
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

##----------------------------
## asignar accidentes (punto 3)
##----------------------------
asign_acc <- function(tij_ens_coords, accident_coord, TOL = 300){
    near <- apply(tij_ens_coords,1, function(t)t <- distCosine(t, accident_coord))
    sum(near < TOL)
}

asign_acc_n <- function(tij_ens_coords, accidents_coords, TOL = 300){
    apply(accidents_coords, 1, function(t)t <- asign_acc(tij_ens_coords, t, TOL))
}
##n_acc     <- asign_acc_n(accident[,11:10], tij_ens[,1:2])
##tij_ens$acc_cerca <- n_acc
####----------------------------
#### asignar circulación (tij_ens, accidente)
####----------------------------
#### Correr k-nearest neighbors
#### Tij-Ens
##set.seed(123454321)
##train <- aforo[,1:2]
##test  <- tij_ens[,2:1]
##y     <- aforo[,3]
##names(test) <- c("latitude","longitude")
##knn_tij <- knn.reg(train = train,test = test,y = y,"cover_tree", k = 3)
##tij_ens$circ_diaria <- knn_tij$pred
#### Accidentes
##train <- aforo[,1:2]
##test  <- accident[,10:11]
##y     <- aforo[,3]
##names(test) <- c("latitude","longitude")
##knn_acc <- knn.reg(train = train,test = test,y = y,"cover_tree", k = 3)
##accident$circ_diaria <- knn_acc$pred
####---------------------------
## pruebas
##---------------------------
## Estudio de Outliers
##outliers <- dplyr::filter(tij_ens, prop_acc > .003)
##road.map      <- get_map(location = "31.98838,-116.7458", zoom = 13, maptype = "roadmap")
##road.map.plot <- ggmap(road.map)
##road.map.plot + geom_point(data = outliers, aes(x = lon, y = lat),
##                           col = "#6200EA" ) +
##theme(axis.text = element_text(colour = "#6200EA"), axis.title.y = element_blank(), axis.title.x = element_blank(),
##      title = element_text(size = 15, colour = "#6200EA", vjust = 0.7 ),
##      panel.background = element_blank())
####----------------------------
## Prueba con datos oficiales
##----------------------------
acc_in_zone <- dplyr::filter(accidentes_tot, Nom_Ent == "Baja California")
unique(acc_in_zone$Nom_Ent)
coords_acc_in_zone <- acc_in_zone[,22:23]



