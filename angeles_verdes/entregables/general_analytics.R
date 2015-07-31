##------------------------------
## Códigoexploración gráfica
## Ángeles Verdes
##------------------------------
library(plyr)
library(foreign)
library(sp)
library(rgdal)
library(maptools)
library(lubridate)
library(shapefiles)
library(proj4)
library(geosphere)
library(mapproj)
library(FNN)
library(ggmap)
library(RecordLinkage)
library(tidyr)
library(RPostgreSQL)
##------------------------------
## Conectamos con PostgreSQL
##------------------------------
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="angeles_verdes", user="lgarcia")
## Dado que en esta parte del análisis no contamos con una base
## de datos que contenga hora, nos limitaremos a hacer el análisis
## en escala de días. Es de fundamental importancia contar con una
## base con horas ya que esta se utilizará para la predicción de
## incidentes en esa escala y optimizar el tiempo de respuesta.
##------------------------------
## Estudio de accidentes
##------------------------------
## Cargamos datos de carreteras.
query_carr <- "select * from carreteras;"
carreteras <- dbGetQuery(con, query_carr)
query_curv <- "select carretera, lat, lon, curv from carreteras, curv where carretera = tramo;"
carr_curv      <- dbGetQuery(con,query_curv)
## Mapa base
road.map      <- get_map(location = "Mexico", zoom = 5, maptype = "roadmap")
road.map.plot <- ggmap(road.map)
## Capa de carreteras
plot_road     <- road.map.plot +
    geom_point(data = carreteras, aes(x = lon,
                   y = lat), alpha = .5, size = .8)
## Capa de curvas
plot_curv     <- road.map.plot +
    geom_point(data = carr_curv, aes(x = lon,
                   y = lat, size = curv, col = curv), alpha = .5, size = .8)
##---------------------------------------------------------------
## ¿Cuáles son las carreteras más problemáticas?
## (Mayor número de incidentes proporcional a la circulación)
query_1 <- "select prop_acc, lon, lat, carretera from (select count(*)/avg(circ) as prop_acc, tramo from accident group by tramo order by prop_acc desc) as query, carreteras where carretera = tramo;"
## consultas
prop_incidentes_carreteras <- dbGetQuery(con, query_1)
## gráficas
## Capa de proporción de accidentes
plot_prop_acc <-
    road.map.plot +
        geom_point(data = carreteras, aes(x = lon,
                       y = lat), alpha = .5, size = .8) +
            geom_point(data = prop_incidentes_carreteras, aes(x = lon,
                           y = lat,
                           col = prop_acc),size = .8) +
                theme(axis.text =
                          element_text(colour = "#6200EA"),
                      axis.title.y =
                          element_blank(),
                      axis.title.x =
                          element_blank(),
                      title =
                          element_text(size = 15, colour = "#6200EA", vjust = 0.7 ),
                      panel.background =
                          element_blank()) +
                    scale_colour_gradient("proporción de \n incidentes",
                                          trans = "log",
                                          low="#6200EA")
multiplot(plot_road, plot_prop_acc, cols = 2)
##---------------------------------------------------------------
## ¿Cuáles son las carreteras con accidentes más graves?
## (Mayor número de turistas atendidos proporcional a la circulación)
query_2 <- "select prop_grav, lon, lat, carretera from (select sum(turistat)/avg(circ) as prop_grav, tramo from accident group by tramo order by prop_grav desc) as query, carreteras where carretera = tramo;"
prop_gravedad_carreteras <- dbGetQuery(con, query_2)
## gráficas
## Capa de carreteras
plot_road     <-road.map.plot +
    geom_point(data = carreteras, aes(x = lon,
                   y = lat), alpha = .5, size = .8)
## Capa de proporción de accidentes
plot_prop_grav <-
    road.map.plot +
        geom_point(data = carreteras, aes(x = lon,
                       y = lat), alpha = .5, size = .8) +
            geom_point(data = prop_gravedad_carreteras, aes(x = lon,
                           y = lat,
                           col = prop_grav), size = .8) +
                theme(axis.text =
                          element_text(colour = "#6200EA"),
                      axis.title.y =
                          element_blank(),
                      axis.title.x =
                          element_blank(),
                      title =
                          element_text(size = 15, colour = "#6200EA", vjust = 0.7 ),
                      panel.background =
                          element_blank()) +
                    scale_colour_gradient("gravedad de \n incidentes",
                                          trans = "log",
                                          low="#D50000",
                                          high="#EF9A9A")
multiplot(plot_prop_acc, plot_prop_grav, cols = 2)
## ¿Afecta la curvatura de las carreteras, el número
##  de incidentes o la gravedad de los mismos?
query_3 <- "select prop_acc, curvatura, lat, lon, carretera from (select count(*)/avg(circ) as prop_acc, abs(avg(curv)) as curvatura, tramo from accident group by tramo order by prop_acc desc) as query, carreteras where carretera = tramo;"
prop_inci_curv <- dbGetQuery(con, query_3)
plot_curv     <- road.map.plot +
    geom_point(data = prop_inci_curv,
               aes(x = lon,
                   y = lat,
                   col = curvatura,
                   size = prop_acc))+
        theme(axis.text =
                  element_text(colour = "#6200EA"),
              axis.title.y =
                  element_blank(),
              axis.title.x =
                  element_blank(),
              title =
                  element_text(size = 15, colour = "#6200EA", vjust = 0.7 ),
              panel.background =
                  element_blank()) +
            scale_colour_gradient("curvatura",
                                  low="#6200EA") +
scale_size("proporción de \n incidentes",range = c(1,3))
## ¿Afecta el número de intersecciones, o la presencia
##  de las mismas?
## ¿Existe una componente temporal?
query_4 <- "select count(*) as accidentes, extract(month from fecha) as mes, tramo from accident group by mes, tramo order by  mes, accidentes desc;"
month_acc_road <- dbGetQuery(con, query_4)
ggplot(data = month_acc_road, aes(x = mes, y = tramo, size = accidentes)) +
    geom_point(color = "#00E5FF")+
        theme(axis.text =
                  element_text(colour = "#6200EA"),
              axis.text.y = element_text(size = 3.5),
              axis.title.y =
                  element_blank(),
              axis.title.x =
                  element_blank(),
              title =
                  element_text(size = 15, colour = "#6200EA", vjust = 0.7 ),
              panel.background =
                  element_blank())
## ¿Existe una componente temporal espacial?
##------------------------------
## Estudio de recorridos
##------------------------------
