
################################# 

#0.Paquetes--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
install.packages("tidyverse")
install.packages("pacman")
install.packages("osmdata")
install.packages("httr2")
library(httr2)
library(osmdata)
library(pacman)
require(pacman)
p_load(here,knitr,tidyverse,ggthemes,fontawesome,kableExtra)
p_load(tidyverse,rio,viridis,sf, leaflet, tmaptools)
# option html
options(htmltools.dir.version = F)
opts_chunk$set(fig.align="center", fig.height=4 , dpi=300 , cache=F)
remotes::install_github('ropensci/osmdata')

#----1. Bogotá  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
####################### Delimitar Bogotá 
Bogota <- getbb(place_name = "Bogota", 
                featuretype = "boundary:administrative", 
                format_out = "sf_polygon") %>% .$multipolygon
Bogota<-Bogota[1,]
leaflet() %>% addTiles() %>% addPolygons(data=Bogota)

#----2. Medellín  -----------------------------------------------------------------------------------------------------------------------------------
####################### Delimitar Medellín 
Medellin <- getbb(place_name = "Medellin Antioquia Colombia", 
                  featuretype = "boundary:administrative", 
                  format_out = "sf_polygon") 
Medellin<-Medellin[1,]
leaflet() %>% addTiles() %>% addPolygons(data=Medellin)

#----3. Cali  -----------------------------------------------------------------------------------------------------------------------------------
####################### Delimitar Cali 
Cali <- getbb(place_name = " Cali Colombia", 
              featuretype = "boundary:administrative", 
              format_out = "sf_polygon") 
leaflet() %>% addTiles() %>% addPolygons(data=Cali)
Cali<-Cali[1,]
leaflet() %>% addTiles() %>% addPolygons(data=Cali)

#-------------------------------------------------- Transporte (vías y estaciones) ----------------------------------------------------------------------------------------------------------------------- 
#----1. Bogotá  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
####################### Bus station Bgtá
## Guardamos un amenity con el Poligono que en este caso es bogota con las estaciones de bus
osm6 = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
## Fijamos un objeto
osm_sf6 = osm6 %>% osmdata_sf()
osm_sf6
bus_bog = osm_sf6$osm_points %>% select(osm_id) 
bus_bog
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_bog, col="red")
## Interceptando estaciones de bus con Bogotá
bus_bog$intercep <- st_intersects(bus_bog, Bogota,sparse=FALSE)[,1]
bus_bog<- bus_bog[bus_bog$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=bus_bog, col="black")
table(bus_bog$intercep)


####################### Vías Bgtá
## Guardamos un amenity con el Poligono que en este caso es bogota con las vias
osm9 = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="highway" , value="primary") 
## Fijamos un objeto
osm_sf9 = osm9 %>% osmdata_sf()
osm_sf9
vias_bog = osm_sf9$osm_lines %>% select(osm_id) 
vias_bog
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addPolylines(data=vias_bog , col="red")
## Interceptando vias con Bogotá
vias_bog$intercep <- st_intersects(vias_bog, Bogota,sparse=FALSE)[,1]
vias_bog<- vias_bog[vias_bog$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addPolylines(data=vias_bog, col="black") %>% addPolylines(data=vias_bog, col="red")
table(vias_bog$intercep)



#----2. Medellín  -----------------------------------------------------------------------------------------------------------------------------------
####################### Bus station Medellín
## Guardamos un amenity con el Poligono que en este caso es Medellin con las estaciones de Bus
osm7 = opq(bbox = getbb("Medellin Antioquia Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
## Fijamos un objeto
osm_sf7 = osm7 %>% osmdata_sf()
osm_sf7
bus_med = osm_sf7$osm_points %>% select(osm_id) 
bus_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_med, col="red")
## Interceptando estaciones de bus con Medellin
bus_med$intercep <- st_intersects(bus_med, Medellin,sparse=FALSE)[,1]
bus_med<- bus_med[bus_med$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=bus_med, col="black")
table(bus_med$intercep)


####################### vías Medellìn
## Guardamos un amenity con el Poligono que en este caso es Medellin con las vias
osm8 = opq(bbox = getbb("Medellin Antioquia Colombia")) %>%
  add_osm_feature(key="highway" , value="primary") 
## Fijamos un objeto
osm_sf8 = osm8 %>% osmdata_sf()
osm_sf8
vias_med = osm_sf8$osm_lines %>% select(osm_id) 
vias_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addPolylines(data=vias_med , col="red")
## Interceptando vias con Medellin
vias_med$intercep <- st_intersects(vias_med, Medellin,sparse=FALSE)[,1]
vias_med<- vias_med[vias_med$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addPolylines(data=vias_med, col="black")%>% addPolylines(data=vias_med, col="blue")
table(vias_med$intercep)


####################### Metro Medellín

## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
osm9 = opq(bbox = getbb("Medellin Antioquia Colombia")) %>%
  add_osm_feature(key="railway" , value="subway") 
## Fijamos un objeto
osm_sf9 = osm9 %>% osmdata_sf()
osm_sf9
metro_med = osm_sf9$osm_points %>% select(osm_id) 
metro_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=metro_med , col="red")
## Interceptando industria con Bogotá
metro_med$intercep <- st_intersects(metro_med, Medellin,sparse=FALSE)[,1]
metro_med<- metro_med[metro_med$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=metro_med, col="black")%>% addCircles(data=metro_med, col="white")
table(metro_med$intercep)



#----3. Cali  -----------------------------------------------------------------------------------------------------------------------------------
####################### Bus station Cali
## Guardamos un amenity con el Poligono 
osm10 = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
## Fijamos un objeto
osm_sf10 = osm10 %>% osmdata_sf()
osm_sf10
bus_cali = osm_sf10$osm_points %>% select(osm_id) 
bus_cali
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_cali, col="red")
## Interceptando estaciones de bus con Cali
bus_cali$intercep <- st_intersects(bus_cali, Cali,sparse=FALSE)[,1]
bus_cali<- bus_cali[bus_cali$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=bus_cali, col="black")
table(bus_cali$intercep)


####################### Vías Cali

## Guardamos un amenity con el Poligono que en este caso es Cali con las vias
osm11 = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="highway" , value="primary") 
## Fijamos un objeto
osm_sf11 = osm11 %>% osmdata_sf()

osm_sf11
vias_cali = osm_sf11$osm_lines %>% select(osm_id) 
vias_cali
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addLines(data=vias_cali , col="red")
## Interceptando vias con Bogotá
vias_cali$intercep <- st_intersects(vias_cali, Cali,sparse=FALSE)[,1]
vias_cali<- vias_cali[vias_cali$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addPolylines(data=vias_cali, col="black")%>% addPolylines(data=vias_cali, col="blue")


