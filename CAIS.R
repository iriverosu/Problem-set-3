#------------------------------------------------------------- Paquetes, lectura bases, NaN -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
#1.Paquetes--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
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


#-----------------------------------------------------CAIs--------------------------------------------

####Bogotá########
## Guardamos un amenity con el Poligono que en este caso es bogota con las estaciones de Policia 
osm_p = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="police") 
## Fijamos un objeto
osm_sfp = osm_p %>% osmdata_sf()
osm_sfp
police_bog = osm_sfp$osm_points %>% select(osm_id) 
police_bog
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=police_bog, col="red")
## Interceptando oficinas con Bogotá
police_bog$intercep <- st_intersects(police_bog, Bogota,sparse=FALSE)[,1]
police_bog<- police_bog[police_bog$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=police_bog, col="black")
table(police_bog$intercep)


#Medellin #######

####################### Estaciones de Policia Medellín 

## Guardamos un amenity con el Poligono que en este caso es Medellin con las estaciones de Policia 
osm_p3 = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="police") 
## Fijamos un objeto
osm_sfp3 = osm_p3 %>% osmdata_sf()
osm_sfp3
police_med = osm_sfp3$osm_points %>% select(osm_id) 
police_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=police_med, col="red")
## Interceptando CAIS con Medellín
police_med$intercep <- st_intersects(police_med, Medellin,sparse=FALSE)[,1]
police_med<- police_med[police_med$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=police_med, col="black")
table(police_med$intercep)





#Cali#######
## Guardamos un amenity con el Poligono que en este caso es Cali con las estaciones de Policia 
osm_p2 = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="police") 
## Fijamos un objeto
osm_sfp2 = osm_p2 %>% osmdata_sf()
osm_sfp2
police_cali = osm_sfp2$osm_points %>% select(osm_id) 
police_cali
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=police_cali, col="red")
## Interceptando CAIS con Cali
police_cali$intercep <- st_intersects(police_cali, Cali,sparse=FALSE)[,1]
police_cali<- police_cali[police_cali$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=police_cali, col="black")
table(police_cali$intercep)


#-------distancias---

#colegios
#Kindergarden
#universidades

train_bogota <- subset(train2, city=='Bogotá D.C')
train_medellin <- subset(train2, city=='Medellín')


dist_universidad_bog<-st_nearest_points(x=train_bogota, y= universidades_bog)
dist_universidad_med<-st_nearest_points(x=train_medellin, y= universidades_med,progress=TRUE)



#vias
#bus/metro
#office
#industrial
#retail
#CAIs

