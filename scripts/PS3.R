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


#Variables PS3 
#1.Área total o cubierta
#2.bedrooms
#3.bathrooms (k-vecinos)
#4.property type
#5.Distancia al CBD
#6.Distancia Tansporte (estaciones o vías arteriales)
#7.Distancia Colegios, universidades, kindergarden
#8.IPM #coordenadas

#data como sf
train2 <- st_as_sf(x = train, ## datos
                   coords=c("lon","lat"), ## coordenadas
                   crs=4326) ## CRS

leaflet() %>% addTiles() %>% addCircleMarkers(data=train2)
class(houses)
class(train2)
table(train2$rooms, train2$bedrooms)
sum(is.na(train2$bedrooms))
sum(is.na(train2$bathrooms)) #15032


#------------------------------------------------------------- OSM ------------------------------------------------------------  
## Buscar un lugar público por el nombre
geocode_OSM("Casa de Nariño, Bogotá")
## geocode_OSM no reconoce el caracter #, en su lugar se usa %23% 
cbd <- geocode_OSM("Centro Internacional, Bogotá", as.sf=T) 
cbd

#------------------------------------------------------------- área -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
#1. total=cubierta
#2. Texto 
#------------------------------------------------------------- CBD -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
Isabella 
# Bgtá (https://www.larepublica.co/economia/concentracion-de-trabajadores-no-es-proporcional-respecto-a-las-zonas-de-residencia-3453320)
####################### Delimitar Bogotá 
####################### Office Bgtá
## Bbox (mapa)
leaflet() %>% addTiles() %>% addPolygons(data=Bogota)
Bogota <- getbb(place_name = "Bogota", 
                featuretype = "boundary:administrative", 
                format_out = "sf_polygon") %>% .$multipolygon
## Oficinas
## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
    osm6 = opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key="building" , value="office") 
## Fijamos un objeto
    osm_sf6 = osm6 %>% osmdata_sf()
    osm_sf6
    office_bog = osm_sf6$osm_points %>% select(osm_id) 
    office_bog
    ### Si queremos visualizarlo
    leaflet() %>% addTiles() %>% addCircleMarkers(data=office_bog, col="red")
## Interceptando oficinas con Bogotá
    office_bog$intercep <- st_intersects(office_bog, Bogota,sparse=FALSE)[,1]
    office_bog<- office_bog[office_bog$intercep == TRUE, ]
    leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=office_bog, col="black")
    table(office_bog$intercep)
    
####################### Industria Bgtá
    ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
      osm9 = opq(bbox = getbb("Bogotá Colombia")) %>%
      add_osm_feature(key="building" , value="industrial") 
    ## Fijamos un objeto
    osm_sf9 = osm9 %>% osmdata_sf()
    osm_sf9
    industrial_bog = osm_sf9$osm_points %>% select(osm_id) 
    industrial_bog
    ### Si queremos visualizarlo
    leaflet() %>% addTiles() %>% addCircleMarkers(data=industrial_bog , col="red")
    ## Interceptando industria con Bogotá
    industrial_bog$intercep <- st_intersects(industrial_bog, Bogota,sparse=FALSE)[,1]
    industrial_bog<- industrial_bog[industrial_bog$intercep == TRUE, ]
    leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=industrial_bog, col="black")
    table(industrial_bog$intercep)
    
#Medellín
#industria
opq(bbox = getbb("Medellín Colombia"))
## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
osm7 = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="building" , value="industrial") 

## Fijamos un objeto
osm_sf7 = osm7 %>% osmdata_sf()
osm_sf7
industrial_med = osm_sf7$osm_points %>% select(osm_id,building) 
industrial_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=industrial_med , col="red")

####################### Office Bgtá
opq(bbox = getbb("Medellín Colombia"))
## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
osm8 = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="building" , value="office") 
## Fijamos un objeto
osm_sf8 = osm8 %>% osmdata_sf()
osm_sf8
office_med =  osm_sf8$osm_points %>% select(osm_id,building) 
office_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=office_med , col="red")


# Medellín (https://www.artchitectours.es/tour/el-poblado-medellin/)
# Cali (Torre de Cali 1.5km medialuna)
#-------------------------------------------------- Transporte (vías y estaciones) ----------------------------------------------------------------------------------------------------------------------- 
Daniel
# Bgtá
# Medellín 
# Cali 
#---------------------------------------------------- Colegios/ Universidades  ---------------------------------------------------------------------------------------------------------------------- 
####################### Colegios Bgtá
opq(bbox = getbb("Bogotá Colombia"))
## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="school") 
## Fijamos un objeto
osm_sf = osm %>% osmdata_sf()
osm_sf
colegios_bog = osm_sf$osm_points %>% select(osm_id,amenity) 
colegios_bog
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=colegios_bog , col="red")

####################### Universidades Bgtá
## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
osm2 = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="university") 
## Fijamos un objeto
osm_sf2 = osm2 %>% osmdata_sf()
osm_sf2 #2377 puntos y 153 poligonos
universidades_bog = osm_sf2$osm_points %>% select(osm_id,amenity) 
universidades_bog
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=universidades_bog , col="black") 
####################### Colegios Medellín
## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
osm3 = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="school") 
## Fijamos un objeto
osm_sf3 = osm3 %>% osmdata_sf()
osm_sf3
colegios_med = osm_sf3$osm_points %>% select(osm_id,amenity) 
colegios_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=colegios_med , col="red")

####################### Universidades Medellìn
## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
osm4 = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="university") 
## Fijamos un objeto
osm_sf4 = osm4 %>% osmdata_sf()
osm_sf4 
universidades_med = osm_sf4$osm_points %>% select(osm_id,amenity) 
universidades_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=universidades_med , col="black")

# Cali 
####################### Colegios cali
## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
osm3 = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="school") 
## Fijamos un objeto
osm_sf3 = osm3 %>% osmdata_sf()
osm_sf3
colegios_med = osm_sf3$osm_points %>% select(osm_id,amenity) 
colegios_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=colegios_med , col="red")
####################### Universidades cali
## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
osm4 = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="university") 
## Fijamos un objeto
osm_sf4 = osm4 %>% osmdata_sf()
osm_sf4 
universidades_med = osm_sf4$osm_points %>% select(osm_id,amenity) 
universidades_med
### Si queremos visualizarlo
leaflet() %>% addTiles() %>% addCircleMarkers(data=universidades_med , col="black")

#------------------------------------------------------ Bathrooms (k-vecinos)  ----------------------------------------------------------------------------------------------------------------------- 
# Bgtá
# Medellín 
# Cali 
#------------------------------------------------------------- Texto ---------------------------------------------------------------------------------------------------------------------- 
#Garaje/parqueadero 
# Bgtá
# Medellín 
# Cali 
#--------------------------------------------------------- Calculo variables ---------------------------------------------------------------------------------------------------------------------- 
#Train 
#Test
#-------------------------------------------------------------- Modelo ---------------------------------------------------------------------------------------------------------------------- 
#RF
#Pruebas 
#------------------------------------------------------ Estadísticas Descriptívas ---------------------------------------------------------------------------------------------------------------------- 
