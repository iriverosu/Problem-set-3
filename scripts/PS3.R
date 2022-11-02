skim(train)

install.packages("tidyverse")
install.packages("pacman")
library(pacman)
require(pacman)
p_load(here,knitr,tidyverse,ggthemes,fontawesome,kableExtra)
p_load(tidyverse,rio,viridis,sf, leaflet, tmaptools)
# option html
options(htmltools.dir.version = F)
opts_chunk$set(fig.align="center", fig.height=4 , dpi=300 , cache=F)


#Variables PS3 
  #1.Área total o cubierta
  #2.bedrooms
  #3.bathrooms (k-vecinos)
  #4.property type
  #5.Distancia al CBD
  #6.Distancia Tansporte (estaciones o vías arteriales)
  #7.Distancia Colegios o universidades 
  #8.IPM #coordenadas

#data como sf
train2 <- st_as_sf(x = train, ## datos
                   coords=c("lon","lat"), ## coordenadas
                   crs=4326) ## CRS
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
# Medellín (https://www.artchitectours.es/tour/el-poblado-medellin/)
# Cali (Torre de Cali 1.5km medialuna)
#-------------------------------------------------- Transporte (vías y estaciones) ----------------------------------------------------------------------------------------------------------------------- 
Daniel
# Bgtá
# Medellín 
# Cali 
#---------------------------------------------------- Colegios/ Universidades  ---------------------------------------------------------------------------------------------------------------------- 
Luisa
# Bgtá
# Medellín 
# Cali 
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
