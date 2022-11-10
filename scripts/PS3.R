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
    # option html
    options(htmltools.dir.version = F)
    opts_chunk$set(fig.align="center", fig.height=4 , dpi=300 , cache=F)
    remotes::install_github('ropensci/osmdata')
#2.Bases y variables--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
  ##Data como sf
    train2 <- st_as_sf(x = train, ## datos
                       coords=c("lon","lat"), ## coordenadas
                       crs=4326) ## CRS
    
    leaflet() %>% addTiles() %>% addCircleMarkers(data=train2)
    class(train2)
    
  ##Variables
    #1.Área total o cubierta
    #2.bedrooms
    #3.bathrooms (k-vecinos)
    #4.property type
    #5.Distancia al CBD
    #6.Distancia Tansporte (estaciones o vías arteriales)
    #7.Distancia Colegios, universidades, kindergarden
    #8.IPM #coordenadas
    
#3. NaN --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
    
    table(train2$rooms, train2$bedrooms)
    table(train2$rooms, train2$bedrooms)
    sum(is.na(train2$bedrooms)) #0
    sum(is.na(train2$bathrooms)) #15032
    sum(is.na(train2$surface_total)) #39044
    sum(is.na(train2$surface_covered)) #41745
    
    train2$surface_total = ifelse(is.na(train_hogares$P5130)==T,0,train_hogares$P5130)
    train_hogares$P5140 = ifelse(is.na(train_hogares$P5140)==T,0,train_hogares$P5140)
    train_hogares$Horas_trabajo1 = ifelse(is.na(train_hogares$Horas_trabajo1)==T,0,train_hogares$Horas_trabajo1)
    train_hogares$Horas_trabajo2 = ifelse(is.na(train_hogares$Horas_trabajo2)==T,0,train_hogares$Horas_trabajo2)
    train_hogares$subsidio<-(ifelse((train_hogares$subsidio>0),1,0))
    arriendo_estimado<-train_hogares$P5130+train_hogares$P5140
    train_hogares<-cbind(train_hogares,arriendo_estimado)
    
    
 
    
#------------------------------------------------------------- área -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
    #1. total=cubierta
    #2. Texto 
    train2$area<-str_detect(string = train2$description , pattern = "área") 
    table(train2$area) #5543
    train2$area2<-str_detect(string = train2$description , pattern = "area") 
    table(train2$area2) #1463
    train2$area3<-str_detect(string = train2$description , pattern = "m2") 
    table(train2$area3) #5995
    train2$area4<-str_detect(string = train2$description , pattern = "M2") 
    table(train2$area4) #4118 
    train2$area5<-str_detect(string = train2$description , pattern = "mt2") 
    table(train2$area5) #745
    train2$area6<-str_detect(string = train2$description , pattern = "Área") 
    table(train2$area6) #1836 
    train2$area7<-str_detect(string = train2$description , pattern = "mts") 
    table(train2$area7) #4544  
    train2$area8<-str_detect(string = train2$description , pattern = "Mts") 
    table(train2$area8) #1066 
    train2$area9<-str_detect(string = train2$description , pattern = "MTS") 
    table(train2$area9) #954 
    train2$area10<-str_detect(string = train2$description , pattern = "Metros")
    table(train2$area10) #118 
    train2$area11<-str_detect(string = train2$description , pattern = "METROS") 
    table(train2$area11) #380 
    train2$area12<-str_detect(string = train2$description , pattern = "metros")
    table(train2$area12) #4541 
    
    
    ### patterns
    x1 <- "[:space:]+[:digit:]+[:space:]+"
    x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
    train2$new_surface <- train2$surface_total
    
    ## replace values
    for (i in c("m2","mt2","mts2","M2","Mts2","cuadrad","mtro","mtr2","metros","METROS","Metros", "MTS","Mts","mts","MT","MT2","Mt2")){
      train2 <- train2 %>% 
        mutate(new_surface = ifelse(is.na(train2$surface_total)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
               new_surface = ifelse(is.na(train2$surface_total)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface))
    }
    
    ## clean var
    for (i in c("metros","METROS","Metros", "MTS", "Mts", "mts", "MT", "MT2", "Mt2","m2","mt2","mts2","M2","Mts2","cuadrad","mtro","mtr2"," ","\n\n")){
      train2$new_surface <- gsub(i,"",train2$new_surface)
    }
    train2$new_surface <- gsub(",",".",train2$new_surface)
    train2$new_surface <- as.numeric(train2$new_surface)
    
    
    #train2$description
    x <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
    str_locate_all(string = train2$description , pattern = x) ## detect pattern
    str_extract(string = train2$description , pattern= x) ## extrac pattern
    
    
    
    train2 <- train2 %>% 
      mutate(new_surface = str_extract(string=description , pattern= x))
    table(train2$new_surface) %>% sort() %>% head()
    
    sum(is.na(train2$new_surface))
    
    
    # "metros","METROS","Metros", "MTS", "Mts", "mts", "MT", "MT2", "Mt2",
    
    train3<-train2
    class(train3$new_surface)
    
    train3$new_surface<-as.numeric(train3$new_surface)
    
    train3$new_surface<-(ifelse(train3$new_surface >30.15,train3$new_surface,NA))
    
    train3$new_surface<-(ifelse((is.na(train3$surface_total)),train3$new_surface,train3$surface_total))
    sum(is.na(train3$surface_total))#39044
    sum(is.na(train3$new_surface))#37060
    
    train3$new_surface<-(ifelse((is.na(train3$new_surface)),train3$surface_covered,train3$new_surface))
    
    sum(is.na(train3$new_surface))#32704
    
    #imputacion missings con arboles
    train3$city<-as.factor(train3$city)
    train3$property_type<-as.factor(train3$property_type)
    train3$operation_type<-as.factor(train3$operation_type)
    
    class(train3)
    train3<-as.data.frame(train3)
    train3<- select(train3, city, surface_total,surface_covered,new_surface,rooms, bedrooms,bathrooms, property_type)
    
    imp <- missForest(train3, verbose=TRUE, variablewise= TRUE)
    imp$OOBerror #error que tiene
    
    
    noNA<-as.data.frame(imp$ximp)
    sum(is.na(noNA$new_surface))#0
    
    train2<-cbind(train2,noNA$new_surface)
    names(train2)
    
    'surface_final'->names(train2)[names(train2)=='noNA.new_surface']
    
#------------------------------------------------------ Bathrooms (arboles de decision)  ----------------------------------------------------------------------------------------------------------------------- 
    
train2<-cbind(train2,noNA$bathrooms)
names(train2)
    
'bathrooms_final'->names(train2)[names(train2)=='noNA.bathrooms']
    
    
#------------------------------------------------------------- CBD ----------------------------------------------------------------------------------------------------------------------------------------------------
#----1. Bogotá  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
####################### Delimitar Bogotá 
          Bogota <- getbb(place_name = "Bogota", 
                          featuretype = "boundary:administrative", 
                          format_out = "sf_polygon") %>% .$multipolygon

####################### Office Bgtá
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
            
####################### Retail Bgtá
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
              osm12 = opq(bbox = getbb("Bogotá Colombia")) %>%
              add_osm_feature(key="building" , value="retail") 
          ## Fijamos un objeto
            osm_sf12 = osm12 %>% osmdata_sf()
            osm_sf12
            retail_bog = osm_sf12$osm_points %>% select(osm_id) 
            retail_bog
          ### Si queremos visualizarlo
            leaflet() %>% addTiles() %>% addCircleMarkers(data=retail_bog , col="red")
          ## Interceptando industria con Bogotá
            retail_bog$intercep <- st_intersects(retail_bog, Bogota,sparse=FALSE)[,1]
            retail_bog<- retail_bog[retail_bog$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=retail_bog, col="black")
            table(retail_bog$intercep)

leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=industrial_bog, col="black")%>% addCircles(data=office_bog, col="white")%>% addCircles(data=retail_bog, col="yellow")
            
    
#----2. Medellín  -----------------------------------------------------------------------------------------------------------------------------------
####################### Delimitar Medellín 
            Medellin <- getbb(place_name = "Medellin Antioquia Colombia", 
                            featuretype = "boundary:administrative", 
                            format_out = "sf_polygon") 
            Medellin<-Medellin[1,]
            leaflet() %>% addTiles() %>% addPolygons(data=Medellin)
####################### Office Medellín
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm7 = opq(bbox = getbb("Medellin Antioquia Colombia")) %>%
            add_osm_feature(key="building" , value="office") 
          ## Fijamos un objeto
            osm_sf7 = osm7 %>% osmdata_sf()
            osm_sf7
            office_med = osm_sf7$osm_points %>% select(osm_id) 
            office_med
          ### Si queremos visualizarlo
            leaflet() %>% addTiles() %>% addCircleMarkers(data=office_med, col="red")
          ## Interceptando oficinas con Bogotá
            office_med$intercep <- st_intersects(office_med, Medellin,sparse=FALSE)[,1]
            office_med<- office_med[office_med$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=office_med, col="black")
            table(office_med$intercep)
            
####################### Industria Medellìn
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm8 = opq(bbox = getbb("Medellin Antioquia Colombia")) %>%
            add_osm_feature(key="building" , value="industrial") 
          ## Fijamos un objeto
            osm_sf8 = osm8 %>% osmdata_sf()
            osm_sf8
            industrial_med = osm_sf8$osm_points %>% select(osm_id) 
            industrial_med
          ### Si queremos visualizarlo
            leaflet() %>% addTiles() %>% addCircleMarkers(data=industrial_med , col="red")
          ## Interceptando industria con Bogotá
            industrial_med$intercep <- st_intersects(industrial_med, Medellin,sparse=FALSE)[,1]
            industrial_med<- industrial_med[industrial_med$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=industrial_med, col="black")%>% addCircles(data=office_med, col="white")
            table(industrial_med$intercep)

####################### Retail Medellín
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm13 = opq(bbox = getbb("Medellín Colombia")) %>%
              add_osm_feature(key="building" , value="retail") 
          ## Fijamos un objeto
            osm_sf13 = osm13 %>% osmdata_sf()
            osm_sf13
            retail_med = osm_sf13$osm_points %>% select(osm_id) 
            retail_med
          ### Si queremos visualizarlo
            leaflet() %>% addTiles() %>% addCircleMarkers(data=retail_med , col="red")
          ## Interceptando industria con Bogotá
            retail_med$intercep <- st_intersects(retail_med, Medellin,sparse=FALSE)[,1]
            retail_med<- retail_med[retail_med$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=retail_med, col="black")
            table(retail_med$intercep)
            
leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=industrial_med, col="black")%>% addCircles(data=office_med, col="white")%>% addCircles(data=retail_med, col="yellow")
            
            
#----3. Cali  -----------------------------------------------------------------------------------------------------------------------------------
####################### Delimitar Cali 
            Cali <- getbb(place_name = " Cali Colombia", 
                              featuretype = "boundary:administrative", 
                              format_out = "sf_polygon") 
            leaflet() %>% addTiles() %>% addPolygons(data=Cali)
            
####################### Office Cali
          ## Guardamos un amenity con el Poligono 
              osm10 = opq(bbox = getbb("Cali Colombia")) %>%
              add_osm_feature(key="building" , value="office") 
          ## Fijamos un objeto
            osm_sf10 = osm10 %>% osmdata_sf()
            osm_sf10
            office_cali = osm_sf10$osm_points %>% select(osm_id) 
            office_cali
          ### Si queremos visualizarlo
            leaflet() %>% addTiles() %>% addCircleMarkers(data=office_cali, col="red")
            ## Interceptando oficinas con Bogotá
            office_cali$intercep <- st_intersects(office_cali, Cali,sparse=FALSE)[,1]
            office_cali<- office_cali[office_cali$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=office_cali, col="black")
            table(office_cali$intercep)
            
####################### Industria Cali
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm11 = opq(bbox = getbb("Cali Colombia")) %>%
            add_osm_feature(key="building" , value="industrial") 
          ## Fijamos un objeto
            osm_sf11 = osm11 %>% osmdata_sf()
            osm_sf11
            industrial_cali = osm_sf11$osm_points %>% select(osm_id) 
            industrial_cali
          ### Si queremos visualizarlo
            leaflet() %>% addTiles() %>% addCircleMarkers(data=industrial_cali , col="red")
          ## Interceptando industria con Bogotá
            industrial_cali$intercep <- st_intersects(industrial_cali, Cali,sparse=FALSE)[,1]
            industrial_cali<- industrial_cali[industrial_cali$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=industrial_cali, col="black")%>% addCircles(data=office_cali, col="white")
            table(industrial_cali$intercep)
            
####################### Retail Medellín
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm14 = opq(bbox = getbb("Cali Colombia")) %>%
            add_osm_feature(key="building" , value="retail") 
          ## Fijamos un objeto
            osm_sf14 = osm14 %>% osmdata_sf()
            osm_sf14
            retail_cali = osm_sf14$osm_points %>% select(osm_id) 
            retail_cali
          ### Si queremos visualizarlo
            leaflet() %>% addTiles() %>% addCircleMarkers(data=retail_cali , col="red")
          ## Interceptando industria con Bogotá
            retail_cali$intercep <- st_intersects(retail_cali, Cali,sparse=FALSE)[,1]
            retail_cali<- retail_cali[retail_cali$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=retail_cali, col="black")
            table(retail_cali$intercep)
            
leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=industrial_cali, col="black")%>% addCircles(data=office_cali, col="white")%>% addCircles(data=retail_cali, col="yellow")
            
#-------------------------------------------------- Transporte (vías y estaciones) ----------------------------------------------------------------------------------------------------------------------- 
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

#-------------------------------------------------- Transporte (vías y estaciones) ----------------------------------------------------------------------------------------------------------------------- 
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
<<<<<<< HEAD
leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addPolylines(data=vias_bog, col="black")
=======
  leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addPolylines(data=vias_bog, col="black") %>% addPolylines(data=vias_bog, col="blue")
>>>>>>> 67095de1aeaa7f59019b7a4291ec1d244a280026
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
<<<<<<< HEAD
leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addPolylines(data=vias_med, col="black")
=======
  leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addPolylines(data=vias_med, col="black")%>% addPolylines(data=vias_med, col="blue")
>>>>>>> 67095de1aeaa7f59019b7a4291ec1d244a280026
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
<<<<<<< HEAD
## Interceptando industria con Bogotá
=======
  ## Interceptando industria con Medellin
  >>>>>>> 67095de1aeaa7f59019b7a4291ec1d244a280026
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
<<<<<<< HEAD
leaflet() %>% addTiles() %>% addLines(data=vias_cali , col="red")
## Interceptando vias con Bogotá
vias_cali$intercep <- st_intersects(vias_cali, Cali,sparse=FALSE)[,1]
vias_cali<- vias_cali[vias_cali$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addPolylines(data=vias_cali, col="black")
=======
  
  ## Interceptando vias con Cali
  vias_cali$intercep <- st_intersects(vias_cali, Cali,sparse=FALSE)[,1]
vias_cali<- vias_cali[vias_cali$intercep == TRUE, ]
leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addPolylines(data=vias_cali, col="black")%>% addPolylines(data=vias_cali, col="blue")




#---------------------------------------------------- Colegios/ Universidades  ---------------------------------------------------------------------------------------------------------------------- 
#----1. Bogotá  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
####################### Colegios Bgtá
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm = opq(bbox = getbb("Bogotá Colombia")) %>%
              add_osm_feature(key="amenity" , value="school") 
          ## Fijamos un objeto
            osm_sf = osm %>% osmdata_sf()
            osm_sf
            colegios_bog = osm_sf$osm_points %>% select(osm_id) 
            colegios_bog
          ### Si queremos visualizarlo
            leaflet() %>% addTiles() %>% addCircleMarkers(data=colegios_bog , col="red")
          ## Interceptando oficinas con Bogotá
            colegios_bog$intercep <- st_intersects(colegios_bog, Bogota,sparse=FALSE)[,1]
            colegios_bog<- colegios_bog[colegios_bog$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=colegios_bog, col="black")
            table(colegios_bog$intercep)
            
####################### Universidades Bgtá
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm2 = opq(bbox = getbb("Bogotá Colombia")) %>%
              add_osm_feature(key="amenity" , value="university") 
          ## Fijamos un objeto
            osm_sf2 = osm2 %>% osmdata_sf()
            osm_sf2 #2377 puntos y 153 poligonos
            universidades_bog = osm_sf2$osm_points %>% select(osm_id) 
            universidades_bog
          ### Si queremos visualizarlo
            universidades_bog$intercep <- st_intersects(universidades_bog, Bogota,sparse=FALSE)[,1]
            universidades_bog<- universidades_bog[universidades_bog$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=universidades_bog, col="black")
            table(universidades_bog$intercep)
            
  ####################### Kindergarden Bgtá
            ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm4 = opq(bbox = getbb("Bogotá Colombia")) %>%
              add_osm_feature(key="amenity" , value="kindergarten") 
            ## Fijamos un objeto
            osm_sf4 = osm4 %>% osmdata_sf()
            osm_sf4 #2377 puntos y 153 poligonos
            kinder_bog = osm_sf4$osm_points %>% select(osm_id) 
            kinder_bog
            ### Si queremos visualizarlo
            kinder_bog$intercep <- st_intersects(kinder_bog, Bogota,sparse=FALSE)[,1]
            kinder_bog<- kinder_bog[kinder_bog$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=kinder_bog, col="black")
            
            
leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addCircles(data=colegios_bog, col="black")%>% addCircles(data=universidades_bog, col="white")%>% addCircles(data=kinder_bog, col="yellow")
            
#----2. Medellín  -----------------------------------------------------------------------------------------------------------------------------------
####################### Colegios Medellín
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm5 = opq(bbox = getbb("Medellín Colombia")) %>%
            add_osm_feature(key="amenity" , value="school") 
          ## Fijamos un objeto
            osm_sf5 = osm5 %>% osmdata_sf()
            osm_sf5
            colegios_med = osm_sf5$osm_points %>% select(osm_id,amenity) 
            colegios_med
          ### Si queremos visualizarlo
            colegios_med$intercep <- st_intersects(colegios_med, Medellin,sparse=FALSE)[,1]
            colegios_med<- colegios_med[colegios_med$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=colegios_med, col="black")
            
####################### Universidades Medellìn
          ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm15 = opq(bbox = getbb("Medellín Colombia")) %>%
            add_osm_feature(key="amenity" , value="university") 
          ## Fijamos un objeto
            osm_sf15 = osm15 %>% osmdata_sf()
            osm_sf15 
            universidades_med = osm_sf15$osm_points %>% select(osm_id) 
            universidades_med
          ### Si queremos visualizarlo
            universidades_med$intercep <- st_intersects(universidades_med, Medellin,sparse=FALSE)[,1]
            universidades_med<- universidades_med[universidades_med$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=universidades_med, col="black")
            
####################### Kindergarden Medellín
            ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm16 = opq(bbox = getbb("Medellin Colombia")) %>%
              add_osm_feature(key="amenity" , value="kindergarten") 
            ## Fijamos un objeto
            osm_sf16 = osm16 %>% osmdata_sf()
            osm_sf16 #2377 puntos y 153 poligonos
            kinder_med = osm_sf16$osm_points %>% select(osm_id) 
            kinder_med
            ### Si queremos visualizarlo
            kinder_med$intercep <- st_intersects(kinder_med, Medellin,sparse=FALSE)[,1]
            kinder_med<- kinder_med[kinder_med$intercep == TRUE, ]
            leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=kinder_med, col="black")
  
leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addCircles(data=colegios_med, col="black")%>% addCircles(data=universidades_med, col="white")%>% addCircles(data=kinder_med, col="yellow")
            
#----3. Cali  -----------------------------------------------------------------------------------------------------------------------------------
####################### Colegios Cali
        ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
            osm17 = opq(bbox = getbb("Cali Colombia")) %>%
            add_osm_feature(key="amenity" , value="school") 
        ## Fijamos un objeto
          osm_sf17 = osm17 %>% osmdata_sf()
          osm_sf17
          colegios_cali = osm_sf17$osm_points %>% select(osm_id) 
          colegios_cali
        ### Si queremos visualizarlo
          colegios_cali$intercep <- st_intersects(colegios_cali, Cali,sparse=FALSE)[,1]
          colegios_cali<- colegios_cali[colegios_cali$intercep == TRUE, ]
          leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=colegios_cali, col="black")
          
####################### Universidades cali
        ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
          osm18 = opq(bbox = getbb("Cali Colombia")) %>%
          add_osm_feature(key="amenity" , value="university") 
        ## Fijamos un objeto
          osm_sf18 = osm18 %>% osmdata_sf()
          osm_sf18 
          universidades_cali = osm_sf18$osm_points %>% select(osm_id) 
          universidades_cali
        ### Si queremos visualizarlo
          universidades_cali$intercep <- st_intersects(universidades_cali, Cali,sparse=FALSE)[,1]
          universidades_cali<- universidades_cali[universidades_cali$intercep == TRUE, ]
          leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=universidades_cali, col="black")
          
####################### Kindergarden Cali
        ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
          osm19 = opq(bbox = getbb("Cali Colombia")) %>%
          add_osm_feature(key="amenity" , value="kindergarten") 
        ## Fijamos un objeto
          osm_sf19 = osm19 %>% osmdata_sf()
          osm_sf19 #2377 puntos y 153 poligonos
          kinder_cali = osm_sf19$osm_points %>% select(osm_id) 
          kinder_cali
        ### Si queremos visualizarlo
          kinder_cali$intercep <- st_intersects(kinder_cali, Cali,sparse=FALSE)[,1]
          kinder_cali<- kinder_cali[kinder_cali$intercep == TRUE, ]
          leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=kinder_cali, col="black")
      
 leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addCircles(data=colegios_cali, col="black")%>% addCircles(data=universidades_cali, col="white")%>% addCircles(data=kinder_cali, col="yellow")
          
 

#--------------------------------------------------------- Calculo variables modelo  ---------------------------------------------------------------------------------------------------------------------- 
#Train 
#Test
#-------------------------------------------------------------- Modelo ---------------------------------------------------------------------------------------------------------------------- 
#RF
#Pruebas 
#------------------------------------------------------ Estadísticas Descriptívas ---------------------------------------------------------------------------------------------------------------------- 
 
##------------------------ IPM
 ##--------------------------IMPM
 
 
 library(sf)
 
 
 IPMcali<- st_read(
   "C:/Users/HP/Downloads/OSM/VULNRB_IPMxMZ.shp")
 
 DATAIPM <- as.data.frame("IPMcali")
 
 
 muestra2 <- st_as_sf(x = muestra2, ## datos
                      coords=c("lon","lat"), ## coordenadas
                      crs=4326) ## CRS
 
 ####
 class(muestra2)
 
 dir <- system.file("shape", package="sf")
 list.files(dir, pattern="^[nc]")
 
 plot(IPM2)
 
 ############## Bogotá
 
 11001
 
 ############# Medellin
 
 05001
 
 ########## Cali
 
 76001
 
 
 ### Submuestra
 
 ## Cambiamos el Sistema de Coordenadas de IPM2
 st_transform(IPM2, "WGS84") 
 
 ## Cambiamos el Sistema de Coordenadas de IPM2
 st_transform(muestra2, 4326) 
 st_crs(muestra2) == st_crs(train2)
 st_crs(train2)
 train2
 
 st_transform(muestra2, crs=4326)
 ####
 muestra2 <- subset(IPM2, (IPM2$COD_MPIO=="05001" | IPM2$COD_MPIO=="11001" | IPM2$COD_MPIO=="76001"))
 
 table(muestra2$COD_MPIO)
 
 
 #######################################3-------
 
 ###- Paquetes
 
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
 
 
 train2 <- st_as_sf(x = train, ## datos
                    coords=c("lon","lat"), ## coordenadas
                    crs=4326) ## CRS
 
 leaflet() %>% addTiles() %>% addCircleMarkers(data=train2,color="red") %>% addPolygons(data=muestra2, color="blue")
 class(train2)
 
 leaflet() %>% addTiles() %>% addPolygons(data=muestra2, color="blue")
 
 
 train2
 #########################33333
 
 ### patterns
 x1 <- "[:space:]+[:digit:]+[:space:]+"
 x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
 train2$new_surface <- NA
 
 ## replace values
 
 muestra2$IPM <- NA
 for (i in train2$geometry ){
   
   for (j in muestra2$geometry) {
     muestra2 <- muestra2 %>%
       mutate(IPM = ifelse(st_within(i,j)==T,IPM==ipm,IPM))
   }
 }
 st_contains(muestra2$geometry, train2$geometry)
 
 ## clean var
 for (i in c("metros","METROS","Metros", "MTS", "Mts", "mts", "MT", "MT2", "Mt2","m2","mt2","mts2","M2","Mts2","cuadrad","mtro","mtr2"," ","\n\n")){
   train2$new_surface <- gsub(i,"",train2$new_surface)
 }
 train2$new_surface <- gsub(",",".",train2$new_surface)
 train2$new_surface <- as.numeric(train2$new_surface)
 
 
 ##--------------------------IPM
 ############################# IPM ###########################3
 
 train2$geometry
 muestra2$geometry
 
 st_intersection(train2$geometry[1:5], muestra2$geometry)
 
 
 #########################################################3
 library(sf)
 
 
 IPMcali<- st_read(
   "C:/Users/HP/Downloads/OSM/VULNRB_IPMxMZ.shp")
 
 DATAIPM <- as.data.frame("IPMcali")
 
 
 muestra2 <- st_as_sf(x = muestra2, ## datos
                      coords=c("lon","lat"), ## coordenadas
                      crs=4326) ## CRS
 
 ####
 class(muestra2)
 
 dir <- system.file("shape", package="sf")
 list.files(dir, pattern="^[nc]")
 
 plot(IPM2)
 
 ############## Bogotá
 
 11001
 
 ############# Medellin
 
 05001
 
 ########## Cali
 
 76001
 
 
 ### Submuestra
 
 ## Cambiamos el Sistema de Coordenadas de IPM2
 st_transform(IPM2, "WGS84") 
 
 ## Cambiamos el Sistema de Coordenadas de IPM2
 
 muestra2 = st_transform(muestra2, "EPSG:4326" )  ### Version Correcta
 
 st_crs(muestra2) == st_crs(train2)
 st_crs(train2)
 
 st_crs
 train2
 
 st_transform(muestra2, crs=4326)
 ####
 muestra2 <- subset(IPM2, (IPM2$COD_MPIO=="05001" | IPM2$COD_MPIO=="11001" | IPM2$COD_MPIO=="76001"))
 
 table(muestra2$COD_MPIO)
 
 st_crs(muestra2)
 #######################################3-------
 
 ###- Paquetes
 
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
 
 
 train2 <- st_as_sf(x = train, ## datos
                    coords=c("lon","lat"), ## coordenadas
                    crs=4326) ## CRS
 
 leaflet() %>% addTiles() %>% addCircleMarkers(data=train2,color="red") %>% addPolygons(data=muestra2, color="blue")
 class(train2)
 
 leaflet() %>% addTiles() %>% addPolygons(data=muestra2, color="blue")
 
 
 train2
 #########################33333
 
 ### patterns
 x1 <- "[:space:]+[:digit:]+[:space:]+"
 x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
 train2$new_surface <- NA
 
 ## replace values
 
 muestra2$IPM <- NA
 for (i in train2$geometry ){
   
   for (j in muestra2$geometry) {
     muestra2 <- muestra2 %>%
       mutate(IPM = ifelse(st_within(i,j)==T,IPM==ipm,IPM))
   }
 }
 st_contains(muestra2$geometry, train2$geometry)
 
 ## clean var
 for (i in c("metros","METROS","Metros", "MTS", "Mts", "mts", "MT", "MT2", "Mt2","m2","mt2","mts2","M2","Mts2","cuadrad","mtro","mtr2"," ","\n\n")){
   train2$new_surface <- gsub(i,"",train2$new_surface)
 }
 train2$new_surface <- gsub(",",".",train2$new_surface)
 train2$new_surface <- as.numeric(train2$new_surface)
 
 ################## Unir train2 y muestra2
 
 base_final <- st_join(train2,muestra2)
 
 sum (is.na(base_final$ipm))
 

aaa <- st_nearest_feature(new_house2$geometry[1], muestra2$geometry,pairwise=TRUE)

sum(is.na(new_house$ipm.x))


st_nearest_points()

new_house <- base_final[c(1:200),]

new_house2 <- subset(base_final, is.na(base_final$ipm == T) )

new_house2 <- base_final[c(1:3),]
                                                   

new_mnz = muestra2

leaflet() %>% addTiles() %>% addPolygons(data=new_mnz,col="red") %>% addCircles(data=new_house2)

na_base_final <- 
  
table(new_house$city)

install.packages("nngeo")
library(nngeo)

new_house <- st_join(x=new_house, y=new_mnz, join=st_nn, maxdist=20, k=1, progress=T)

new_house2 <- st_join(x=new_house2, y=new_mnz, join=st_nn, maxdist=20, k=1, progress=T)
##
install.packages("spdep")
library(spdep)
new_house_sp <- new_house %>% st_buffer(20) %>% as_Spatial()

nb_house = poly2nb(pl=new_house_sp, queen=T)

nb_house[[1]]


leaflet() %>% addTiles() %>% addCircles(data=new_house[32,],col="red") %>% addCircles(data=new_house[nb_house[[1]],])

new_house$rooms[1]

new_house$rooms[nb_house[[1]]]

mean(new_house$rooms[nb_house[[1]]],na.rm=T)


#### Acá termina el código !!! 10 / 11 / 22
