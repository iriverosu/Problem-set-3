#--------------------------------------------------Paquetes, lectura bases, NaN -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
#1.Paquetes--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
    install.packages("tidyverse")
    install.packages("pacman")
    install.packages("osmdata")
    install.packages("httr2")
    install.packages("sf")
    install.packages("nngeo")
    install.packages("spdep")
    install.packages("missForest")
    install.packages("caret", dependencies = c("Depends", "Suggests"))
    install.packages("Metrics")
    install.packages("ggplot2")
    install.packages("dplyr")
    install.packages("cowplot")
    library(cowplot)
    library(dplyr)
    library(ggplot2)
    library(Metrics)
    library(caret)
    library(tidyverse)
    library(missForest)
    library(spdep)
    library(nngeo)
    library(sf)
    library(httr2)
    library(osmdata)
    library(pacman)
    require(pacman)
    p_load(here,knitr,tidyverse,ggthemes,fontawesome,kableExtra)
    p_load(tidyverse,rio,viridis,sf, leaflet, tmaptools)
    p_load(tidyverse, fastDummies, caret, glmnet, MLmetrics)
    
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
    names(train2)
    test <- st_as_sf(x = test, ## datos
                       coords=c("lon","lat"), ## coordenadas
                       crs=4326) ## CRS
    
    leaflet() %>% addTiles() %>% addCircleMarkers(data=test)
    names(test)
    
    
  ##Variables
    #1.Área total o cubierta
    #2.bedrooms
    #3.bathrooms (k-vecinos)  
    #4.property type
    #5.Office
    #6.Industrial
    #7.Retail
    #6.Estaciones de bus o metro
    #7.Vías principales 
    #8.Colegios
    #9.Universidades
    #10. Kindergarden
    #11.IPM 
    
#3. NaN --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
    
    table(train2$rooms, train2$bedrooms)
    table(train2$rooms, train2$bedrooms)
    sum(is.na(train2$bedrooms)) #0
    sum(is.na(train2$bathrooms)) #15032
    sum(is.na(train2$surface_total)) #39044
#------------------------------------------ área y bathrooms por árboles de decisión y texto -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
#1. Área total----------------------------------------------------------------------------------------------------
#----------1.1 Texto 
    
              x <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
              str_locate_all(string = train2$description , pattern = x) ## detect pattern
              str_extract(string = train2$description , pattern= x) ## extrac pattern
              
              train2 <- train2 %>% 
              mutate(new_surface = str_extract(string=description , pattern= x))
              table(train2$new_surface) %>% sort() %>% head()
              sum(is.na(train2$new_surface))
              train2$new_surface<-as.numeric(train2$new_surface)
              train2$new_surface<-(ifelse(train2$new_surface >30.15,train2$new_surface,NA))
              
              train2$new_surface<-(ifelse((is.na(train2$surface_total)),train2$new_surface,train2$surface_total))
              sum(is.na(train2$surface_total))#39044
              sum(is.na(train2$new_surface))#37060
              train2$new_surface<-(ifelse((is.na(train2$new_surface)),train2$surface_covered,train2$new_surface))
              sum(is.na(train2$new_surface))#32704
----------#1.2 imputacion missings con arboles
              train2$city<-as.factor(train2$city)
              train2$property_type<-as.factor(train2$property_type)
              train2$operation_type<-as.factor(train2$operation_type)
              
              class(train2)
              train3<-as.data.frame(train2)
              train3<- train3 %>% select(city, surface_total,surface_covered,new_surface,rooms, bedrooms,bathrooms, property_type)
              
              imp <- missForest(train3, verbose=TRUE, variablewise= TRUE)
              imp$OOBerror #error que tiene
              #        PFC          MSE          MSE          MSE          MSE      MSE          MSE          PFC 
              # 0.000000e+00 8.265750e+05 1.674749e+04 5.646564e+06 2.094840e-02 0.000000e+00 1.737392e-01 0.000000e+00 
              noNA<-as.data.frame(imp$ximp)
              sum(is.na(noNA$new_surface))#0
              
              train2<-cbind(train2,noNA$new_surface)
              names(train2)
              'surface_final'->names(train2)[names(train2)=='noNA.new_surface']
    
#2. Baños---------------------------------------------------------------------------------------------------- 
              train2<-cbind(train2,noNA$bathrooms)
              names(train2)
              'bathrooms_final'->names(train2)[names(train2)=='noNA.bathrooms']
   
#Valores como enteros
              
#3  Modificar la base----------------------------------------------------------------------------------------------------------------
    train2<-train2 %>% select(-surface_total,-surface_covered,-rooms, -bathrooms, -title, -description, -operation_type, -new_surface)
    train2<-train2 %>% select(-bathrooms, -title, -description, -operation_type, -new_surface)
              
  
#1. Área total tes----------------------------------------------------------------------------------------------------
    #----------1.1 Texto 
    x <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
    str_locate_all(string = test$description , pattern = x) ## detect pattern
    str_extract(string = test$description , pattern= x) ## extrac pattern
    
    test <- test %>% 
      mutate(new_surface = str_extract(string=description , pattern= x))
    table(test$new_surface) %>% sort() %>% head()
    sum(is.na(test$new_surface))
    test$new_surface<-as.numeric(test$new_surface)
    test$new_surface<-(ifelse(test$new_surface >30.15,test$new_surface,NA))
    
    test$new_surface<-(ifelse((is.na(test$surface_total)),test$new_surface,test$surface_total))
    sum(is.na(test$surface_total))#3146
    sum(is.na(test$new_surface))#3005
    test$new_surface<-(ifelse((is.na(test$new_surface)),test$surface_covered,test$new_surface))
    sum(is.na(test$new_surface))#2723
    
    ----------#1.2 imputacion missings con arboles
    test$city<-as.factor(test$city)
    test$property_type<-as.factor(test$property_type)
    test$operation_type<-as.factor(test$operation_type)
    test$property_id<- as.numeric(test$property_id)
    
    class(test)
    test2<-as.data.frame(test)
    test2<-as.data.frame(test2)
    test2<- test2 %>% select(city, surface_total,surface_covered,new_surface,rooms, bedrooms,bathrooms, property_type)
    
    imp2 <- missForest(test2, verbose=TRUE, variablewise= TRUE)
    imp2$OOBerror #error que tiene
    #        PFC          MSE          MSE          MSE          MSE      MSE          MSE          PFC 
    #0.000000e+00 2.712978e+05 1.266396e+05 7.210981e+07 5.218208e-02 0.000000e+00 5.560255e-01 0.000000e+00     
    noNA<-as.data.frame(imp2$ximp)
    sum(is.na(noNA$new_surface))#0
    
    test<-cbind(test,noNA$new_surface)
    names(test)
    'surface_final'->names(test)[names(test)=='noNA.new_surface']
    
#2. Baños test --------------------------------------------------------------------------------------------------- 
    test<-cbind(test,noNA$bathrooms)
    names(test)
    'bathrooms_final'->names(test)[names(test)=='noNA.bathrooms']
    #Valores como enteros
    
#3  Modificar la base test ----------------------------------------------------------------------------------------------------------------
    test<-test %>% select(-surface_total,-surface_covered,-rooms, -bathrooms, -title, -description, -operation_type, -new_surface)
#-------------------------------------------------- Delimitar las ciudades ----------------------------------------------------------------------------------------------------------------------- 
#----1. Bogotá  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
      ####################### Delimitar Bogotá 
      Bogota <- getbb(place_name = "Bogota", 
                      featuretype = "boundary:administrative + admin_level= 8" ,
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


      
#-------------------------------------------------- CAI ----------------------------------------------------------------------------------------------------------------------- 
#----1. Bogotá  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
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
      
      
#----2. Medellín  -----------------------------------------------------------------------------------------------------------------------------------
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
      
#----3. Cali  -----------------------------------------------------------------------------------------------------------------------------------
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
#-----------------------------------------------CBD (office, retail e industrial) ----------------------------------------------------------------------------------------------------------------------------------------------------
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
####################### Bus station Bgtá
        ## Guardamos un amenity con el Poligono que en este caso es bogota con las estaciones de bus
        osm20 = opq(bbox = getbb("Bogotá Colombia")) %>%
        add_osm_feature(key="amenity" , value="bus_station") 
        ## Fijamos un objeto
        osm_sf20 = osm20 %>% osmdata_sf()
        osm_sf20
        bus_bog = osm_sf20$osm_points %>% select(osm_id) 
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
          osm21 = opq(bbox = getbb("Bogotá Colombia")) %>%
          add_osm_feature(key="highway" , value="primary") 
          ## Fijamos un objeto
          osm_sf21 = osm21 %>% osmdata_sf()
          osm_sf21
          vias_bog = osm_sf21$osm_lines %>% select(osm_id) 
          vias_bog
          ### Si queremos visualizarlo
          leaflet() %>% addTiles() %>% addPolylines(data=vias_bog , col="red")
          ## Interceptando vias con Bogotá
          vias_bog$intercep <- st_intersects(vias_bog, Bogota,sparse=FALSE)[,1]
          vias_bog<- vias_bog[vias_bog$intercep == TRUE, ]
          leaflet() %>% addTiles() %>% addPolygons(data=Bogota,col="red") %>% addPolylines(data=vias_bog, col="black")
          table(vias_bog$intercep)

#----2. Medellín  -----------------------------------------------------------------------------------------------------------------------------------
####################### Bus station Medellín
          ## Guardamos un amenity con el Poligono que en este caso es Medellin con las estaciones de Bus
          osm22 = opq(bbox = getbb("Medellin Antioquia Colombia")) %>%
            add_osm_feature(key="amenity" , value="bus_station") 
          ## Fijamos un objeto
          osm_sf22 = osm22 %>% osmdata_sf()
          osm_sf22
          bus_med = osm_sf22$osm_points %>% select(osm_id) 
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
        osm23 = opq(bbox = getbb("Medellin Antioquia Colombia")) %>%
          add_osm_feature(key="highway" , value="primary") 
        ## Fijamos un objeto
        osm_sf23 = osm23 %>% osmdata_sf()
        osm_sf23
        vias_med = osm_sf23$osm_lines %>% select(osm_id) 
        vias_med
        ### Si queremos visualizarlo
        leaflet() %>% addTiles() %>% addPolylines(data=vias_med , col="red")
        ## Interceptando vias con Medellin
        vias_med$intercep <- st_intersects(vias_med, Medellin,sparse=FALSE)[,1]
        vias_med<- vias_med[vias_med$intercep == TRUE, ]
        leaflet() %>% addTiles() %>% addPolygons(data=Medellin,col="red") %>% addPolylines(data=vias_med, col="black")
####################### Metro Medellín

        ## Guardamos un amenity con el Poligono que en este caso es bogota con los coelgios 
        osm24 = opq(bbox = getbb("Medellin Antioquia Colombia")) %>%
          add_osm_feature(key="railway" , value="subway") 
        ## Fijamos un objeto
        osm_sf24 = osm24 %>% osmdata_sf()
        osm_sf24
        metro_med = osm_sf24$osm_points %>% select(osm_id) 
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
        osm25 = opq(bbox = getbb("Cali Colombia")) %>%
          add_osm_feature(key="amenity" , value="bus_station") 
        ## Fijamos un objeto
        osm_sf25 = osm25 %>% osmdata_sf()
        osm_sf25
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
        osm26 = opq(bbox = getbb("Cali Colombia")) %>%
          add_osm_feature(key="highway" , value="primary") 
        ## Fijamos un objeto
        osm_sf26 = osm26 %>% osmdata_sf()
        
        osm_sf26
        vias_cali = osm_sf26$osm_lines %>% select(osm_id) 
        vias_cali
        ### Si queremos visualizarlo
        leaflet() %>% addTiles() %>% addLines(data=vias_cali , col="red")
        ## Interceptando vias con Bogotá
        vias_cali$intercep <- st_intersects(vias_cali, Cali,sparse=FALSE)[,1]
        vias_cali<- vias_cali[vias_cali$intercep == TRUE, ]
        leaflet() %>% addTiles() %>% addPolygons(data=Cali,col="red") %>% addPolylines(data=vias_cali, col="black")

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
          
 

#------------------------------------------------------  IPM ---------------------------------------------------------------------------------------------------------------------- 
         IPM<- st_read("C:/Users/HP/Downloads/OSM/VULNRB_IPMxMZ.shp")
         DATAIPM <- as.data.frame("IPMcali")
         muestra2 <- st_as_sf(x = muestra2, ## datos
                              coords=c("lon","lat"), ## coordenadas
                              crs=4326) ## CRS
         ####
         class(muestra2)
         dir <- system.file("shape", package="sf")
         list.files(dir, pattern="^[nc]")
    
         ## Cambiamos el Sistema de Coordenadas de IPM2
         muestra2 = st_transform(muestra2, "EPSG:4326" )  ### Version Correcta
         st_crs(muestra2) == st_crs(train2)
         
         ####
         muestra2 <- subset(IPM2, (IPM2$COD_MPIO=="05001" | IPM2$COD_MPIO=="11001" | IPM2$COD_MPIO=="76001"))
         table(muestra2$COD_MPIO)
         
         ################## Unir train2 y muestra2
         
         base_final <- st_join(train2,muestra2)
         sum (is.na(base_final$ipm))
         na_houses<-subset(base_final, is.na(base_final$ipm==T))
         ## Distancia a muchos polygonos
         matrix_dist_mnz<- st_nearest_points(x=na_houses , y=muestra2)
         matrix_dist_parque[1:5,1:5]
         mean_dist_parque <- apply(matrix_dist_parque , 1 , mean)
         mean_dist_parque %>% head()
         house_chapi$dist_parque = mean_dist_parque
        
 

#--------------------------------------------------------- Calculo variables modelo  ---------------------------------------------------------------------------------------------------------------------- 
#----1. Medellín  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
#-------Distancia universidades
         train_medellin<-subset(train2,city=="Medellín")
         ## Distancia a muchos puntos
         matrix_dist_uni_med <- st_distance(x=train_medellin , y=universidades_med)
         matrix_dist_uni_med[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_uni_med <- apply(matrix_dist_uni_med , 1 , min)
         min_dist_uni_med %>% head()
         #pegar a dataframe general
         train_medellin$dist_uni = min_dist_uni_med
         
#-------Distancia colegios
         ## Distancia a muchos puntos
         matrix_dist_col_med <- st_distance(x=train_medellin , y=colegios_med)
         matrix_dist_col_med[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_col_med <- apply(matrix_dist_col_med , 1 , min)
         min_dist_col_med %>% head()
         #pegar a dataframe general
         train_medellin$dist_col = min_dist_col_med
         
#-------Distancia kindergarden
         ## Distancia a muchos puntos
         matrix_dist_kin_med <- st_distance(x=train_medellin , y=kinder_med)
         matrix_dist_kin_med[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_kin_med <- apply(matrix_dist_kin_med , 1 , min)
         min_dist_kin_med %>% head()
         #pegar a dataframe general
         train_medellin$dist_kin = min_dist_kin_med

#-------Distancia estaciones de bus
         ## Distancia a muchos puntos
         matrix_dist_bus_med <- st_distance(x=train_medellin , y=bus_med)
         matrix_dist_bus_med[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_bus_med <- apply(matrix_dist_bus_med , 1 , min)
         min_dist_bus_med %>% head()
         #pegar a dataframe general
         train_medellin$dist_bus = min_dist_bus_med
         
#-------Distancia vías principales
         ## Distancia a muchos puntos
         matrix_dist_vias_med <- st_distance(x=train_medellin , y=vias_med)
         matrix_dist_vias_med[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_vias_med <- apply(matrix_dist_vias_med , 1 , min)
         min_dist_vias_med %>% head()
         #pegar a dataframe general
         train_medellin$dist_vias = min_dist_vias_med
#-------Distancia office
         ## Distancia a muchos puntos
         matrix_dist_off_med <- st_distance(x=train_medellin , y=office_med)
         matrix_dist_off_med[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_off_med <- apply(matrix_dist_off_med , 1 , min)
         min_dist_off_med %>% head()
         #pegar a dataframe general
         train_medellin$dist_off = min_dist_off_med
         
#-------Distancia Industrial
         ## Distancia a muchos puntos
         matrix_dist_ind_med <- st_distance(x=train_medellin , y=industrial_med)
         matrix_dist_ind_med[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_ind_med <- apply(matrix_dist_ind_med , 1 , min)
         min_dist_ind_med %>% head()
         #pegar a dataframe general
         train_medellin$dist_ind = min_dist_ind_med
         
#-------Distancia retail
         ## Distancia a muchos puntos
         matrix_dist_ret_med <- st_distance(x=train_medellin , y=retail_med)
         matrix_dist_ret_med[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_ret_med <- apply(matrix_dist_ret_med , 1 , min)
         min_dist_ret_med %>% head()
         #pegar a dataframe general
         train_medellin$dist_ret = min_dist_ret_med
         
#-------Distancia CAI
         ## Distancia a muchos puntos
         matrix_dist_cai_med <- st_distance(x=train_medellin , y=police_med)
         matrix_dist_cai_med[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_cai_med <- apply(matrix_dist_cai_med , 1 , min)
         min_dist_cai_med %>% head()
         #pegar a dataframe general
         train_medellin$dist_cai = min_dist_cai_med
         
         
         

         
#----2. Bogotá  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
#-------Distancia universidades
         train_bog<-subset(train2,city=="Bogotá D.C")
         ## Distancia a muchos puntos
         matrix_dist_uni_bog <- st_distance(x=train_bog , y=universidades_bog)
         matrix_dist_uni_bog[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_uni_bog <- apply(matrix_dist_uni_bog , 1 , min)
         min_dist_uni_bog %>% head()
         #pegar a dataframe general
         train_bog$dist_uni = min_dist_uni_bog
         
#-------Distancia colegios
         ## Distancia a muchos puntos
         matrix_dist_col_bog <- st_distance(x=train_bog , y=colegios_bog)
         matrix_dist_col_bog[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_col_bog <- apply(matrix_dist_col_bog , 1 , min)
         min_dist_col_bog %>% head()
         #pegar a dataframe general
         train_bog$dist_col = min_dist_col_bog
         
#-------Distancia kindergarden
         ## Distancia a muchos puntos
         matrix_dist_kin_bog <- st_distance(x=train_bog , y=kinder_bog)
         matrix_dist_kin_bog[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_kin_bog <- apply(matrix_dist_kin_bog , 1 , min)
         min_dist_kin_bog %>% head()
         #pegar a dataframe general
         train_bog$dist_kin = min_dist_kin_bog
         
#-------Distancia estaciones de bus
         ## Distancia a muchos puntos
         matrix_dist_bus_bog <- st_distance(x=train_bog , y=bus_bog)
         matrix_dist_bus_bog[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_bus_bog <- apply(matrix_dist_bus_bog , 1 , min)
         min_dist_bus_bog %>% head()
         #pegar a dataframe general
         train_bog$dist_bus = min_dist_bus_bog
         
#-------Distancia vías principales
         ## Distancia a muchos puntos
         matrix_dist_vias_bog <- st_distance(x=train_bog , y=vias_bog)
         matrix_dist_vias_bog[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_vias_bog <- apply(matrix_dist_vias_bog , 1 , min)
         min_dist_vias_bog %>% head()
         #pegar a dataframe general
         train_bog$dist_vias = min_dist_vias_bog
         
#-------Distancia office
         ## Distancia a muchos puntos
         matrix_dist_off_bog <- st_distance(x=train_bog , y=office_bog)
         matrix_dist_off_bog[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_off_bog <- apply(matrix_dist_off_bog , 1 , min)
         min_dist_off_bog %>% head()
         #pegar a dataframe general
         train_bog$dist_off = min_dist_off_bog
         
#-------Distancia Industrial
         ## Distancia a muchos puntos
         matrix_dist_ind_bog <- st_distance(x=train_bog , y=industrial_bog)
         matrix_dist_ind_bog[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_ind_bog <- apply(matrix_dist_ind_bog , 1 , min)
         min_dist_ind_bog %>% head()
         #pegar a dataframe general
         train_bog$dist_ind = min_dist_ind_bog
         
#-------Distancia retail
         ## Distancia a muchos puntos
         matrix_dist_ret_bog <- st_distance(x=train_bog , y=retail_bog)
         matrix_dist_ret_bog[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_ret_bog <- apply(matrix_dist_ret_bog , 1 , min)
         min_dist_ret_bog %>% head()
         #pegar a dataframe general
         train_bog$dist_ret = min_dist_ret_bog
         
#-------Distancia CAI
         ## Distancia a muchos puntos
         matrix_dist_cai_bog <- st_distance(x=train_bog , y=police_bog)
         matrix_dist_cai_bog[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_cai_bog <- apply(matrix_dist_cai_bog , 1 , min)
         min_dist_cai_bog %>% head()
         #pegar a dataframe general
         train_bog$dist_cai = min_dist_cai_bog
        
#----3. Cali -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
#-------Distancia universidades
         ## Distancia a muchos puntos
         matrix_dist_uni_cali <- st_distance(x=test , y=universidades_cali)
         matrix_dist_uni_cali[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_uni_cali <- apply(matrix_dist_uni_cali , 1 , min)
         min_dist_uni_cali%>% head()
         #pegar a dataframe general
         test$dist_uni = min_dist_uni_cali
#-------Distancia colegios
         ## Distancia a muchos puntos
         matrix_dist_col_cali <- st_distance(x=test , y=colegios_cali)
         matrix_dist_col_cali[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_col_cali <- apply(matrix_dist_col_cali , 1 , min)
         min_dist_col_cali%>% head()
         #pegar a dataframe general
         test$dist_col = min_dist_col_cali
         
#-------Distancia kindergarden
         ## Distancia a muchos puntos
         matrix_dist_kin_cali <- st_distance(x=test , y=kinder_cali)
         matrix_dist_kin_cali[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_kin_cali <- apply(matrix_dist_kin_cali , 1 , min)
         min_dist_kin_cali %>% head()
         #pegar a dataframe general
         test$dist_kin = min_dist_kin_cali
         
#-------Distancia estaciones de bus
         ## Distancia a muchos puntos
         matrix_dist_bus_cali <- st_distance(x=test , y=bus_cali)
         matrix_dist_bus_cali[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_bus_cali <- apply(matrix_dist_bus_cali , 1 , min)
         min_dist_bus_cali%>% head()
         #pegar a dataframe general
         test$dist_bus = min_dist_bus_cali
         
#-------Distancia vías principales
         ## Distancia a muchos puntos
         matrix_dist_vias_cali <- st_distance(x=test , y=vias_cali)
         matrix_dist_vias_cali[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_vias_cali <- apply(matrix_dist_kin_cali , 1 , min)
         min_dist_vias_cali%>% head()
         #pegar a dataframe general
         test$dist_vias = min_dist_vias_cali
         
#-------Distancia office
         ## Distancia a muchos puntos
         matrix_dist_off_cali <- st_distance(x=test , y=office_cali)
         matrix_dist_off_cali[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_off_cali <- apply(matrix_dist_off_cali , 1 , min)
         min_dist_off_cali %>% head()
         #pegar a dataframe general
         test$dist_off = min_dist_off_cali
         
#-------Distancia Industrial
         ## Distancia a muchos puntos
         matrix_dist_ind_cali <- st_distance(x=test, y=industrial_cali)
         matrix_dist_ind_cali[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_ind_cali<- apply(matrix_dist_ind_cali , 1 , min)
         min_dist_ind_cali %>% head()
         #pegar a dataframe general
         test$dist_ind = min_dist_ind_cali
         
#-------Distancia retail
         ## Distancia a muchos puntos
         matrix_dist_ret_cali <- st_distance(x=test , y=retail_cali)
         matrix_dist_ret_cali[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_ret_cali <- apply(matrix_dist_ret_cali , 1 , min)
         min_dist_ret_cali %>% head()
         #pegar a dataframe general
         test$dist_ret = min_dist_ret_cali
         
#-------Distancia CAI
         ## Distancia a muchos puntos
         matrix_dist_cai_cali <- st_distance(x=test , y=police_cali)
         matrix_dist_cai_cali[1:5,1:5]
         #Distancia al punto más cercano
         min_dist_cai_cali <- apply(matrix_dist_cai_cali , 1 , min)
         min_dist_cai_cali %>% head()
         #pegar a dataframe general
         test$dist_cai = min_dist_cai_cali
         
#--------------------------------------------------------- Base final train y test  ---------------------------------------------------------------------------------------------------------------------- 
#-------Base definitiva train
         train_bog<-as.data.frame((train_bog))
         train_medellin<-as.data.frame(train_medellin)
         train_final <- rbind(train_bog,train_medellin)
         
         train_final <- select(train_final,-property_id, -city)
         test <- select(test,-property_id, -city)
         
         train_final$bathrooms_final <- as.integer(train_final$bathrooms_final)
         test$bathrooms_final <- as.integer(test$bathrooms_final)
         test$surface_final <- as.integer(test$surface_final)
         
#-------División de muestra train y test (de Bog y Med)
         
         set.seed(12345) 
         train_final <- train_final %>%
           mutate(holdout= as.logical(1:nrow(train_final) %in%
                                        sample(nrow(train_final), nrow(train_final)*.2)))
         test_rf<-train_final[train_final$holdout==T,] #10.287
         train_rf<-train_final[train_final$holdout==F,] #41.150
         train_rf$surface_final <- as.integer(train_rf$surface_final)
         

         
#-------------------------------------------------------------- Modelo ---------------------------------------------------------------------------------------------------------------------- 
#----1.O L S ----------------------------------------------------------------------------------------------------------------
         
         x<- select(train_rf,-price,-geometry,-holdout)
         y<-select(train_rf, price)
         y<- log(y)
         train_ols<-cbind(x,y)
         train_ols$property_type<-as.factor(train_ols$property_type)
         set.seed(12345)
         ols <- train(price~ property_type+ bedrooms+surface_final+ bathrooms_final+ dist_cai+dist_bus+ dist_ret+dist_ind+
                      dist_off + dist_vias+ dist_kin+dist_col+ dist_uni,# model to fit
                      data = train_ols,
                      trControl = trainControl(method = "cv", number = 10),
                      method = "lm")

         ols$finalModel$coefficients
         #intercept      RMSE  Rsquared      MAE    RMSESD    RsquaredSD    MAESD
         #    TRUE   0.514107 0.4458714 0.3935926 0.005537195 0.01589225 0.004633498
         RMSE      Rsquared   MAE      
         0.514107  0.4458714  0.3935926
         
#-----1.1 .Gráfico de coeficientes OLS 
         
         df_coeficientes_reg<- ols$finalModel$coefficients %>%
           enframe(name = "predictor", value = "coeficiente")
         
         df_coeficientes_reg[-1,] %>%
           filter(predictor != "`(Intercept)`") %>%
           ggplot(aes(x = reorder(predictor, abs(coeficiente)), 
                      y = coeficiente)) +
           geom_col(fill = "darkblue") +
           coord_flip() +
           labs(title = "Coeficientes del modelo de regresión", 
                x = "Variables",
                y = "Coeficientes") +
           theme_bw()
         
#-----1.2 Resultados test 
         y_ols_test<-predict(ols,newdata=test_rf)
         RMSE1<-rmse(log(test_rf$price), y_ols_test)#0.5190467
         MAE1<-mae(log(test_rf$price), y_ols_test)#0.3972433
         RSQUARE = function(y_actual,y_predict){
             cor(y_actual,y_predict)^2
         }
         RSQUARE(log(test_rf$price), y_ols_test) #0.4492948
         
#-----2. Forwad selection         
         forward <- train(price ~ ., data = train_ols,
                          method = "leapForward",
                          trControl = trainControl(method = "cv", number = 10))
         forward
         
         nvmax  RMSE       Rsquared   MAE      
         2      0.5329273  0.4043518  0.4070229
         3      0.5280398  0.4151745  0.4024380
         4      0.5227795  0.4267756  0.3994420
         
#------2.1 Resultados en test 
         
         y_forward_test<-predict(forward,newdata=test_rf)
         RMSE1<-rmse(log(test_rf$price), y_forward_test)#0.526729
         MAE1<-mae(log(test_rf$price), y_forward_test)#0.4025454
         RSQUARE(log(test_rf$price), y_forward_test) #0.4327536
         
#-----3. Backward selection
         backwards <- train(price ~ ., data = train_ols,
                            method = "leapBackward",
                            trControl = trainControl(method = "cv", number = 10))
         backwards
         
         
         nvmax  RMSE       Rsquared   MAE      
         2      0.5329517  0.4044860  0.4070588
         3      0.5280726  0.4153445  0.4024745
         4      0.5228242  0.4269313  0.3994806
         
#--------3.1 Resultados en test 
         y_backwards_test<-predict(backwards,newdata=test_rf)
         RMSE1<-rmse(log(test_rf$price), y_backwards_test)#0.526729
         MAE1<-mae(log(test_rf$price), y_backwards_test)#0.4025454
         RSQUARE(log(test_rf$price), y_backwards_test) #0.4327536
         
         
#----4.E L A S T I C   N E T --------------------------------------------------            
         # Model Building : Elastic Net Regression
         custom <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 5,
                                verboseIter = TRUE)
         set.seed(12345)
         en <- train(price~.,
                     data=train_ols,
                     method='glmnet',
                     tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                           lambda = seq(0.0001,0.2,length=5)),
                     trControl=custom,
                     preProcess = c("center", "scale")
                     )
#-------4.1 Resultados 
         en
         mean(en$resample$RMSE) 0.5141376
         mean(en$results$MAE) 0.4131547
         mean(en$results$Rsquared) 0.4233886
        
         alpha      lambda    RMSE       Rsquared   MAE      
         0.0000000  0.000100  0.5148004  0.4453253  0.3946089
         Fitting alpha = 1, lambda = 1e-04 on full training set

#--------4.2 Ploting EN
         plot(en, main = "Elastic Net Regression")
         #plotting important variables
         plot(varImp(en,scale=TRUE))

#--------4.3 Resultados en test 
         y_en_test<-predict(en,newdata=test_rf)
         RMSE1<-rmse(log(test_rf$price), y_en_test)#0.5190552
         MAE1<-mae(log(test_rf$price), y_en_test)#0.3972199
         RSQUARE(log(test_rf$price), y_en_test) #0.4493329
          
#----3.R F ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
         install.packages("ranger")
         library(ranger)
         
         #base para reemplazo del modelo
         x_continuas=(train_ols[c(1,3,6,7,8,4,5,9,10,11,12,13,14)])
         x_categoricas=(train_ols[c(2)])
        # Ahora procedemos a dummyficar la base
         x_categoricas<- model.matrix(~ ., x_categoricas) %>%
                          as.data.frame 
         x<- cbind(x_categoricas,x_continuas)
         x=(x[c(-1)])
         x$property_typeCasa<-as.factor(x$property_typeCasa)
         
#------ Creamos una grilla para tunear el random forest
                        set.seed(12345)
                        cv3 <- trainControl(number = 3, method = "cv")
                        tunegrid_rf <- expand.grid(mtry = c(3, 5, 10), 
                                                   min.node.size = c(10, 30, 50,
                                                                     70, 100),
                                                   splitrule="variance"
                        )
                        
                        cv3 <- trainControl(number = 3, method = "cv")
                        tunegrid_rf <- expand.grid(mtry = c(3), 
                                                   min.node.size = c(10),
                                                   splitrule="variance"
                        )
                        
#------ Modelo Random forest
                        
                        modeloRF <- train( price~ .,
                                          data = x, 
                                          method = "ranger", 
                                          trControl = cv3,
                                          metric = 'Acurracy', 
                                          verbose = TRUE,
                                          preProcess= c("center", "scale"),
                                          tuneGrid = tunegrid_rf)
#-------Resultados
                        modeloRF
                        mean(modeloRF$results$RMSE) 0.363424 0.3405719 
                        mean(modeloRF$results$MAE) 0.2631535 0.2400904
                        mean(modeloRF$results$Rsquared) 0.728282  0.7618303
                        RMSE was used to select the optimal model using the smallest value.
                        The final values used for the model were mtry = 3, splitrule = variance and min.node.size = 10.
                        
#--------------## Visualize variable importance 
                        
                        plot(modeloRF)
                        varImp(modeloRF)
                        
                        install.packages("ranger")
                        library(ranger)
                        modelo  <- ranger(
                          formula   = price~ .,
                          data      = x,
                          num.trees = 10,
                          seed      = 12345,
                          importance= "impurity"
                        )
                        predicciones <- predict(
                          modelo,
                          data = test_rf
                        )
                        
                        test_rmse    <- sqrt(mean((predicciones - log(test_rf$price))^2))
                        paste("Error de test (rmse) del modelo:", round(test_rmse,2)) #0.35
        
                        # Get variable importance from the model fi
                        importancia_pred <- modelo$variable.importance %>%
                          enframe(name = "predictor", value = "importancia")
                        
                        # Gráfico
                        grafico1<- ggplot(
                          data = importancia_pred,
                          aes(x    = reorder(predictor, importancia),
                              y    = importancia,
                              fill = importancia)
                        ) +
                          labs(x = "predictor", title = "Importancia predictores (permutación)") +
                          geom_col() +
                          coord_flip() +
                          theme_bw() +
                          theme(legend.position = "none")
                        
                        grafico2<-ggplot(
                          data = importancia_pred,
                          aes(x    = reorder(predictor, importancia),
                              y    = importancia,
                              fill = importancia)
                        ) +
                          labs(x = "predictor", title = "Importancia predictores (pureza de nodos)") +
                          geom_col() +
                          coord_flip() +
                          theme_bw() +
                          theme(legend.position = "none")
                        
                        plot_grid(grafico1, grafico2, nrow  = 1, ncol=2, labels="AUTO")
                        
                        
#--------3.3 Resultados en test 
                        'property_typeCasa'->names(test_rf)[names(test_rf)=='property_type']
                        test_rf$property_typeCasa<-ifelse(test_rf$property_typeCasa=="Casa",1,0)
                        test_rf$property_typeCasa<-as.factor(test_rf$property_typeCasa)
                        y_rf_test<-predict(modeloRF,newdata=test_rf)
                        RMSE1<-rmse(log(test_rf$price), y_rf_test)#0.327371
                        MAE1<-mae(log(test_rf$price), y_rf_test)#0.224281
                        RSQUARE(log(test_rf$price), y_rf_test) #0.7848002
                        
   #--------3.4 Predicciones gráfico
                        test_rf$y_rf_test <- y_rf_test
                        pl1 <-test_rf %>% 
                          ggplot(aes(log(price),y_xg_test)) +
                          geom_point(alpha=0.5) + 
                          stat_smooth(aes(Precio='predicho')) +
                          xlab('Precio real') +
                          ylab('Precio predicho')+
                          theme_bw()
                        
                        ggplotly(pl1)
                        
          
#------ 4. XGBOOTS ---------------------------------------------------------------------------------------------------------------
                        
                       install.packages("xgboost")
                       library(xgboost)
                        grid_default <- expand.grid(nrounds = c(250,500),
                                                    max_depth = c(4,6,8),
                                                    eta = c(0.01,0.3,0.5),
                                                    gamma = c(0,1),
                                                    min_child_weight = c(10, 25,50),
                                                    colsample_bytree = c(0.7),
                                                    subsample = c(0.6))
                        
                        ctrl<- trainControl(method = "cv",
                                                 number = 5,
                                                 summaryFunction = defaultSummary,
                                                 classProbs = TRUE,
                                                 verbose=FALSE,
                                                 savePredictions = T)
                        set.seed(1410)
                        xgboost <- train(
                          price ~.,
                          data=x,
                          method = "xgbTree",
                          trControl = ctrl,
                          metric = "RMSE",
                          tuneGrid = grid_default,
                          preProcess = c("center", "scale")
                        )
                        
                        Tuning parameter 'colsample_bytree' was held constant at a value of 0.7
                        Tuning parameter 'subsample' was held constant at a value of 0.6
                        RMSE was used to select the optimal model using the smallest value.
                        The final values used for the model were nrounds = 500, max_depth = 8, eta = 0.3, gamma = 0, colsample_bytree =
                          0.7, min_child_weight = 50 and subsample = 0.6.
                        xgboost$
                          
                        
                        min(xgboost$results$RMSE) #0.347947
                        min(xgboost$results$MAE) #0.2450082
                        max(xgboost$results$Rsquared) #0.7478363
                        
#--------3.3 Resultados en test 
                        y_xg_test<-predict(xgboost,newdata=test_rf)
                        RMSE1<-rmse(log(test_rf$price), y_xg_test)#0.3398501
                        MAE1<-mae(log(test_rf$price), y_xg_test)#0.2392797
                        RSQUARE(log(test_rf$price), y_xg_test) #0.7647703
                        
#--------3.3 Gráfico de resultados  
                      resultados %>% mutate(modelo = fct_relevel(modelo, 
                                                               "RANDOM FOREST","XG-BOOST","OLS", "ELASTIC NET", "FORWARD", "BACWARD")) %>%
                      install.packages("cowplot")
                        library(cowplot)  
                         Barra1<- ggplot(resultados,aes(modelo,RMSE))+ geom_bar(width = 0.9, stat="identity",             
                                                                        position = position_dodge())+ ylim(c(0,0.75))+
                            labs(x="Modelo de regresión", y= "RMSE \n (Fuera de muestra)") +
                            labs(fill = "")+ theme_linedraw()+facet_grid(~"Diagrama de barras para el RMSE")+
                            geom_text(aes(label=RMSE), vjust=0.9, hjust=1.2, color="white",position = position_dodge(0.9),size=4.0) + 
                            theme_bw(base_size=15)        + theme_light() +coord_flip()   +scale_fill_manual(values=brewer.pal(n = 3, name = "Accent"))    
                          
                         Barra2<- ggplot(resultados,aes(modelo,Rsquared))+ geom_bar(width = 0.9, stat="identity",              
                                                                                position = position_dodge())+ ylim(c(0,1))+
                           labs(x="Modelo de regresión", y= "Rcuadrado \n (Fuera de muestra)") +
                           labs(fill = "darkblue")+ theme_linedraw()+facet_grid(~"Diagrama de barras para el Rcuadrado")+
                           geom_text(aes(label=Rsquared), vjust=0.9, hjust=1.2, color="white",position = position_dodge(0.9),size=4.0) + 
                           theme_bw(base_size=15)        + theme_light() +coord_flip()  +scale_fill_manual(values=brewer.pal(n = 3, name = "Accent"))  
                         
                    
                      plot_grid(Barra1, Barra2, nrow  = 1, ncol=2, labels="AUTO")
                         
#------------------------------------------------------ Estadísticas Descriptívas ---------------------------------------------------------------------------------------------------------------------- 
               
                      library(ggplot2)
                      install.packages("corrplot")
                      library(corrplot)
                      library(mlbench)
                      install.packages("Amelia")
                      library(Amelia)
                      install.packages("plotly")
                      library(plotly)
                      library(reshape2)
                      library(caret)
                      library(caTools)
                      library(dplyr) 
#Correlación entre variables
                      corrplot(cor(select(x,-property_typeCasa)))
#Effect of the variables in the dataframe on price.
                        x %>%
                        select(c(-property_typeCasa)) %>%
                        melt(id.vars = "price") %>%
                        ggplot(aes(x = value, y = price, colour = variable)) +
                        geom_point(alpha = 0.7) +
                        stat_smooth(aes(colour = "black")) +
                        facet_wrap(~variable, scales = "free", ncol = 2) +
                        labs(x = "Variable Value", y = "Median House Price ($1000s)") +
                        theme_minimal()
                        
                        
                        
#------------------------------------------------------ Depurar bases  ---------------------------------------------------------------------------------------------------------------------- 
         
         rm(train_cali)
         rm(administrative)
         rm(Bogota)
         rm(bus_bog)
         rm(bus_cali)
         rm(bus_med)
         rm(Cali)
         rm(Medellin)
         rm(colegios_bog)
         rm(colegios_cali)
         rm(colegios_med)
         rm(imp)
         rm(imp2)
         rm(industrial_bog)
         rm(industrial_cali)
         rm(industrial_med)
         rm(kinder_bog)
         rm(kinder_cali)
         rm(kinder_med)
         rm(metro_med)
         rm(office_bog)
         rm(office_cali)
         rm(office_med)
         rm(osm)
         rm(osm_sfp2)
         rm(osm_p2)
         rm(osm_sfp3)
         rm(osm_p3)
         rm(osm_sfp)
         rm(osm_p)
         rm(osm_sf)
         rm(osm_sf10)
         rm(osm_sf11)
         rm(osm_sf12)
         rm(osm_sf13)
         rm(osm_sf14)
         rm(osm_sf15)
         rm(osm_sf16)
         rm(osm_sf17)
         rm(osm_sf18)
         rm(osm_sf19)
         rm(osm_sf20)
         rm(osm_sf2)
         rm(osm_sf21)
         rm(osm_sf22)
         rm(osm_sf23)
         rm(osm_sf24)
         rm(osm_sf25)
         rm(osm_sf26)
         rm(osm_sf3)
         rm(osm_sf4)
         rm(osm_sf5)
         rm(osm_sf6)
         rm(osm_sf7)
         rm(osm_sf8)
         rm(osm_sf9)
         rm(osm10)
         rm(osm11)
         rm(osm12)
         rm(osm13)
         rm(osm14)
         rm(osm15)
         rm(osm16)
         rm(osm17)
         rm(osm18)
         rm(osm19)
         rm(osm2)
         rm(osm20)
         rm(osm21)
         rm(osm22)
         rm(osm23)
         rm(osm24)
         rm(osm25)
         rm(osm26)
         rm(osm3)
         rm(osm4)
         rm(osm5)
         rm(osm6)
         rm(osm7)
         rm(osm8)
         rm(osm9)
         rm(retail_bog)
         rm(retail_cali)
         rm(retail_med)
         rm(test2)
         rm(train)
         rm(train2)
         rm(train3)
         rm(universidades_bog)
         rm(universidades_med)
         rm(universidades_cali)
         rm(vias_bog)
         rm(vias_med)
         rm(vias_cali)
         rm(noNA)
         rm(matrix_dist_cai_cali)
         rm(matrix_dist_cai_bog)
         rm(matrix_dist_cai_med)
         rm(matrix_dist_bus_bog)
         rm(matrix_dist_bus_med)
         rm(matrix_dist_bus_cali)
         rm(matrix_dist_col_bog)
         rm(matrix_dist_col_med)
         rm(matrix_dist_col_cali)
         rm(matrix_dist_ind_bog)
         rm(matrix_dist_ind_med)
         rm(matrix_dist_ind_cali)
         rm(matrix_dist_kin_bog)
         rm(matrix_dist_kin_med)
         rm(matrix_dist_kin_cali)
         rm(matrix_dist_off_bog)
         rm(matrix_dist_off_med)
         rm(matrix_dist_off_cali)
         rm(matrix_dist_ret_bog)
         rm(matrix_dist_ret_med)
         rm(matrix_dist_ret_cali)
         rm(matrix_dist_uni_bog)
         rm(matrix_dist_uni_med)
         rm(matrix_dist_uni_cali)
         rm(matrix_dist_vias_bog)
         rm(matrix_dist_vias_med)
         rm(matrix_dist_vias_cali)
         
         rm(min_dist_cai_cali)
         rm(min_dist_cai_bog)
         rm(min_dist_cai_med)
         rm(min_dist_bus_bog)
         rm(min_dist_bus_med)
         rm(min_dist_bus_cali)
         rm(min_dist_col_bog)
         rm(min_dist_col_med)
         rm(min_dist_col_cali)
         rm(min_dist_ind_bog)
         rm(min_dist_ind_med)
         rm(min_dist_ind_cali)
         rm(min_dist_kin_bog)
         rm(min_dist_kin_med)
         rm(min_dist_kin_cali)
         rm(min_dist_off_bog)
         rm(min_dist_off_med)
         rm(min_dist_off_cali)
         rm(min_dist_ret_bog)
         rm(min_dist_ret_med)
         rm(min_dist_ret_cali)
         rm(min_dist_uni_bog)
         rm(min_dist_uni_med)
         rm(min_dist_uni_cali)
         rm(min_dist_vias_bog)
         rm(min_dist_vias_med)
         rm(min_dist_vias_cali)
         
         rm(x)
         
         
         
#------------------------------------------------------ Predicción final ---------------------------------------------------------------------------------------------------------------------- 
         'property_typeCasa'->names(test)[names(test)=='property_type']
         test$property_typeCasa<-ifelse(test$property_typeCasa=="Casa",1,0)
         test$property_typeCasa<-as.factor(test$property_typeCasa)
         y_rf_test_final<-predict(modeloRF,newdata=test)
         test$y_rf_test_final<-y_rf_test_final
         sqrt(7.798392e+16)
         mean(exp(y_rf_test_final)/test$surface_final)
         mean(exp(train_bog$price)/train_bog$surface_final)
         
         pl <- ggplot(test, aes(x=exp(y_rf_test_final)))
         p3<-pl + geom_histogram( aes(fill=..count..), col='black')+ labs(title = 'Precio total vivienda en Cali',
                                                                                   
                                                                                   x = 'Precio total casas',
                                                                                   y = 'conteos',
                                                                                   subtitle = 'Distrinución',
                                                                                   caption = 'Resultados Modelo Random Forest')
         
         p2 <- ggplot(x2, aes(x=pricem2))
         p8<-p2 + geom_histogram( aes(fill=..count..), col='black')+ labs(title = 'Precios m2 Cali',
                                                                                   
                                                                                   x = 'Precio por m2 en Cali',
                                                                                   y = 'conteos',
                                                                                   subtitle = 'Distrinución',
                                                                                   caption = 'Resultados Modelo Random Forest')+
           scale_x_continuous(limits = c(0,9000000))
           
         p1<-plot_grid(p3,p8, nrow  = 1, ncol=2, labels="AUTO")
         
         
         x2<-test
         x2<- x2[,-1]
         x2<-as.data.frame(x2)
         x2<- select(x2,-geometry,-property_typeCasa)
         x2$price<- exp(y_rf_test_final)
         x2$bathrooms_final <- as.integer(x2$bathrooms_final)
         x2$surface_final <- as.integer(x2$surface_final)
         
         
        
         corrplot(cor(x2))
                  #Effect of the variables in the dataframe on price.
         
                    x2 %>%
                    select(c(-property_typeCasa)) %>%
                    melt(id.vars = "x2") %>%
                    ggplot(aes(x = value, y = pricem2, colour = variable)) +
                    geom_point(alpha = 0.7) +
                    stat_smooth(aes(colour = "black")) +
                    facet_wrap(~variable, scales = "free", ncol = 2) +
                    labs(x = "Variable Value", y = "Precio m2") +
                    theme_minimal()
                  
                  
                  test$price<-exp(y_rf_test_final)
                  x2$pricem2<-x2$price/x2$surface_final
                  x2$pricem2<-as.integer(x2$pricem2)
                  
                  
                    cero<- ggplot()+
                    geom_sf(data=Cali) +
                    geom_sf(data=x2$geometry,aes(alpha=x2$pricem2))+
                    scale_x_continuous(limits = c(-76.56,-76.45,0.01))+
                    theme_minimal()+
                    labs(x = "Latitud", y = "Longitud") +labs(title = 'Distribución geográfica precios m2- Cali',
                      subtitle = 'Zona urbana',
                      caption = 'Precios predichos por modelo RF')+labs(alpha = "Precio m2")
                      
                    cero1<- ggplot()+
                      geom_sf(data=Cali) +
                      geom_sf(data=x2$geometry,aes(alpha=x2$surface_final), color="cadetblue")+
                      scale_x_continuous(limits = c(-76.56,-76.45,0.01))+
                      theme_minimal()+labs(alpha = "Área total")
                      labs(x = "Latitud", y = "Longitud")
                    
                    cero2<- ggplot()+
                      geom_sf(data=Cali) +
                      geom_sf(data=x2$geometry,aes(alpha=x2$bathrooms_final), color="darkslategray")+
                      scale_x_continuous(limits = c(-76.56,-76.45,0.01))+labs(alpha = "total baños")
                      theme_minimal()+
                      labs(x = "Latitud", y = "Longitud") 
                    
                    cero3<- ggplot()+
                      geom_sf(data=Cali) +
                      geom_sf(data=x2$geometry,aes(alpha=x2$bedrooms, color="deepskyblue3")+
                      scale_x_continuous(limits = c(-76.56,-76.45,0.01))+labs(alpha = "Total cuartos")
                      theme_minimal()+
                      labs(x = "Latitud", y = "Longitud")
                    
                    p4<-plot_grid(cero,cero1, cero2, cero3, nrow  = 1, ncol=4, labels="AUTO")
                    
                    
                  uno<-ggplot()+
                    geom_sf(data=Cali) +
                    geom_sf(data=colegios_cali, color= "red") +
                    labs(title = 'Colegios')+ scale_x_continuous(limits = c(-76.56,-76.45))+
                  theme(plot.margin = margin(t = 0.1, r = 0.1,  b = 0.1,   l = 0.1, unit="cm"), axis.text = element_text(size = 6),
                        plot.title = element_text(size=10))
                  
                  dos<-ggplot()+
                    geom_sf(data=Cali) +
                    labs(title = 'Oficinas (CBD)')+ scale_x_continuous(limits = c(-76.56,-76.45))+
                    geom_sf(data=office_cali, color= "pink") + #theme_bw()+
                   theme(plot.margin = margin(t = 0.1, r = 0.1,  b = 0.1,   l = 0.1, unit="cm"), axis.text = element_text(size = 6),
                         plot.title = element_text(size=10))
                  
                  
                  tres<-ggplot()+
                    geom_sf(data=Cali) +
                    geom_sf(data=industrial_cali, color= "white") +
                    labs(title = 'Industrias')+ scale_x_continuous(limits = c(-76.56,-76.45))+
                    theme(plot.margin = margin(t = 0.1, r = 0.1,  b = 0.1,   l = 0.1, unit="cm"), axis.text = element_text(size = 6),
                          plot.title = element_text(size=10))
                  #+theme_bw() 
                  
                  cuatro<-ggplot()+
                    geom_sf(data=Cali) +
                    geom_sf(data=universidades_cali, color= "blue") +
                    labs(title = 'Universidades')+ scale_x_continuous(limits = c(-76.56,-76.45))+
                    theme(plot.margin = margin(t = 0.1, r = 0.1,  b = 0.1,   l = 0.1, unit="cm"), axis.text = element_text(size = 6),
                          plot.title = element_text(size=10))
                  #+theme_bw() 
                  
                  cinco<-ggplot()+
                    geom_sf(data=Cali) +
                    geom_sf(data=police_cali, color= "black") +
                    labs(title = 'CAI')+ scale_x_continuous(limits = c(-76.56,-76.45))+
                    theme(plot.margin = margin(t = 0.1, r = 0.1,  b = 0.1,   l = 0.1, unit="cm"), axis.text = element_text(size = 6),
                          plot.title = element_text(size=10))
                  #+                  theme_bw() 
                  
                  seis<-ggplot()+
                    geom_sf(data=Cali) +
                    geom_sf(data=bus_cali, color= "brown") +
                    labs(title = 'Estaciones de buses')+scale_x_continuous(limits = c(-76.56,-76.45))+
                    theme(plot.margin = margin(t = 0.1, r = 0.1,  b = 0.1,   l = 0.1, unit="cm"), axis.text = element_text(size = 6),
                          plot.title = element_text(size=10))
                  #+                  theme_bw() 
                  
                  siete<-ggplot()+
                    geom_sf(data=Cali) +
                    geom_sf(data=vias_cali, color= "yellow") +
                    labs(title = 'vías principales')+scale_x_continuous(limits = c(-76.56,-76.45))+
                    theme(plot.margin = margin(t = 0.1, r = 0.1,  b = 0.1,   l = 0.1, unit="cm"), axis.text = element_text(size = 6),
                          plot.title = element_text(size=10))
                  #+ theme_bw() 
                  
                  ocho<-ggplot()+
                    geom_sf(data=Cali) +
                    geom_sf(data=kinder_cali, color= "orange") +
                    labs(title = 'Jardines escolares')+scale_x_continuous(limits = c(-76.56,-76.45))+
                  theme(plot.margin = margin(t = 0.1, r = 0.1,  b = 0.1,   l = 0.1, unit="cm"), axis.text = element_text(size = 6),
                        plot.title = element_text(size=10))
                #+                    theme_bw() 
                  
                  nueve<-ggplot()+
                    geom_sf(data=Cali) +
                    geom_sf(data=retail_cali, color= "purple") +
                    labs(title = 'Centros comerciales')+scale_x_continuous(limits = c(-76.56,-76.45))+
                    theme(plot.margin = margin(t = 0.1, r = 0.1,  b = 0.1,   l = 0.1, unit="cm"), axis.text = element_text(size = 6),
                          plot.title = element_text(size=10))
                  #+                    theme_bw() 
                  
                  p5<-plot_grid(uno, dos, tres, cuatro, cinco,seis,siete,ocho,nueve, nrow  = 3, ncol=3, labels="AUTO")
                  
              
                  plot_grid (cero,p5,
                    alinear = "h" , eje = "b" , nrow = 1,ncol=2)
                  
              x2$property_id<-test$property_id
              x2<- select(x2,property_id, price)
              predictions<-x2
                  
             submission_template$price=predictions$price
            submission_template==predictions_cuellar_mendivelso_riveros.csv
             