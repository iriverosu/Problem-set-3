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
              
#-------------------------------------------------- Delimitar las ciudades ----------------------------------------------------------------------------------------------------------------------- 
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
        
 
#----1. Bogotá  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
 11001
#----2. Medellín  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
 05001
#----3. Cali -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
 76001
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
         train_bog<-subset(train2,city=="Cali")
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
         min_dist_vias_bog <- apply(matrix_dist_kin_bog , 1 , min)
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
         
         
         
         
#--------------------------------------------------------- Base final train y test  ---------------------------------------------------------------------------------------------------------------------- 
#-------Base definitiva
         train_bog<-as.data.frame((train_bog))
         train_medellin<-as.data.frame(train_medellin)
         train_final <- rbind(train_bog,train_medellin)
         
         train_final <- select(train_final,-property_id, -city)
         
#------Modificar la base
         train2<-train2 %>% select(-surface_total,-surface_covered,-rooms, -bathrooms, -title, -description, -operation_type, -new_surface)
         
#-------------------------------------------------------------- Modelo ---------------------------------------------------------------------------------------------------------------------- 
#RF
#Pruebas 
#------------------------------------------------------ Estadísticas Descriptívas ---------------------------------------------------------------------------------------------------------------------- 
