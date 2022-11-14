
############## Estadisticas Descriptivas

install.packages("ggplot2")

install.packages("vtable")

library(vtable)
require("sf")
require("ggplot2")
require("dplyr")
library(sf)





##--------------- Estadísticas Descriptivas train_final

## Precio por Tipo de Propiedad (Casa o Apartamento) 

train_final %>%
  group_by(property_type) %>%
  summarise_at(.vars = "price",
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)


## Superficie por Tipo de Propiedad (Casa o Apartamento) 

train_final %>%
  group_by(property_type) %>%
  summarise_at(.vars = "surface_final",
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)

## Número de Habitaciones por Tipo de Propiedad (Casa o Apartamento) 

train_final %>%
  group_by(property_type) %>%
  summarise_at(.vars = "bedrooms",
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)



##### Tabla con el Promedio en la  Distancia de los Amenities, Desviación Estandar, Distancia Mínima y Máxima


st(train_final, vars=c('dist_uni', 'dist_col', 'dist_kin', 'dist_bus', 'dist_vias', 'dist_off','dist_ind', 'dist_ret', 'dist_cai'), 
   labels=c('Distancia a Universidad', 'Distancia a Colegios', 
            'Distancia a Kindergarten', 'Distancia a Estaciones de Bus','Distancia a Via Principal',
            'Distancia a Oficinas ', 'Distancia a Industria','Distancia a Comercio',
            'Distancia a Cai'), summ.names = list(c('Observaciones','Promedio','Desv.Est.','Min','Max')))

### 

train_final %>%
  select(c(-property_type)) %>%
  melt(id.vars = "price") %>%
  ggplot(aes(x = value, y = price, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price ($1000s)") +
  theme_minimal()


############# 

train_final %>%
  select(c(property_type)) %>%
  melt(id.vars = "price") %>%
  ggplot(aes(x = value, y = price, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price ($1000s)") +
  theme_minimal()


################################ Mapas

install.packages("cowplot")
library(cowplot)



Cali_2 <- opq(bbox = 'Cali, Colombia') %>%
  add_osm_feature(key = 'admin_level', value = '8') %>% 
  osmdata_sf %>% unique_osmdata
localidades <- Cali_2$osm_multipolygons

calii2<- as.data.frame(Cali2)
##############################3

ggplot()+
  geom_sf(data=Bogota) +
  geom_sf(data=industrial_bog, color= "white") +
  geom_jitter(width = 0.0005, height = 0.07)+ geom_smooth(span = 0.002) + scale_y_continuous(trans = "log10")+
  geom_sf(data=train_bog$geometry,aes(alpha=train_bog$price))+
  labs (title= "´Precio Vivienda en Relación con los Centros de Comercio en la Ciudad",
        x="Clasificacion Pobreza | 0 = No Pobre    1 = Pobre", 
        y="Ingreso Hogar", 
        caption= " Datos GEIH 2O18") + theme_classic()

color=as.factor(Pobre) 

