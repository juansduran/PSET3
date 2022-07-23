#PSET3#

#usamos la libreria
library("pacman")

p_load(tidyverse,
       sf,
       rio,
       leaflet,
       tmaptools,
       osmdata)

#Cargamos las bases
train <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/PSET3/dataPS3/dataPS3/train.Rds")
test <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/PSET3/dataPS3/dataPS3/test.Rds")

#Dividimos la base para Bogotá y para Medellín"

train_medallo <- subset(train, train$l3=="Medellín")
train_nevera <- subset(train, train$l3=="Bogotá D.C")

#Para bogotá vamos a agregar parques y universidades pensando 
# en que en esta zona vive población universitaria
######
transmis_rolos <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="bus_station")
transmi_rolos_sf <- osmdata_sf(transmi_rolos)
transmi_rolos_geom <- transmi_rolos_sf$osm_points

universidades_rolas <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="university")
uni_rolas_sf <- osmdata_sf(universidades_rolas)
uni_rolas_geom <- uni_rolas_sf$osm_polygons



##Creamos el mapa en OSM para Bogotá
leaflet() %>% 
  addTiles()%>%
  addPolygons(data=uni_rolas_geom, col = "green") %>%
  addCircleMarkers(data = transmi_rolos_geom, col = "blue") %>%
  addCircles(data=train_nevera, col = "red")


##El poblado en medellín es una zona rosa turística, por esta razón buscaremos cercanía a restaurantes
# y parques
restaurantes_paisas <- opq(bbox=getbb("Medellin Colombia")) %>%
  add_osm_feature(key="amenity", value ="restaurant")
restaurantes_paisas_sf <- osmdata_sf(restaurantes_paisas)
restaurantes_paisas_geom <- restaurantes_paisas_sf$osm_points

parques_paisas <- opq(bbox=getbb("Medellin Colombia")) %>%
  add_osm_feature(key="leisure", value ="park")
par_paisas_sf <- osmdata_sf(parques_paisas)
par_paisas_geom <- par_paisas_sf$osm_polygons


##Creamos el mapa en OSM para Medellín
leaflet() %>% 
  addTiles()%>%
  addPolygons(data=par_paisas_geom, col = "green") %>%
  addCircleMarkers(data = restaurantes_paisas_geom, col = "blue") %>%
  addCircles(data=train_medallo, col = "red")






















#INTENTO 1
require("sf")

require("ggplot2")
require("dplyr")

library(ggplot2)
library(dplyr)
library(tidyverse)
install.packages("sf")
library(sf)
install.packages("osmdata")

db<-data.frame(place= train$property_id,
               lat= train$lat,
               long= train$lon
)
db<-db %>% mutate(latp=lat,longp=long)

db<-st_as_sf(db,coords=c('longp','latp'),crs=4626)


train_geometry <- train 

train_geometry <- train_geometry %>% mutate(geometry = db$geometry )

ggplot()+
  geom_sf(data=train_geometry,aes(geometry=geometry)) +
theme_bw()  + theme(axis.title =element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text = element_text(size=6))



