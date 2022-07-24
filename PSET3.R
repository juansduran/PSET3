#PSET3#
rm(list = ls())
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
#Ponemos sistema de coordenadas

db<-data.frame(place= train$property_id,
               lat= train$lat,
               long= train$lon
)
db<-db %>% mutate(latp=lat,longp=long)

db<-st_as_sf(db,coords=c('longp','latp'),crs=4626)


train <- train %>% mutate(geometry = db$geometry )



#####


#Dividimos la base para Bogotá y para Medellín"

train_medallo <- subset(train, train$l3=="Medellín")
train_nevera <- subset(train, train$l3=="Bogotá D.C")

#Para bogotá vamos a agregar parques y universidades pensando 
# en que en esta zona vive población universitaria
######
transmi_rolos <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="bus_station")
transmi_rolos_sf <- osmdata_sf(transmi_rolos)
transmi_rolos_geom <- transmi_rolos_sf$osm_points

#Le ponemos el sistema de coordenadas de train
transmis_rolos_new <- st_transform(transmi_rolos_geom, cres=st_crs(train_nevera))

#Repetimos para universidades
universidades_rolas <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="university")
uni_rolas_sf <- osmdata_sf(universidades_rolas)
uni_rolas_geom <- uni_rolas_sf$osm_polygons

#Le ponemos el sistema de coordenadas de train
uni_rolas_new <- st_transform(uni_rolas_geom, cres=st_crs(train_nevera))


##Creamos el mapa en OSM para Bogotá
leaflet() %>% 
  addTiles()%>%
  addPolygons(data=uni_rolas_geom, col = "green") %>%
  addCircleMarkers(data = transmi_rolos_geom, col = "blue") %>%
  addCircles(data=train_nevera, col = "red")

####Distancia promedio a universidades de las casas
# Primero sacamos la distancia a una universidad

st_crs(uni_rolas_new)
st_crs(train_nevera)
dist_uni <- st_distance(x=train_nevera, y=uni_rolas_new)
dist_uni

#ahora sacamos la distancia promedio
prom_dist_uni <- apply(dist_uni, 1, mean)
prom_dist_uni

#Pegamos a la base


####Distancia mínima a una estación de transmilenio






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

###########################
#### Repetimos para la base test

test_medallo <- subset(test, test$l3=="Medellín")
test_nevera <- subset(test, test$l3=="Bogotá D.C")







#########Fin del script