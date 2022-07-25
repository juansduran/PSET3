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
##Sacamos información de el Poblado

base_train_test <- bind_rows(train,test) %>% st_as_sf(coords=c("lon","lat"),crs=4626)

#Definimos sólo el área del poblado
PH_Poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 
#Dejamos con el mismo sistema de coordenadas

st_crs(PH_Poblado)==st_crs(base_train_test$geometry)
PH_Poblado <- st_transform(PH_Poblado, crs=st_crs(base_train_test$geometry))


# Observaciones en el poblado 
Pobladation <- base_train_test[PH_Poblado,]

leaflet() %>%
  addTiles() %>%
  addPolygons(data=PH_Poblado, col = "red") %>%
  addCircles(data=Pobladation)


######Definimos el área de chapinero
chapi_papi <- getbb(place_name = "UPZ Chapinero, Bogota", 
                    featuretype = "boundary:administrative", 
                    format_out = "sf_polygon") %>% .$multipolygon
#Dejamos con el mismo sistema de coordenadas

st_crs(chapi_papi)==st_crs(base_train_test$geometry)
chapi_papi <- st_transform(chapi_papi, crs=st_crs(base_train_test$geometry))

# Observaciones en chapinero
chapineration <- base_train_test[chapi_papi,]


leaflet() %>%
  addTiles() %>% 
  addPolygons(data=chapi_papi,color="red") %>% 
  addCircles(data=chapineration)


#Para bogotá vamos a agregar parques y universidades pensando 
# en que en esta zona vive población universitaria
######
transmi_rolos <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="bus_station")
transmi_rolos_sf <- osmdata_sf(transmi_rolos)
transmi_rolos_geom <- transmi_rolos_sf$osm_points

#Le ponemos el sistema de coordenadas de train
transmi_rolos_new <- st_transform(transmi_rolos_geom, crs=st_crs(train_nevera$geometry))

#Repetimos para universidades
universidades_rolas <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="university")
uni_rolas_sf <- osmdata_sf(universidades_rolas)
uni_rolas_geom <- uni_rolas_sf$osm_polygons

#Le ponemos el sistema de coordenadas de train
uni_rolas_new <- st_transform(uni_rolas_geom, crs=st_crs(train_nevera$geometry))


##Creamos el mapa en OSM para Bogotá
leaflet() %>% 
  addTiles()%>%
  addPolygons(data=uni_rolas_geom, col = "green") %>%
  addCircleMarkers(data = transmi_rolos_geom, col = "blue") %>%
  addCircles(data=train_nevera, col = "red")

####Distancia promedio a universidades de las casas
# Primero sacamos la distancia a una universidad

st_crs(uni_rolas_geom)
st_crs(train_nevera$geometry)
dist_uni <- st_distance(x=train_nevera$geometry, y=uni_rolas_new)
dist_uni

#ahora sacamos la distancia promedio
prom_dist_uni <- apply(dist_uni, 1, mean)
prom_dist_uni

#Pegamos a la base


####Distancia a una estación de transmilenio
dist_tra <- st_distance(x=train_nevera$geometry, y=transmi_rolos_new)
dist_tra

####Distancia mínima a una estación de transmileni
min_dist_tra <- apply(dist_uni, 1, mean)
min_dist_tra 






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