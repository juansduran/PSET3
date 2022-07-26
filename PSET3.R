#PSET3#
rm(list = ls())
#usamos la libreria

library("pacman")
p_load(tidyverse,
       sf,
       rio,
       leaflet,
       tmaptools,
       osmdata,
       stringr,
       tidyr,
       caret,
       rgdal
       )


#Cargamos las bases
train <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/PSET3/dataPS3/dataPS3/train.Rds")

test <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/PSET3/dataPS3/dataPS3/test.Rds")

setwd("C:/Users/pcere/Dropbox/Machine Learning/PSET3/dataPS3/MGN2017_05_ANTIOQUIA/05_ANTIOQUIA/URBANO")

man_pa <- st_read("MGN_URB_MANZANA.shp")

setwd("C:/Users/pcere/Dropbox/Machine Learning/PSET3/dataPS3/MGN2017_11_BOGOTA/11_BOGOTA/URBANO")
man_ro <- st_read("MGN_URB_MANZANA.shp")
#############################3333
#Agregamos variables con análisis de textos

#convertir todo el texto en minusculas

train$description <- tolower(train$description)

#quitar tildes

train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")


#elimnar caracter especial

train$description <- str_replace_all(train$description, "[^[:alnum:]]", " ")


#eliminamos espacios extras

train$description <- gsub("\\s+", " ", str_trim(train$description))


### encontrar metros y n?mero de ba?os

#se crean patrones

x = "[:space:]+[:digit:]+m2"
y = "[:space:]+[:digit:]+[:space:]+mt2"
z = "[:space:]+[:digit:]+[:space:]+m2"
aa = "[:space:]+[:digit:]+mt2"
ab = "[:space:]+[:digit:]+[:space:]+metros"
ac = "[:space:]+[:digit:]+metros"
ad = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt2"
ae = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2"
af = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metros"

ah = "[:space:]+[:digit:]+[:space:]+banos"
ai = "[:space:]+[:digit:]+[:space:]+bano"

digit = "[:space:]+[:digit:]+[:space:]"
digit2 = "[:space:]+[:digit:]"
digit3 = "[:space:]+[:digit:]+m2"
digit4 = "[:space:]+[:digit:]+mts"
digit5 = "[:space:]+[:digit:]+[:digit:]+[:digit:]+[:digit:]"
mdos = "m2"
metrostr = "mt2"
metr= "metros"


#metros cuadrados

#se extraen de acuerdo a los patrones y se crea nueva variable
train <- train %>% mutate(metros2 = str_extract(string = train$description, pattern = paste0(x,"|",y,"|",z,"|",aa,"|",ab,"|",ac,"|",ad,"|",ae,"|",af)))

#nos quedamos solo con los valores num?ricos
train <- train %>% mutate(metros3 = str_extract(string = train$metros2, pattern = paste0(digit,"|", digit2,"|", digit3,"|", digit4,"|", digit5)))

#remplazamos todos las palabras que tengan el n?mero y la palabra pegada

train<- train %>% mutate(metros4 = str_replace_all(string = train$metros2, pattern = paste0(mdos, "|", metrostr, "|", metr), replacement = ""))

#lo convertimos a un valor n?m?rico
train$metros3 <- as.numeric(train$metros3)

#nos quedamos con la informaci?n
train <- train %>% mutate(metros_tot = ifelse(is.na(metros3)==T, surface_total, metros4))

train$metros_tot <- as.numeric(train$metros_tot)


#para eliminar todas las filas que contengan na en la columna de metros
train <- train %>% drop_na(metros_tot)

#arreglamos otros datos inconsistentes

train <- train %>% mutate(mts_tot =ifelse(metros_tot<25, 25, metros_tot ))


train <- train %>% mutate(mts_totales2 =ifelse(mts_tot>3000, 3000, mts_tot ))

#eliminamos nuevas variables

train$metros2 = NULL
train$metros3 = NULL
train$metros4 = NULL
train$metros_tot = NULL
train$mts_tot = NULL
train$metrosl2 = NULL

#contar na
sum(is.na(train$metros_tot))


# sacamos m?s ba?os para otros del description

train <- train %>% mutate(banos2 = str_extract(string = train$description, pattern = paste0(ah,"|",ai))) 

train <- train %>% mutate(banos3 = str_extract(string = train$banos2, pattern = digit))

#se convierte en num?rico puesto que se saca de texto
train$banos3 <- as.numeric(train$banos3)
#ba?os totales

#convertir na a 0

train$bathrooms[is.na(train$bathrooms)] <- 0
train$banos3[is.na(train$banos3)] <- 0

train <- train %>% mutate(tot_banos= bathrooms+banos3)

train$tot_banos[train$tot_banos == 0] <- 1
train$tot_banos[train$tot_banos == 20] <- 1
train$tot_banos[train$tot_banos == 22] <- 1
train$tot_banos[train$tot_banos == 25] <- 1
train$tot_banos[train$tot_banos == 26] <- 1
train$tot_banos[train$tot_banos == 36] <- 1
train$tot_banos[train$tot_banos == 47] <- 1
train$tot_banos[train$tot_banos == 60] <- 1
train$tot_banos[train$tot_banos == 100] <- 1
train$tot_banos[train$tot_banos == 2010] <- 1
train$tot_banos[train$tot_banos == 2016] <- 1

table(train$metros_tot)                                                                

                                                                 #paste0(x,"|",y,"|",z,"|",aa,"|",ab,"|",ac,"|",ad,"|",ae,"|",af),0)))

#elimino las variabkes de ba?os que no uso

train$banos2 = NULL
train$banos3 = NULL

#contamos para ver con cuantos nas contamos

sum(is.na(test$rooms))

# eliminamos los na

train <- train[!is.na(train$description),]

#se eliminan los rooms de na

train$bedrooms[train$bedrooms == 0] <- 1

train <- train %>% mutate(rooms_tot = ifelse(is.na(rooms)==T, bedrooms+1, rooms))


#creamos nuevas variables del texto

#conjunto residencial

train <- train %>% mutate(conjunto = str_detect(train$description, "conjunto"))

#ascensor
train <- train %>% mutate(ascensor = str_detect(train$description, "ascensor"))
train <- train %>% mutate(ascensor1 = str_detect(train$description, "acensor"))
train <- train %>% mutate(ascensor2 = str_detect(train$description, "asensor"))

#garaje
train <- train %>% mutate(garaje = str_detect(train$description, "garaje"))
train <- train %>% mutate(garaje1 = str_detect(train$description, "garage"))
train <- train %>% mutate(garaje2 = str_detect(train$description, "garages"))
train <- train %>% mutate(garaje3 = str_detect(train$description, "garajes"))
train <- train %>% mutate(garaje4 = str_detect(train$description, "garje"))
train <- train %>% mutate(garaje5 = str_detect(train$description, "garge"))

#amoblado
train <- train %>% mutate(amoblado = str_detect(train$description, "amoblado"))
train <- train %>% mutate(amoblado1 = str_detect(train$description, "amoblada"))
train <- train %>% mutate(amoblado2 = str_detect(train$description, "amovlada"))                
train <- train %>% mutate(amoblado3 = str_detect(train$description, "amovlado"))

#convertir los true en 1 y 0

train <- train %>%
  mutate(conjunto1 = ifelse(conjunto=="TRUE", 1,0),
         ascensor6 = ifelse(ascensor=="TRUE", 1,0),
         ascensor7 = ifelse(ascensor1=="TRUE", 1,0),
         ascensor8 = ifelse(ascensor2=="TRUE", 1,0),
         garaje6 = ifelse(garaje=="TRUE", 1,0),
         garaje7 = ifelse(garaje1=="TRUE", 1,0),
         garaje8 = ifelse(garaje2=="TRUE", 1,0),
         garaje9 = ifelse(garaje3=="TRUE", 1,0),
         garaje10 = ifelse(garaje4=="TRUE", 1,0),
         garaje11 = ifelse(garaje5=="TRUE", 1,0),
         amoblado4 = ifelse(amoblado=="TRUE", 1,0),
         amoblado5 = ifelse(amoblado1=="TRUE", 1,0),
         amoblado6 = ifelse(amoblado2=="TRUE", 1,0),
         amoblado7 = ifelse(amoblado3=="TRUE", 1,0))

#sumarlos y dejar listas las variables

train <- train %>% mutate(ascensor9 = ascensor6 + ascensor7 + ascensor8)
train <- train %>% mutate(garaje12 = garaje6 + garaje7 + garaje8 + garaje9 + garaje10 + garaje11)
train <- train %>% mutate(amoblado8 = amoblado4 + amoblado5 + amoblado6 + amoblado7 )


table(train$amoblado8)
table(train$garaje12)
table(train$conjunto1)
table(train$ascensor9)

#cambiar la codificación para que queden en 1 y 0

#garaje
train$garaje12[train$garaje12 == 2] <- 1
train$garaje12[train$garaje12 == 3] <- 1
train$garaje12[train$garaje12 == 4] <- 1

#ascensor

train$ascensor9[train$ascensor == 2] <- 1


#eliminar columnas que ya no se necesitan 


train$conjunto = NULL
train$ascensor = NULL
train$ascensor1 = NULL
train$ascensor2 = NULL
train$ascensor3 = NULL
train$ascensor6 = NULL
train$ascensor7= NULL
train$ascensor8 = NULL
train$garaje = NULL
train$garaje1 = NULL
train$garaje2 = NULL
train$garaje3 = NULL
train$garaje4 = NULL
train$garaje5 = NULL
train$garaje6 = NULL
train$garaje7 = NULL
train$garaje8 = NULL
train$garaje9 = NULL
train$garaje10 = NULL
train$garaje11 = NULL
train$amoblado = NULL
train$amoblado1 = NULL
train$amoblado2 = NULL
train$amoblado3 = NULL
train$amoblado4 = NULL
train$amoblado5 = NULL
train$amoblado6 = NULL
train$amoblado7 = NULL


#interacciones

train <- train %>% mutate(metros_4 = mts_totales2^2)

train <- train %>% mutate(apartamento = ifelse(property_type =="Apartamento", 1, 0))

train <- train %>% mutate(apartamento_ascensor = apartamento*ascensor9)




######################################################
# test

####################


#convertir todo el texto en minusculas


test$description <- tolower(test$description)

#quitar tildes

test$description <- iconv(test$description, from = "UTF-8", to = "ASCII//TRANSLIT")


#elimnar caracter especial

test$description <- str_replace_all(test$description, "[^[:alnum:]]", " ")


#eliminamos espacios extras

test$description <- gsub("\\s+", " ", str_trim(test$description))

typos <- aregexec("garajes", test$description)
regmatches(test$description, typos)

#################


#metros cuadrados

#se extraen de acuerdo a los patrones y se crea nueva variable
test <- test %>% mutate(metros2 = str_extract(string = test$description, pattern = paste0(x,"|",y,"|",z,"|",aa,"|",ab,"|",ac,"|",ad,"|",ae,"|",af)))

#nos quedamos solo con los valores num?ricos
test <- test %>% mutate(metros3 = str_extract(string = test$metros2, pattern = paste0(digit,"|", digit2,"|", digit3,"|", digit4,"|", digit5)))

#remplazamos todos las palabras que tengan el n?mero y la palabra pegada

test<- test %>% mutate(metros4 = str_replace_all(string = test$metros2, pattern = paste0(mdos, "|", metrostr, "|", metr), replacement = ""))

#lo convertimos a un valor n?m?rico
test$metros3 <- as.numeric(test$metros3)

#nos quedamos con la informaci?n
test <- test %>% mutate(metros_tot = ifelse(is.na(metros3)==T, surface_total, metros4))

test$metros_tot <- as.numeric(test$metros_tot)



#para eliminar todas las filas que contengan na en la columna de metros
test <- test %>% drop_na(metros_tot)

#arreglamos otros datos inconsistentes

test <- test %>% mutate(mts_tot =ifelse(metros_tot<25, 25, metros_tot ))


test <- test %>% mutate(mts_totales2 =ifelse(mts_tot>3000, 3000, mts_tot ))

#eliminamos nuevas variables

test$metros2 = NULL
test$metros3 = NULL
test$metros4 = NULL
test$metros_tot = NULL
test$mts_tot = NULL


#contar na
sum(is.na(test$metros_tot))


# sacamos m?s ba?os para otros del description

test <- test %>% mutate(banos2 = str_extract(string = test$description, pattern = paste0(ah,"|",ai))) 

test <- test %>% mutate(banos3 = str_extract(string = test$banos2, pattern = digit))

#se convierte en num?rico puesto que se saca de texto
test$banos3 <- as.numeric(test$banos3)
#ba?os totales

#convertir na a 0

test$bathrooms[is.na(test$bathrooms)] <- 0
test$banos3[is.na(test$banos3)] <- 0

test <- test %>% mutate(tot_banos= bathrooms+banos3)



test$tot_banos[test$tot_banos == 0] <- 1

table(test$tot_banos)                                                                


#elimino las variabkes de ba?os que no uso

test$banos2 = NULL
test$banos3 = NULL


#se eliminan los rooms de na

test$bedrooms[test$bedrooms == 0] <- 1

test <- test %>% mutate(rooms_tot = ifelse(is.na(rooms)==T, bedrooms+1, rooms))

########################################


#contamos para ver con cuantos nas contamos

sum(is.na(test$description))

# eliminamos los na

test <- test[!is.na(test$description),]

#creamos nuevas variables del texto

#conjunto residencial

test <- test %>% mutate(conjunto = str_detect(test$description, "conjunto"))

#ascensor
test <- test %>% mutate(ascensor = str_detect(test$description, "ascensor"))
test <- test %>% mutate(ascensor1 = str_detect(test$description, "acensor"))
test <- test %>% mutate(ascensor2 = str_detect(test$description, "asensor"))

#garaje
test <- test %>% mutate(garaje = str_detect(test$description, "garaje"))
test <- test %>% mutate(garaje1 = str_detect(test$description, "garage"))
test <- test %>% mutate(garaje2 = str_detect(test$description, "garages"))
test <- test %>% mutate(garaje3 = str_detect(test$description, "garajes"))
test <- test %>% mutate(garaje4 = str_detect(test$description, "garje"))
test <- test %>% mutate(garaje5 = str_detect(test$description, "garge"))

#amoblado
test <- test %>% mutate(amoblado = str_detect(test$description, "amoblado"))
test <- test %>% mutate(amoblado1 = str_detect(test$description, "amoblada"))
test <- test %>% mutate(amoblado2 = str_detect(test$description, "amovlada"))                
test <- test %>% mutate(amoblado3 = str_detect(test$description, "amovlado"))

#convertir los true en 1 y 0

test <- test %>%
  mutate(conjunto1 = ifelse(conjunto=="TRUE", 1,0),
         ascensor6 = ifelse(ascensor=="TRUE", 1,0),
         ascensor7 = ifelse(ascensor1=="TRUE", 1,0),
         ascensor8 = ifelse(ascensor2=="TRUE", 1,0),
         garaje6 = ifelse(garaje=="TRUE", 1,0),
         garaje7 = ifelse(garaje1=="TRUE", 1,0),
         garaje8 = ifelse(garaje2=="TRUE", 1,0),
         garaje9 = ifelse(garaje3=="TRUE", 1,0),
         garaje10 = ifelse(garaje4=="TRUE", 1,0),
         garaje11 = ifelse(garaje5=="TRUE", 1,0),
         amoblado4 = ifelse(amoblado=="TRUE", 1,0),
         amoblado5 = ifelse(amoblado1=="TRUE", 1,0),
         amoblado6 = ifelse(amoblado2=="TRUE", 1,0),
         amoblado7 = ifelse(amoblado3=="TRUE", 1,0))

#sumarlos y dejar listas las variables

test <- test %>% mutate(ascensor9 = ascensor6 + ascensor7 + ascensor8)
test <- test %>% mutate(garaje12 = garaje6 + garaje7 + garaje8 + garaje9 + garaje10 + garaje11)
test <- test %>% mutate(amoblado8 = amoblado4 + amoblado5 + amoblado6 + amoblado7 )


table(test$amoblado8)
table(test$garaje12)
table(test$conjunto)
table(test$ascensor9)

#cambiar la codificación para que queden en 1 y 0

#garaje
test$garaje12[test$garaje12 == 2] <- 1
test$garaje12[test$garaje12 == 3] <- 1
test$garaje12[test$garaje12 == 4] <- 1

#ascensor

test$ascensor9[test$ascensor == 2] <- 1

#eliminar columnas que ya no se necesitan 


test$conjunto = NULL
test$ascensor = NULL
test$ascensor1 = NULL
test$ascensor2 = NULL
test$ascensor3 = NULL
test$ascensor6 = NULL
test$ascensor7= NULL
test$ascensor8 = NULL
test$garaje = NULL
test$garaje1 = NULL
test$garaje2 = NULL
test$garaje3 = NULL
test$garaje4 = NULL
test$garaje5 = NULL
test$garaje6 = NULL
test$garaje7 = NULL
test$garaje8 = NULL
test$garaje9 = NULL
test$garaje10 = NULL
test$garaje11 = NULL
test$amoblado = NULL
test$amoblado1 = NULL
test$amoblado2 = NULL
test$amoblado3 = NULL
test$amoblado4 = NULL
test$amoblado5 = NULL
test$amoblado6 = NULL
test$amoblado7 = NULL

#interacciones

test <- test %>% mutate(metros_4 = mts_totales2^2)

test <- test %>% mutate(apartamento = ifelse(property_type =="Apartamento", 1, 0))

test <- test %>% mutate(apartamento_ascensor = apartamento*ascensor9)
rm(test_nevera)

#partimos la base para Bogota y Medellin

train_medallo <- subset(train, train$l3=="Medell�n")
train_nevera <- subset(train, train$l3=="Bogot� D.C")
test_medallo <- subset(test, test$l3=="Medell�n")
test_nevera <- subset(test, test$l3=="Bogot� D.C")



###Preparamos la base de train
##ponemos sistema de coordenadas para train Bogota

train_nevera_1<-data.frame(place= train_nevera$property_id,
               lat= train_nevera$lat,
               long= train_nevera$lon
               )
train_nevera_1<-train_nevera_1 %>% mutate(latp=lat,longp=long)

train_nevera_2<-st_as_sf(train_nevera_1,coords=c('longp','latp'),crs=4626)

train_nevera$geometry <- train_nevera_2$geometry

##ponemos sistema de coordenadas para train Medellin
train_medallo_1<-data.frame(place= train_medallo$property_id,
                         lat= train_medallo$lat,
                         long= train_medallo$lon
)
train_medallo_1<-train_medallo_1 %>% mutate(latp=lat,longp=long)

train_medallo_2<-st_as_sf(train_medallo_1,coords=c('longp','latp'),crs=4626)

train_medallo$geometry <- train_medallo_2$geometry


rm(train_medallo_1, train_medallo_2, train_nevera_1, train_nevera_2)

#####
##Sacamos información de el Poblado

#Definimos sólo el área del poblado
PH_Poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 
#Dejamos con el mismo sistema de coordenadas

st_crs(PH_Poblado)==st_crs(train_medallo$geometry)
PH_Poblado <- st_transform(PH_Poblado, crs=4626)
st_crs(PH_Poblado)==st_crs(train_medallo$geometry)

train_medallo_sf <- st_as_sf(train_medallo)

# Observaciones en el poblado 
Pobladation <- train_medallo_sf[PH_Poblado,]

leaflet() %>%
  addTiles() %>%
  addPolygons(data=PH_Poblado, col = "red") %>%
  addCircles(data=Pobladation)


######Definimos el área de chapinero
chapi_papi <- getbb(place_name = "UPZ Chapinero Bogota", 
                    featuretype = "boundary:administrative", 
                    format_out = "sf_polygon") %>% .$multipolygon
#Dejamos con el mismo sistema de coordenadas

st_crs(chapi_papi)==st_crs(train_nevera$geometry)
chapi_papi <- st_transform(chapi_papi, crs=st_crs(train_nevera$geometry))
st_crs(chapi_papi)==st_crs(train_nevera$geometry)

  # Observaciones en chapinero
train_nevera_sf <- st_as_sf(train_nevera)
chapineration <- train_nevera_sf[chapi_papi,]


leaflet() %>%
  addTiles() %>% 
  addCircles(data=chapineration)%>%
addPolygons(data=chapi_papi,color="red") 


####Variables adicionales de OSM##########

#Para bogotá vamos a agregar estaciones de transmilenio y universidades pensando 
# en que en esta zona vive población universitaria
######
transmi_rolos <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="bus_station")
transmi_rolos_sf <- osmdata_sf(transmi_rolos)
transmi_rolos_geom <- transmi_rolos_sf$osm_points

#Le ponemos el sistema de coordenadas de train
transmi_rolos_new <- st_transform(transmi_rolos_geom, crs=st_crs(train_nevera$geometry))
st_crs(transmi_rolos_new)==st_crs(train_nevera$geometry)

#Repetimos para universidades
universidades_rolas <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="university")
uni_rolas_sf <- osmdata_sf(universidades_rolas)
uni_rolas_geom <- uni_rolas_sf$osm_polygons

#Le ponemos el sistema de coordenadas de train
uni_rolas_new <- st_transform(uni_rolas_geom, crs=st_crs(train_nevera$geometry))
st_crs(uni_rolas_new)==st_crs(train_nevera$geometry)

##Creamos el mapa en OSM para Bogotá

leaflet() %>% 
  addTiles()%>%
  addPolygons(data=chapi_papi,color="red") %>% 
  addCircleMarkers(data = transmi_rolos_new, col = "blue")

####Distancia promedio a universidades de las casas
# Primero sacamos la distancia a una universidad

st_crs(uni_rolas_geom)==st_crs(train_nevera$geometry)
uni_rolas_new <- st_transform(uni_rolas_geom, crs=st_crs(train_nevera$geometry))
dist_uni <- st_distance(x=train_nevera$geometry, y=uni_rolas_new)
dist_uni


#ahora sacamos la distancia promedio
prom_dist_uni <- apply(dist_uni, 1, mean)


#Pegamos a la base


####Distancia a una estación de transmilenio
dist_tra <- st_distance(x=train_nevera$geometry, y=transmi_rolos_new)
dist_tra

####Distancia mínima a una estación de transmileni
min_dist_tra <- apply(dist_tra, 1, min)
min_dist_tra 


#################
#Agregamos las distancias a la base
train_nevera <- train_nevera %>%
  mutate(Transmi = min_dist_tra)

train_nevera <- train_nevera %>%
  mutate(Universidad = prom_dist_uni)

##El poblado en medellín es una zona rosa turística, por esta razón buscaremos cercanía a restaurantes
# y parques
restaurantes_paisas <- opq(bbox=getbb("Comuna 14 - El Poblado")) %>%
  add_osm_feature(key="amenity", value ="restaurant")
restaurantes_paisas_sf <- osmdata_sf(restaurantes_paisas)
restaurantes_paisas_geom <- restaurantes_paisas_sf$osm_points

parques_paisas <- opq(bbox=getbb("Comuna 14 - El Poblado")) %>%
  add_osm_feature(key="leisure", value ="park")
par_paisas_sf <- osmdata_sf(parques_paisas)
par_paisas_geom <- par_paisas_sf$osm_polygons


##Creamos el mapa en OSM para Medellín
leaflet() %>% 
  addTiles()%>%
  addPolygons(data=PH_Poblado, col = "yellow") %>%
  addPolygons(data=par_paisas_geom, col = "green") %>%
  addCircleMarkers(data = restaurantes_paisas_geom, col = "blue") %>%
  


####Distancia promedio a un restaurante
# Primero sacamos la distancia a un restaurante
restaurantes_paisas_geom <- st_transform(restaurantes_paisas_geom, crs=st_crs(train_medallo$geometry))
st_crs(restaurantes_paisas_geom)==st_crs(train_medallo$geometry)
dist_rest <- st_distance(x=train_medallo$geometry, y=restaurantes_paisas_geom)
dist_rest

#ahora sacamos la distancia promedio
prom_dist_rest <- apply(dist_rest, 1, mean)
prom_dist_rest




####Distancia a un parque
par_paisas_geom <- st_transform(par_paisas_geom, crs=st_crs(train_medallo$geometry))
st_crs(par_paisas_geom)==st_crs(train_medallo$geometry)
dist_park <- st_distance(x=train_medallo$geometry, y=par_paisas_geom)
dist_park

####Distancia mínima a un parque
min_dist_park <- apply(dist_park, 1, min)
min_dist_park

#Agregamos las distancias a la base
train_medallo <- train_medallo %>%
  mutate(Restaurante = prom_dist_rest)

train_medallo <- train_medallo %>%
  mutate(Parque = min_dist_park)


#################################################################
#Repetimos para test
###Preparamos la base de test
##ponemos sistema de coordenadas para test Bogota

test_nevera_1<-data.frame(place= test_nevera$property_id,
                          lat= test_nevera$lat,
                          long= test_nevera$lon
)
test_nevera_1<-test_nevera_1 %>% mutate(latp=lat,longp=long)

test_nevera_2<-st_as_sf(test_nevera_1,coords=c('longp','latp'),crs=4626)

test_nevera$geometry <- test_nevera_2$geometry

##ponemos sistema de coordenadas para test Medellin
test_medallo_1<-data.frame(place= test_medallo$property_id,
                           lat= test_medallo$lat,
                           long= test_medallo$lon
)
test_medallo_1<-test_medallo_1 %>% mutate(latp=lat,longp=long)

test_medallo_2<-st_as_sf(test_medallo_1,coords=c('longp','latp'),crs=4626)

test_medallo$geometry <- test_medallo_2$geometry


rm(test_medallo_1, test_medallo_2, test_nevera_1, test_nevera_2)

#####
##Sacamos información de el Poblado


leaflet() %>%
  addTiles() %>%
  addPolygons(data=PH_Poblado, col = "red") %>%
  addCircles(data=test_medallo)
#addCircles(data=Pobladation)

# Observaciones en chapinero

#chapineration <- test_nevera[chapi_papi,]


leaflet() %>%
  addTiles() %>% 
  addPolygons(data=chapi_papi,color="red") %>%
  addCircles(data=test_nevera)


####Variables adicionales de OSM##########

#Para bogotá vamos a agregar estaciones de transmilenio y universidades pensando 
# en que en esta zona vive población universitaria
######
transmi_rolos <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="bus_station")
transmi_rolos_sf <- osmdata_sf(transmi_rolos)
transmi_rolos_geom <- transmi_rolos_sf$osm_points

#Le ponemos el sistema de coordenadas de test
transmi_rolos_new <- st_transform(transmi_rolos_geom, crs=st_crs(test_nevera$geometry))
st_crs(transmi_rolos_new)==st_crs(test_nevera$geometry)

#Repetimos para universidades
universidades_rolas <- opq(bbox=getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity", value ="university")
uni_rolas_sf <- osmdata_sf(universidades_rolas)
uni_rolas_geom <- uni_rolas_sf$osm_polygons

#Le ponemos el sistema de coordenadas de test
uni_rolas_new <- st_transform(uni_rolas_geom, crs=st_crs(test_nevera$geometry))
st_crs(uni_rolas_new)==st_crs(test_nevera$geometry)

##Creamos el mapa en OSM para Bogotá

leaflet() %>% 
  addTiles()%>%
  addPolygons(data=uni_rolas_geom, col = "green") %>%
  addPolygons(data=chapi_papi,color="yellow") %>% 
  addCircleMarkers(data = transmi_rolos_new, col = "blue")

####Distancia promedio a universidades de las casas
# Primero sacamos la distancia a una universidad

st_crs(uni_rolas_geom)==st_crs(test_nevera$geometry)
uni_rolas_new <- st_transform(uni_rolas_geom, crs=st_crs(test_nevera$geometry))
dist_uni <- st_distance(x=test_nevera$geometry, y=uni_rolas_new)
dist_uni


#ahora sacamos la distancia promedio
prom_dist_uni <- apply(dist_uni, 1, mean)


#Pegamos a la base


####Distancia a una estación de transmilenio
dist_tra <- st_distance(x=test_nevera$geometry, y=transmi_rolos_new)
dist_tra

####Distancia mínima a una estación de transmileni
min_dist_tra <- apply(dist_tra, 1, min)
min_dist_tra 


#################
#Agregamos las distancias a la base
test_nevera <- test_nevera %>%
  mutate(Transmi = min_dist_tra)

test_nevera <- test_nevera %>%
  mutate(Universidad = prom_dist_uni)

##El poblado en medellín es una zona rosa turística, por esta razón buscaremos cercanía a restaurantes
# y parques
restaurantes_paisas <- opq(bbox=getbb("Comuna 14 - El Poblado")) %>%
  add_osm_feature(key="amenity", value ="restaurant")
restaurantes_paisas_sf <- osmdata_sf(restaurantes_paisas)
restaurantes_paisas_geom <- restaurantes_paisas_sf$osm_points

parques_paisas <- opq(bbox=getbb("Comuna 14 - El Poblado")) %>%
  add_osm_feature(key="leisure", value ="park")
par_paisas_sf <- osmdata_sf(parques_paisas)
par_paisas_geom <- par_paisas_sf$osm_polygons


##Creamos el mapa en OSM para Medellín
leaflet() %>% 
  addTiles()%>%
  addPolygons(data=par_paisas_geom, col = "green") %>%
  addCircleMarkers(data = restaurantes_paisas_geom, col = "blue") %>%
  addCircles(data=test_medallo, col = "red")


####Distancia promedio a un restaurante
# Primero sacamos la distancia a un restaurante
restaurantes_paisas_geom <- st_transform(restaurantes_paisas_geom, crs=st_crs(test_medallo$geometry))
st_crs(restaurantes_paisas_geom)==st_crs(test_medallo$geometry)
dist_rest <- st_distance(x=test_medallo$geometry, y=restaurantes_paisas_geom)
dist_rest

#ahora sacamos la distancia promedio
prom_dist_rest <- apply(dist_rest, 1, mean)
prom_dist_rest




####Distancia a un parque
par_paisas_geom <- st_transform(par_paisas_geom, crs=st_crs(test_medallo$geometry))
st_crs(par_paisas_geom)==st_crs(test_medallo$geometry)
dist_park <- st_distance(x=test_medallo$geometry, y=par_paisas_geom)
dist_park

####Distancia mínima a un parque
min_dist_park <- apply(dist_park, 1, min)
min_dist_park

#Agregamos las distancias a la base
test_medallo <- test_medallo %>%
  mutate(Restaurante = prom_dist_rest)

test_medallo <- test_medallo %>%
  mutate(Parque = min_dist_park)


#################################################################

#estadisticas descriptivas


install.packages("arsenal")
library("arsenal")
install.packages("vtable")
library(vtable)

train_nevera2 <- train_nevera
train_medallo2 <- train_medallo

#nevera

train_nevera2$geometry = NULL
train_nevera2$lat = NULL
train_nevera2$lon = NULL
train_nevera2$l1 = NULL
train_nevera2$l2 = NULL
train_nevera2$l3 = NULL
train_nevera2$start_date = NULL
train_nevera2$end_date = NULL
train_nevera2$currency = NULL
train_nevera2$title = NULL
train_nevera2$description = NULL
train_nevera2$operation_type = NULL


#medallo

train_medallo2$geometry = NULL
train_medallo2$lat = NULL
train_medallo2$lon = NULL
train_medallo2$l1 = NULL
train_medallo2$l2 = NULL
train_medallo2$l3 = NULL
train_medallo2$start_date = NULL
train_medallo2$end_date = NULL
train_medallo2$currency = NULL
train_medallo2$title = NULL
train_medallo2$description = NULL
train_medallo2$operation_type = NULL



#Getting complex
st(train_nevera2, col.breaks = 20,
   summ = list(
     c('notNA(x)','mean(x)','sd(x^2)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Mean','SD of X^2','Min','Max'),
     c('Count','Percent')
   ))


#Getting complex
st(train_medallo2, col.breaks = 20,
   summ = list(
     c('notNA(x)','mean(x)','sd(x^2)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Mean','SD of X^2','Min','Max'),
     c('Count','Percent')
   ))



<<<<<<< HEAD
=======
summary(table_one, title = "Datos de Bogot?")

library(gtsummary)
library(haven)
library(tidyr)

train %>%
  select(train$rooms_tot, train$bedrooms) %>%
  tbl_summary(by=train$property_type) %>%
  add_overall() %>%
  add_n()


>>>>>>> a1532b49a02135f34c9e87011ffdf7bee2e8f3a7

##Modelos de predicción. Se correrán para Medellin y Bogota para
#encontrar el mejor modelo para cada ciudad

#Usamos paquede caret para entrenar el modelo
library("caret")

FiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl <- trainControl(method = "cv",
                     number = 10,
                     savePredictions = TRUE,
                     summaryFunction = FiveStats)

#Creamos matriz ponderada

library("sf")
library("spdep")
library("dplyr")

#Bogota
train_nevera_sf <- st_as_sf(train_nevera)
train_nevera_sf <- st_transform(train_nevera_sf, crs=4626)
man_ro <- st_transform(man_ro, crs=4626)
train_nevera_mnz <- st_join(x=train_nevera_sf,y=man_ro)

Reina_B<-poly2nb(train_nevera_mnz, queen=TRUE)
W_R_B<-nb2listw(Reina_B, style="W", zero.policy=TRUE)

Torre_B<-poly2nb(train_nevera_mnz, queen=False)
W_T_B<-nb2listw(Torre_B, style="W", zero.policy=TRUE)

#1 Regresion con todos los predictores con pre procesamiento

set.seed(1712)
Reg_1_bog <- train(
  price~ metros_4 + apartamento_ascensor + property_type + tot_banos + mts_totales2 + rooms_tot + bedrooms + conjunto1 + ascensor9 +garaje12 +amoblado8 + Universidad + Transmi, data=train_nevera,
  method = "lm",
  preProcess = c("center", "scale")  
)
Reg_1_bog


#2 Regresion con sin interacciones los parametros con pre procesamiento

set.seed(1712)
Reg_2_bog <- train(
  price~  metros_4 + apartamento_ascensor + property_type + tot_banos + mts_totales2 + rooms_tot + bedrooms + conjunto1 + ascensor9 +garaje12 +amoblado8, data=train_nevera,
  method = "lm",
  preProcess = c("center", "scale")
)
Reg_2_bog

#3 Regresion con correlacion espacial Reina
reg_3_bog<-lagsarlm(price~ property_type + tot_banos + mts_totales2 + rooms_tot + bedrooms  +amoblado8 + Universidad + Transmi, data=train_nevera_mnz, W_R_B)

#4 Regresion con correlacion espacial Torre
reg_3_bog<-lagsarlm(price~ property_type + tot_banos + mts_totales2 + rooms_tot + bedrooms  +amoblado8 + Universidad + Transmi, data=train_nevera_mnz, W_T_B)




###Medellin

train_medallo_sf <- st_as_sf(train_medallo)
train_medallo_sf <- st_transform(train_medallo_sf, crs=4626)
man_pa <- st_transform(man_pa, crs=4626)
train_medallo_mnz <- st_join(x=train_medallo_sf,y=man_pa)

Reina_pa<-poly2nb(train_medallo_mnz, queen=TRUE)
W_R_M<-nb2listw(Reina_pa, style="W", zero.policy=TRUE)

Torre_pa<-poly2nb(train_medallo_mnz, queen=False)
W_T_M<-nb2listw(Torre_pa, style="W", zero.policy=TRUE)

#1 Regresion con todos los predictores con pre procesamiento

set.seed(1712)
Reg_1_med <- train(
  price~ metros_4 + apartamento_ascensor + property_type + tot_banos + mts_totales2 + rooms_tot + bedrooms + conjunto1 + ascensor9 +garaje12 +amoblado8 + Parque + Restaurante, data=train_medallo,
  method = "lm",
  preProcess = c("center", "scale")  
)
Reg_1_med


#2 Regresion con sin interacciones los parametros con pre procesamiento

set.seed(1712)
Reg_2_med <- train(
  price~  metros_4 + apartamento_ascensor + property_type + tot_banos + mts_totales2 + rooms_tot + bedrooms + conjunto1 + ascensor9 +garaje12 +amoblado8, data=train_medallo,
  method = "lm",
  preProcess = c("center", "scale")
)
Reg_2_med

#3 Regresion con correlacion espacial Reina
reg_3_bog<-lagsarlm(price~ property_type + tot_banos + mts_totales2 + rooms_tot + bedrooms  +amoblado8 + Parque + Restaurante, data=train_paisa_mnz, W_R_B)

#4 Regresion con correlacion espacial Torre
reg_3_bog<-lagsarlm(price~ property_type + tot_banos + mts_totales2 + rooms_tot + bedrooms  +amoblado8 + Parque + Restaurante, data=train_paisa_mnz, W_T_B)




#########
#Corremos la regresión en la base test

#Tabla
install.packages("huxtable")
library(huxtable)

tablila <- huxreg(Reg_1_bog)

summary(Reg_1_bog)

summary(Reg_1_med)


#Extraemos csv



#########Fin del script




