
rm(list = ls())

require("sf")

require("ggplot2")
require("dplyr")

library(ggplot2)
library(dplyr)
library(tidyverse)
install.packages("sf")
library(sf)
install.packages("osmdata")

install.packages("stringr")
library(stringr)



#convertir todo el texto en minusculas


train$description <- tolower(train$description)

#quitar tildes

train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")


#elimnar caracter especial

train$description <- str_replace_all(train$description, "[^[:alnum:]]", " ")


#eliminamos espacios extras

train$description <- gsub("\\s+", " ", str_trim(train$description))

typos <- aregexec("garajes", train$description)
regmatches(train$description, typos)

#contamos para ver con cuantos nas contamos

sum(is.na(train$description))

# eliminamos los na

train <- train[!is.na(train$description),]

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
table(train$conjunto)
table(train$ascensor9)

#cambiar la codificación para que queden en 1 y 0

#garaje
train$garaje12[train$garaje12 == 2] <- 1
train$garaje12[train$garaje12 == 3] <- 1
train$garaje12[train$garaje12 == 4] <- 1

#ascensor

train$ascensor9[train$ascensor == 2] <- 1



#eliminar columnas que ya no se necesitan 

train$global = NULL
train$conjunto1 = NULL
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



