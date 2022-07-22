
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



