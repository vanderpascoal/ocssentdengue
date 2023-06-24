load("dados.RData")
library(sf)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(magrittr)


#### Taxa 100k e Zero p NA
#base$tx_1k<-base$tx_1k*10000
#shp_data$tx_1k<-shp_data$tx_1k*10000


m <- shp %>% 
  left_join(
    base[which(base$anomes == 201201),
         c("geocod","tx_1k","anomes")], by=c("cod_geom"="geocod"))

m$tx_1k[is.na(m$tx_1k)]<-0



bins <- c(0,as.numeric(quantile(base$tx_1k, na.rm = T)))
pal <- leaflet::colorBin("YlOrBr", domain = m$tx_1k, bins = bins)

