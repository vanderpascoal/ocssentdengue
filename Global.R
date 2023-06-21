load("dados.RData")


m <- shp %>% 
  left_join(
    base[which(base$anomes == 201201),
         c("geocod","tx_1k","anomes")], by=c("cod_geom"="geocod"))

bins <- c(0.002817727, 0.025503407, 0.060084143, 0.151100063,11.292095533 , Inf)
pal <- colorBin("YlOrBr", domain = m$tx_1k, bins = bins)

